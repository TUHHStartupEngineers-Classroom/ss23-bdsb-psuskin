library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(glue)

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Challenge 1

worldwide_cases <- covid_data_tbl %>%
  select(continent, date, total_cases) %>%
  drop_na(continent) %>%
  group_by(date, continent) %>%
  summarise(Total = sum(total_cases, na.rm = TRUE))

data_max <- worldwide_cases %>% 
  group_by(continent) %>%
  filter(Total == max(Total))

last_date <- as.character(tail(worldwide_cases, n=1)$date)

worldwide_cases %>% ggplot(aes(as.Date(date), Total, color = continent)) +
  geom_line(linewidth = 1) +
  theme_linedraw() +
  scale_x_date(date_labels = "%B %y", 
               date_breaks = "1 month", 
               expand = c(0,NA)) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = glue("As of {last_date}")
  ) +
  xlab("Date") + ylab("Cumulative Cases") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1))

# Challenge 2

world <- map_data("world")

worldwide_mortality <- covid_data_tbl %>%
  select(location, date, total_deaths, population) %>%
  drop_na(location) %>%
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location )) %>% 
  distinct() %>%
  group_by(location) %>% 
  filter(date == max(date)) %>%
  mutate(death_rate = total_deaths / population) %>%
  rename(region = location) %>%
  left_join(world, by = "region")

worldwide_mortality %>% ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = death_rate), color = "white") +
  labs(
    title = "Mortality Rate Around the World (deaths / population)",
    subtitle = "Around 6.9 Million confirmed COVID-19 deaths worldwide",
    fill = "Mortality Rate"
  ) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  scale_fill_gradientn(na.value="gray",
                       colors = rev(colorRampPalette(c("darkred", "lightcoral"))(5)))
