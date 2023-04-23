library(glue)
library(rvest)

url_home <- "https://www.rosebikes.de/fahrräder"
html_home <- read_html(url_home)

bike_fams <- html_home %>%
  html_nodes(css = ".catalog-navigation__link")

links <- sapply(bike_fams, function(x) {x %>% html_attr("href")})

links <- links[1:9] %>%
  enframe(name = "position", value = "subdirectory") %>%
  mutate(
    url = glue("https://www.rosebikes.de{subdirectory}"))  %>%
  distinct(url)

html_bike_cat <- read_html(links$url[1])

listings <- html_nodes(html_bike_cat, css = '.catalog-category-bikes__price-title') %>% 
  html_text(trim=TRUE) %>%              
  str_replace_all(" ","") %>%
  str_replace_all("ab", "") %>%
  str_replace_all("€", "") %>%
  str_replace_all("\n", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>%
  iconv('utf-8', 'ascii', sub='') %>%
  as.numeric()

names <- html_nodes(html_bike_cat, xpath = '//basic-headline/h4') %>% 
  html_text() %>%
  str_replace_all("\n", "") %>%
  str_to_title()

cats <- rep(links$url[1] %>% str_replace_all("https://www.rosebikes.de/fahrräder/", ""), 
            each=length(names)) %>%
  str_to_title()

bike_data <- tibble(bike.type = cats,
                    bike.name = names,
                    bike.price = as.numeric(listings))

for (i in 2:9) {
  html_bike_cat <- read_html(links$url[i])
  
  listings <- html_nodes(html_bike_cat, css = '.catalog-category-bikes__price-title') %>% 
    html_text(trim=TRUE) %>%              
    str_replace_all(" ","") %>%
    str_replace_all("ab", "") %>%
    str_replace_all("€", "") %>%
    str_replace_all("\n", "") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", "\\.") %>%
    iconv('utf-8', 'ascii', sub='') %>%
    as.numeric()
  
  names <- html_nodes(html_bike_cat, xpath = '//basic-headline/h4') %>% 
    html_text() %>%
    str_replace_all("\n", "") %>%
    str_to_title()
  
  cats <- rep(links$url[i] %>% str_replace_all("https://www.rosebikes.de/fahrräder/", ""), 
              each=length(names)) %>%
    str_to_title()
  
  bike_data <- bike_data %>% add_row(bike.type = cats,
                                     bike.name = names,
                                     bike.price = as.numeric(listings))
}

bike_data
