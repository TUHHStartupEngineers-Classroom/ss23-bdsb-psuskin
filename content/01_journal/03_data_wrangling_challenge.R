library(vroom)
library(data.table)

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tb1 <- vroom(
  file       = 'Patent_data_reduced/patent.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

assignee_tb1 <- vroom(
  file       = 'Patent_data_reduced/assignee.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_assignee_tb1 <- vroom(
  file       = 'Patent_data_reduced/patent_assignee.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

uspc_tb1 <- vroom(
  file       = 'Patent_data_reduced/uspc.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")) %>%
  transform(patent_id = as.character(patent_id))

wrangled <- assignee_tb1 %>%
  left_join(patent_assignee_tb1, by = c("id" = "assignee_id")) %>%
  left_join(patent_tb1, by = c("patent_id" = "id")) %>%
  left_join(uspc_tb1, by = "patent_id")

# Part 1

patent_leaders <- sort(table(wrangled$organization), decreasing=T)[1:10] %>%
  as.data.frame() %>%
  mutate(Var1 = Var1 %>% str_to_title())

data.table(
  "Organization (US)" = patent_leaders$Var1,
  "No. of patents (2014)" = patent_leaders$Freq)

# Part 2

wrangled_august <- wrangled %>% 
  select(organization, date) %>%
  filter(date >= "2014-08-01" & date <= "2014-08-31")

patent_leaders_august <- sort(table(wrangled_august$organization), decreasing=T)[1:10] %>%
  as.data.frame() %>%
  mutate(Var1 = Var1 %>% str_to_title())

data.table(
  "Organization (US)" = patent_leaders_august$Var1,
  "No. of patents (August 2014)" = patent_leaders_august$Freq)

# Part 3

wrangled_class <- wrangled %>%
  select(organization, mainclass_id) %>%
  filter(organization %in% patent_leaders$Var1[1:10]) %>% 
  subset(mainclass_id != "No longer published")

class_leaders <- sort(table(wrangled_class$mainclass_id), decreasing=T)[1:5] %>% 
  as.data.frame() %>%
  mutate(Var1 = Var1 %>% str_to_title())

data.table(
  "USPTO tech main class (id since can't find name) for top 10 companies (US since can't find worldwide)" = class_leaders$Var1,
  "No. of patents (2014)" = class_leaders$Freq)
