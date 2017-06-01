library(rvest)
url <- "https://www.rottentomatoes.com/celebrity/diego_luna"
dl_tab <- url %>%
  read_html() %>%
  html_node("#filmographyTbl") %>%
  html_table()

library(stringr)
dl_tab <- dl_tab %>%
  filter(RATING != "No Score Yet") %>%
  filter(!str_detect(CREDIT, "Producer")) %>%
  filter(!str_detect(CREDIT, "Director")) %>%
  mutate(RATING = str_replace(RATING, "%", "")) %>%
  mutate(RATING = as.numeric(RATING)) %>%
  select(-`BOX OFFICE`, -CREDIT) 


url <- "http://www.the-numbers.com/movie/budgets/all"
budget_tab <- url %>%
  read_html() %>%
  html_node("table") %>%
  html_table(fill=TRUE)

budget_tab %>%
  select(-1) %>%
  filter(!is.na(Movie)) %>%
  View()

