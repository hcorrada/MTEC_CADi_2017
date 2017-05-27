library(tidyverse)
library(rvest)
library(stringr)

# Scrape budget data
budget_url <- "http://www.the-numbers.com/movie/budgets/all"
budget_html <- read_html(budget_url)
budget_tab <- budget_html %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  magrittr::extract2(1) %>%
  select(-1) %>%
  as_tibble()

# clean up the result
clean_budget_tab <- budget_tab %>%
  # remove all those NA rows
  filter(!is.na(`Release Date`)) %>%
  
  # make the budget columns look numeric 
  mutate_at(vars(-1), funs(str_replace(., "\\$", ""))) %>%
  mutate_at(vars(-1), funs(str_replace_all(., ",", ""))) %>%
  
  # rename columns
  rename(release_date=`Release Date`,
         movie=Movie,
         production_budget=`Production Budget`,
         domestic_gross=`Domestic Gross`,
         worldwide_gross=`Worldwide Gross`) %>%
  
  # convert columns to proper types
  type_convert(cols(release_date=col_date(format="%m/%d/%Y"))) %>%
  
  # represent budget and gross in millions
  mutate_at(vars(-1,-2), funs(. / 1e6))

# write budget tab to file
write_csv(clean_budget_tab, path="movie_budgets.csv")

