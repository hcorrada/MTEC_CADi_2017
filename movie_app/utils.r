library(tidyverse)
library(ggplot2)
library(rvest)
library(broom)
library(stringr)

clean_budget_tab <- read_csv("movie_budgets.csv")

scrape_table <- function(actor, base_url="https://www.rottentomatoes.com/celebrity/") {
  # first let's make the name work with RT
  rt_name <- actor %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  
  message("Scraping Rotten Tomatoes with name ", rt_name)
  dirty_dat <- scrape_rt(rt_name, base_url=base_url) 
  
  message("Preparing data for analysis")
  dirty_dat %>%
    cleanup_rt_tab() %>%
    join_budget() 
}

plot_movies <- function(data, actor) {
  plt <- data %>% ggplot() +
    aes(x=rating, y=domestic_gross, color=cluster) +
    geom_point(size=2.3) +
    theme_bw() +
    labs(title=paste0(actor, "'s movies"),
         x="Rotten Tomatoes rating",
         y="Domestic Gross (Millions)")
  
  annot_dat <- data %>%
    group_by(cluster) %>%
    arrange(center_dist) %>%
    slice(1)
  
  plt <- plt +
    annotate("text", 
             x=annot_dat$x1,
             y=annot_dat$x2,
             label=annot_dat$title)
  plt
}

cluster_movies <- function(data, k=3) {
  data <- data %>%
    select(rating=RATING, title=TITLE, domestic_gross)
  
  message("Clustering movies with k=", k)
  kmeans_result <-  data %>%
    select(rating, domestic_gross) %>%
    kmeans(centers=k) 
  
  clustered_tab <- kmeans_result %>%
    augment(data=data) %>%
    rename(cluster=.cluster) %>%
    as_tibble()
  
  kmeans_centers <- kmeans_result %>%
    tidy() %>%
    as_tibble()
  
  clustered_tab %>%
    left_join(select(kmeans_centers, x1, x2, cluster)) %>%
    
    # calculate the distance of each movie to its center
    mutate(center_dist=sqrt((rating-x1)^2+(domestic_gross-x2)^2))
}

join_budget <- function(data) {
  data %>%
    # join the two tables together
    inner_join(clean_budget_tab, by=c(TITLE="movie")) 
}

cleanup_rt_tab <- function(data) {
  data  %>% 
    # make sure the movie is rated
    filter(RATING != "No Score Yet") %>% 
    
    # make the rating look numeric
    mutate(RATING = str_replace(RATING, "%", "")) %>%
    
    # remove producer and director credits
    filter(!str_detect(CREDIT, "Prod") &
             !str_detect(CREDIT, "Dir")) %>%
    
    # convert to proper types
    readr::type_convert()
}

scrape_rt <- function(actor, base_url="https://www.rottentomatoes.com/celebrity/") {
  url <- paste0(base_url, actor)
  html <- read_html(url) 
  
  html %>%
    html_nodes("#filmographyTbl") %>%
    html_table() %>%
    magrittr::extract2(1) %>%
    as_tibble()
}

