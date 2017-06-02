library(tidyverse)
library(rvest)
library(stringr)
library(ggplot2)

clean_budget_tab <- read_csv("clean_budget.csv")

scrape_table <- function(actor_name) {
  # bajar tabla del actor
  url <- paste0("https://www.rottentomatoes.com/celebrity/",
              actor_name)

  tab <- url %>%
    read_html() %>%
    html_node("#filmographyTbl") %>%
    html_table()
  tab
}

clean_table <- function(tab) {
  # limpiar tabla del actor
  clean_tab <- tab %>%
    # quitar pelis sin rating
    filter(RATING != "No Score Yet") %>%
    
    # solo usar pelis donde actua
    filter(!str_detect(CREDIT, "Producer")) %>%
    filter(!str_detect(CREDIT, "Director")) %>%
    
    # hacer rating numerico
    mutate(RATING = str_replace(RATING, "%", "")) %>%
    mutate(RATING = as.numeric(RATING))
  clean_tab
}

join_tables <- function(clean_tab) {
  # Unir las tablas
  analysis_tab <- clean_tab %>%
    inner_join(clean_budget_tab, 
               by=c("TITLE" = "Movie")) %>%
    select(rating=RATING,
           title=TITLE,
           year=YEAR,
           domestic_gross=`Domestic Gross`)
  analysis_tab
}

# plot
make_plot <- function(analysis_tab, actor_name) {
  analysis_tab %>%
    ggplot(aes(x=rating,
               y=domestic_gross,
               color=year)) +
    geom_point() +
    labs(title=paste0(actor_name, "'s Movies"),
         x="Rating",
         y="Domestic Gross (millions)") +
    theme_minimal()
}

analyze_actor <- function(long_name) {
  # crea nombre para rotten tomatoes
  short_name <- long_name %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  
  scrape_table(short_name) %>%
    clean_table() %>%
    join_tables() %>%
    make_plot(long_name)
}

