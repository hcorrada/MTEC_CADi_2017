library(tidyverse)
library(rvest)
library(stringr)
library(ggplot2)
library(class)

# load budget table from csv file
clean_budget_tab <- read_csv("clean_budget.csv")

# bajar tabla del actor
scrape_table <- function(actor_name) {
  url <- paste0("https://www.rottentomatoes.com/celebrity/",
              actor_name)

  tab <- url %>%
    read_html() %>%
    html_node("#filmographyTbl") %>%
    html_table()
  tab
}

# limpiar tabla del actor
clean_table <- function(tab) {
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

# Unir las tablas
join_tables <- function(clean_tab) {
  analysis_tab <- clean_tab %>%
    inner_join(clean_budget_tab, 
               by=c("TITLE" = "Movie")) %>%
    select(rating=RATING,
           title=TITLE,
           year=YEAR,
           domestic_gross=`Domestic Gross`)
  analysis_tab
}

# clusters
make_clusters <- function(tab, k=3) {
  kmeans_res <- tab %>%
    select(rating,domestic_gross) %>%
    kmeans(centers=k)
  
  kmeans_res
}

# plot
make_plot <- function(analysis_tab, actor_name) {
  analysis_tab %>%
    ggplot(aes(x=rating,
               y=domestic_gross,
               color=cluster)) +
    geom_point(size=2) +
    labs(title=paste0(actor_name, "'s Movies"),
         x="Rating",
         y="Domestic Gross (millions)") +
    theme_minimal()
}

make_regression <- function(tab) {
  lm(domestic_gross~rating, data=tab) %>% 
    tidy()
}

get_table <- function(long_name) {
  # crea nombre para rotten tomatoes
  short_name <- long_name %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  
  scrape_table(short_name) %>%
    clean_table() %>%
    join_tables()
}

# compute squared euclidean distance between
# movie and its cluster center
get_distance_to_center <- function(data) {
  data %>%
    mutate(distance_to_center = 
             (rating - center_rating)^2 +
             (domestic_gross - center_domestic_gross)^2)
}

# find the title of the movie closest to
# each cluster center
get_annotation <- function(cluster_centers, movie_tab) {
  movie_tab %>%
    inner_join(cluster_centers, by="cluster") %>%
    get_distance_to_center() %>%
    group_by(cluster) %>%
    arrange(distance_to_center) %>%
    slice(1) %>%
    ungroup() %>%
    select(title, center_rating, center_domestic_gross)
}

annotate_plot <- function(actor_plot, annotation_data) {
  actor_plot +
    annotate("text", 
             x=annotation_data$center_rating,
             y=annotation_data$center_domestic_gross,
             label=annotation_data$title)
}
analyze_actor <- function(long_name, k=3) {
  # scrape and clean data
  analysis_tab <- get_table(long_name)
  
  # do the clustering
  cluster_res <- analysis_tab %>% make_clusters(k=k)
  
  # augment data with cluster assignment
  augmented_tab <- cluster_res %>% 
    augment(analysis_tab) %>%
    rename(cluster=.cluster)
    
  # make the plot, including cluster info
  actor_plot <- augmented_tab %>% make_plot(long_name)
    
  # get annotation data for the plot
  annotation_tab <- cluster_res %>%
    tidy() %>%
    select(center_rating=x1, center_domestic_gross=x2, cluster) %>%
    get_annotation(augmented_tab)
  
  # add annotation to the plot
  actor_plot %>% annotate_plot(annotation_tab)
}

regress_actor <- function(long_name) {
  get_table(long_name) %>%
    make_regression()
}

