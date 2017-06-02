library(tidyverse)
library(rvest)
library(stringr)


# bajar tabla de Diego Luna
url <- "https://www.rottentomatoes.com/celebrity/diego_luna"
dl_tab <- url %>%
  read_html() %>%
  html_node("#filmographyTbl") %>%
  html_table()

# limpiar tabla de Diego Luna
clean_dl_tab <- dl_tab %>%
  # quitar pelis sin rating
  filter(RATING != "No Score Yet") %>%
  
  # solo usar pelis donde actua
  filter(!str_detect(CREDIT, "Producer")) %>%
  filter(!str_detect(CREDIT, "Director")) %>%
  
  # hacer rating numerico
  mutate(RATING = str_replace(RATING, "%", "")) %>%
  mutate(RATING = as.numeric(RATING))
  

url <- "http://www.the-numbers.com/movie/budgets/all"
budget_tab <- url %>%
  read_html() %>%
  html_node("table") %>%
  html_table(fill=TRUE)

clean_budget_tab <- budget_tab %>%
  select(-1) %>%
  filter(!is.na(Movie)) %>%
  
  # hacer Prod. Budget numerico y
  # usar millones de dolares
  mutate(`Production Budget`= 
         str_replace_all(`Production Budget`, ",", "")) %>%
  mutate(`Production Budget` = 
         str_replace_all(`Production Budget`, "\\$", "")) %>%
  mutate(`Production Budget`= 
           as.numeric(`Production Budget`) / 1000000) %>%
  
  # limpiar Domestic Gross
  mutate(`Domestic Gross`= 
         str_replace_all(`Domestic Gross`, ",", "")) %>%
  mutate(`Domestic Gross` = 
         str_replace_all(`Domestic Gross`, "\\$", "")) %>%
  mutate(`Domestic Gross`= 
           as.numeric(`Domestic Gross`) / 1000000) %>%
  
  # limpiar Worldwide Gross
  mutate(`Worldwide Gross`= 
         str_replace_all(`Worldwide Gross`, ",", "")) %>%
  mutate(`Worldwide Gross` = 
         str_replace_all(`Worldwide Gross`, "\\$", "")) %>%
  mutate(`Worldwide Gross`= 
           as.numeric(`Worldwide Gross`) / 1000000)

write_csv(clean_budget_tab, "clean_budget.csv")

# Unir las tablas
analysis_tab <- clean_dl_tab %>%
  inner_join(clean_budget_tab, 
             by=c("TITLE" = "Movie")) %>%
  select(rating=RATING,
         title=TITLE,
         year=YEAR,
         domestic_gross=`Domestic Gross`)

library(ggplot2)

dl_plot <- analysis_tab %>%
  ggplot(aes(x=rating,
             y=domestic_gross,
             color=year)) +
  geom_point() 

dl_plot +
  labs(title="Diego Luna's Movies",
       x="Rating",
       y="Domestic Gross (millions)") +
  theme_minimal()
  




