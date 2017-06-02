library(gapminder)
library(ggplot2)
library(broom)
library(tidyverse)

data(gapminder)

# Exercise 1
gapminder %>%
  ggplot(aes(x=year, y=lifeExp)) +
  geom_point() +
  labs(title="Life expectancy over time",
       x="Year",
       y="Life Expectancy")

# Exercise 1
gapminder %>%
  ggplot(aes(x=factor(year), y=lifeExp)) +
  geom_violin() +
  labs(title="Life expectancy over time",
       x="Year",
       y="Life Expectancy")

# Exercise 2
gapminder_fit <- lm(lifeExp~year, data=gapminder)
gapminder_fit %>% 
  tidy() %>%
  View()

gapminder_augmented <- gapminder_fit %>%
  augment(gapminder)

gapminder_augmented %>%
  ggplot(aes(x=.fitted, y=.resid)) +
    geom_point() +
    geom_smooth()

# Exercise 3
gapminder_augmented %>%
  ggplot(aes(x=factor(year), y=.resid)) +
  geom_violin() +
  labs(title="Residuals over time",
       x="Year",
       y="residual")

# Exercise 3
gapminder_augmented %>%
  ggplot(aes(x=factor(year), 
             y=.resid)) +
  facet_wrap(~continent) +
  geom_violin() +
  labs(title="Residuals over time",
       x="Year",
       y="residual")

# 
gapminder %>%
  ggplot(aes(x=year, 
             y=lifeExp, 
             color=continent)) +
  geom_point() +
  geom_smooth(method=lm)

gapminder_fit_2 <- lm(lifeExp~year*continent,
                      data=gapminder)
gapminder_fit_2 %>%
  tidy() %>%
  View()

# Exercise 3
gapminder_fit_2 %>%
  augment(gapminder) %>%
  ggplot(aes(x=factor(year), 
             y=.resid)) +
  geom_violin() +
  labs(title="Residuals over time",
       x="Year",
       y="residual")

gapminder_fit_2 %>%
  augment(gapminder) %>%
  ggplot(aes(x=.fitted, 
             y=.resid)) +
  geom_point() +
  geom_smooth() +
  labs(x="Predicted life Expectancy",
       y="Residual")

# model selection using AIC
gapminder_fit %>% glance() %>% select(AIC)
gapminder_fit_2 %>% glance() %>% select(AIC)

# anova of interaction model
gapminder_fit_2 %>% anova() %>% tidy()

