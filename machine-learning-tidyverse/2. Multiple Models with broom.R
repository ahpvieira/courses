#' In this chapter you'll learn how to use the coefficients of these models to gain new
#' insights into the gapminder data.

# Load packages -------------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(broom)


# Creating nested data and models -------------------------------------------------------------

gap_nested <- gapminder %>% 
      drop_na %>% 
      group_by(country) %>% 
      nest()

gap_models <- gap_nested %>% # group_by(gapminder, country) %>% nest() -- Make a list column
      mutate(model = map(data, ~lm(formula = life_expectancy~year, data = .x))) # Work with lists

# Tidy up the coefficients of your models -----------------------------------------------------

# Extract the coefficient statistics of each model into nested dataframes and simplify the coef dataframes
model_coef <- gap_models %>% 
      mutate(coef = map(model, ~tidy(.x))) %>% 
      unnest(coef)

#' Now let's explore these values to see what you can learn from this data.

# Plot a histogram of the coefficient estimates for year         
model_coef %>% 
      filter(term == "year") %>% 
      ggplot(aes(x = estimate)) +
      geom_histogram()

# Which country had the fastest growth in life expectancy

model_coef %>% 
      filter(term == "year") %>% 
      filter(estimate == max(estimate))

# How many countries had descreases in life expectancy

model_coef %>% 
      filter(term == "year") %>% 
      count(count = estimate < 0) %>% 
      mutate(prop = n/sum(n))

# Evaluating the fit of many models -----------------------------------------------------------

# Extract the fit statistics of each model into simplified dataframes

model_perf <- gap_models %>% 
      mutate(coef = map(model, ~glance(.x))) %>% 
      unnest(coef)

# Find the best fitting model
 
model_perf %>% 
      top_n(n = 2, wt = r.squared)

# Find the worst fitting model

model_perf %>% 
      top_n(n = 2, wt = -r.squared)

# Plot a histogram of rsquared for the 185 models    

model_perf %>% 
      ggplot(aes(x = r.squared)) + 
      geom_histogram()

# Visually inspect the fit of many models -----------------------------------------------------

# Building augmented dataframes

augmented_models <- gap_models %>% 
      mutate(agumented = map(model, ~augment(.x))) %>% 
      unnest(agumented)

# Model for a country with a high r squared

augmented_models %>% 
      filter(country == "Italy") %>% 
      ggplot(aes(year, life_expectancy)) +
      geom_point() +
      geom_line(aes(y = .fitted), color = "red")

# Model for a country with a medium r squared

augmented_models %>% 
      filter(country == "Fiji") %>% 
      ggplot(aes(year, life_expectancy)) +
      geom_point() +
      geom_line(aes(y = .fitted), color = "red")

# Model for a country with a low r squared

augmented_models %>% 
      filter(country == "Kenya") %>% 
      ggplot(aes(year, life_expectancy)) +
      geom_point() +
      geom_line(aes(y = .fitted), color = "red")

# Compare the predicted values with the actual values of life expectancy 
# for the top 4 best fitting models

best_augmented %>% 
      ggplot(aes(x = year)) +
      geom_point(aes(y = life_expectancy)) + 
      geom_line(aes(y = .fitted), color = "red") +
      facet_wrap(~country, scales = "free_y")

# Compare the predicted values with the actual values of life expectancy 
# for the top 4 worst fitting models
worst_augmented %>% 
      ggplot(aes(x = year)) +
      geom_point(aes(y = life_expectancy)) + 
      geom_line(aes(y = .fitted), color = "red") +
      facet_wrap(~country, scales = "free_y")

# Improve the fit of your models --------------------------------------------------------------

# Simple linear model

gap_models <- gap_nested %>%
      mutate(model = map(data, ~lm(formula = life_expectancy ~ year, data = .x)))

# Multiple linear model

gap_fullmodels <- gap_nested %>%
      mutate(model = map(data, ~lm(formula = life_expectancy ~ year + infant_mortality +
                                         fertility + population, data = .x)))

tidy(gap_fullmodels$model[[1]])
augment(gap_fullmodels$model[[1]])
glance(gap_fullmodels$model[[1]]) # use adjusted r squared

fullmodel_perf <- gap_fullmodels %>% 
      # Extract the fit statistics of each model into dataframes
      mutate(fit = map(model, ~glance(.x))) %>% 
      # Simplify the fit dataframes for each model
      unnest(fit)

# View the performance for the four countries with the worst fitting 
# four simple models you looked at before

fullmodel_perf %>% 
      top_n(n = 4, wt = -r.squared) %>% 
      # filter(country %in% worst_fit$country) %>% 
      select(country, adj.r.squared)

#' In the upcomming chapter you will learn how to estimate model performance using data withheld 
#' from building the model.

