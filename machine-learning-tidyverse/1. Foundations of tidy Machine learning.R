library(tidyverse)
library(dslabs)
library(broom)

## List Column Workflow:
#' 1. Make a list column (nest)
#' 2. Work with list columns (map)
#' 3. Simplify the list columns (unnest)

## The Gapminder Dataset: 77 obs for 52 years per country. 6 features


## Nesting by country

gap_nested <- gapminder %>% 
      group_by(country) %>% 
      nest() # creates a serie of nested data set for each country

# Viewing a Nested Tibble

gap_nested$data[[2]]
gap_nested$data[[4]]

mean(gap_nested$data[[2]]$population, na.rm = T)

## Simplify List Columns - unnest()

gap_unnested <- gap_nested %>% 
      unnest()

# Confirm that your data was not modified  
identical(gapminder, gap_unnested)


# The map family of functions -----------------------------------------------------------------

gap_nested %>% # group_by(gapminder, country) %>% nest() -- Make a list column
      mutate(pop_mean = map(data, ~mean(.x$population, na.rm = T))) %>% # Work with lists
      unnest(pop_mean) # Simplify the list columns

gap_nested %>% # group_by(gapminder, country) %>% nest() -- Make a list column
      mutate(pop_mean = map_dbl(data, ~mean(.x$population, na.rm = T))) # Work with lists

# Build models with map()

gap_models <- gap_nested %>% # group_by(gapminder, country) %>% nest() -- Make a list column
      mutate(model = map(data, ~lm(formula = life_expectancy~year, data = .x))) # Work with lists

# Extract the model for Algeria    
algeria_model <- gap_models$model[[2]]

# View the summary for the Algeria model
summary(algeria_model)

# Tidy your models with broom -----------------------------------------------------------------

#' Now that you know how to work with list columns in a tidy manner you can begin to work
#' with the tools you need to explore and evaluate machine learning models.
#' 
#' The bulk of the work of machine learning resides in step two of the list column workflow.
#' 
#' We are going to work with broom package, which was designed to convert useful model outputs
#' into tidy dataframes.
#' 
#' The core of broom is encapsulated by three functions: tidy, glance and augment.

tidy(algeria_model) # coefficients and test statistics
glance(algeria_model) # one row summary of a model
augment(algeria_model) # model-specifics statistics of fit for each obs

# Plotting augmented data

augment(algeria_model) %>% 
      ggplot(aes(x = year)) +
      geom_point(aes(y = life_expectancy)) +
      geom_line(aes(y = .fitted), color = "red")









