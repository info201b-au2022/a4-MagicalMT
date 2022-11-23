library(tidyverse)
library(usmap)
library(usdata)

# The functions might be useful for A4
#source("assignments/a4-MagicalMT/source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
data <- read.csv("C:/Users/1/Desktop/homework/INFO/Documents/info201/data/incarceration_trends.csv")
# get the ratio of highest black proportion in 2018
highest_black_proportion_2018 <- function(){
  highest_2018 <- filter(data, year == 2018) %>%
    select(state, black_jail_pop, total_jail_pop) %>%
    group_by(state) %>%
    summarise(black_pop = sum(black_jail_pop, na.rm = TRUE),
      total_pop = sum(total_jail_pop, na.rm = TRUE)
    ) %>%
    mutate(black_proportion = black_pop / total_pop * 100) %>%
    filter(black_proportion == max(black_proportion, na.rm = TRUE)) %>%
    pull(black_proportion)

return(highest_2018)
}

# get the ratio of lowest black proportion in 2018
lowest_black_proportion_2018 <- function(){
  lowest_2018 <- filter(data, year == 2018) %>%
    select(state, black_jail_pop, total_jail_pop) %>%
    group_by(state) %>%
    summarise(black_pop = sum(black_jail_pop, na.rm = TRUE),
      total_pop = sum(total_jail_pop, na.rm = TRUE)
    ) %>%
    mutate(black_proportion = black_pop / total_pop * 100) %>%
    filter(black_proportion == min(black_proportion, na.rm = TRUE)) %>%
    pull(black_proportion)

return(lowest_2018)
}

# get the mean number of black proportion in 2018
mean_black_proportion_2018 <- function(){
  mean_2018 <- filter(data, year == 2018) %>%
    select(state, black_jail_pop, total_jail_pop) %>%
    group_by(state) %>%
    summarise(black_pop = sum(black_jail_pop, na.rm = TRUE),
      total_pop = sum(total_jail_pop, na.rm = TRUE)
    ) %>%
    mutate(black_proportion = black_pop / total_pop * 100)
  
  return(mean(mean_2018$black_proportion, na.rm = TRUE))
}
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function get the data of total jail population by year
get_year_jail_pop <- function() {
  # TODO: Implement this function 
  total <- select(data, year, total_jail_pop) %>%
    group_by(year) %>%
    summarise(Total_jail_population = sum(total_jail_pop, na.rm = TRUE))
return(total)   
}

# This function get the chart of total jail population by year
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function
  plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(
      mapping = aes(x = year, y = Total_jail_population)
    ) +
    labs(
      x = "Year", 
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970)-2018"
    )
  return(plot)
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# This function get the data of jail populatiob by states in year
get_jail_pop_by_states <- function(states){
  state <- select(data, year, state, total_jail_pop) %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(Total_jail_population = sum(total_jail_pop, na.rm = TRUE),
      .groups = "drop")
  return(state)
}

# This function get the chart of jail populatiob by states in year
plot_jail_pop_by_states <- function(states){
  plot <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(
      mapping = aes(x = year, y = Total_jail_population, color = state)
    ) +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Jail Population in states of U.S. (1970)-2018"
    )
  return(plot)
}
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

#This function gets the data of different racial proportion in 2018
get_racial_by_states_2018 <- function(){
  racial <- filter(data, year == 2018) %>%
    select(state, total_jail_pop, black_jail_pop,
      white_jail_pop, latinx_jail_pop) %>%
    group_by(state) %>%
    summarise(black_pop = sum(black_jail_pop, na.rm = TRUE),
      white_pop = sum(white_jail_pop, na.rm = TRUE),
      latinx_pop = sum(latinx_jail_pop, na.rm = TRUE),
      total_pop = sum(total_jail_pop, na.rm = TRUE)
    ) %>%
    mutate(black_proportion = black_pop / total_pop * 100,
      white_proportion = white_pop / total_pop * 100,
      latinx_proportion = latinx_pop / total_pop * 100
    )
  return(racial)
}

#This function gets the chart of black people proportion in 2018
plot_black_by_states_2018 <- function(){
  plot <- ggplot(data = get_racial_by_states_2018()) +
    geom_col(mapping = aes(x = state, y = black_proportion),
    fill = "#FF9999",
    colour = "black"
    ) +
    labs(
      x = "State",
      y = "Black Proportion",
      title = "Black people as a percentage of the overall prison population in 2018"
    )
  return(plot)
}
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# This function gets the data of 2018 male proportion in each states
get_2018_male_by_states <- function(){
  male <- filter(data, year == 2018) %>%
    select(state, male_jail_pop, total_jail_pop) %>%
    group_by(state) %>%
    summarise(male_pop = sum(male_jail_pop, na.rm = TRUE),
      total_pop = sum(total_jail_pop, na.rm = TRUE)
    ) %>%
    mutate(male_proportion = male_pop / total_pop * 100) %>%
    select(state, male_proportion) %>%
    mutate(state = tolower(abbr2state(state)))
  return(male)
}

# This function helps to get mapping data for drawing maps
get_mapping_data <- function(){
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(get_2018_male_by_states(), by="state")
  return(state_shape)
}

# This functions gets the maps for male proportion in each states in 2018
plot_2018_male_by_states <- function(){
  plot <- ggplot(get_mapping_data()) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = male_proportion),
      color = "white"
    ) +
  coord_map() +
  scale_fill_continuous(low = "white", high = "purple") +
  labs(fill = "Male proportion",
    title = "Male proportion in jail in each state 2018"
    )

  return(plot)
}
## Load data frame ---- 


