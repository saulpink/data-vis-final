## READING IN AND CLEANING DATA ----

## load packages ----
library(tidyverse)
library(here)
library(sf)
library(skimr)
library(maps)

## read in data ----
death_causes <- read_csv(here("data/leading_causes_of_death.csv")) |> 
  janitor::clean_names()

## skim ----
skim_without_charts(death_causes)

## test if I can make a map ---
us_states_map <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Convert state names to match your dataset
us_states_map$ID <- tolower(us_states_map$ID)

death_causes$state <- tolower(death_causes$state)

death_causes_map <- us_states_map %>%
  left_join(death_causes, by = c("ID" = "state"))

death_causes_map |> 
  filter(cause_name == "Diabetes", year == 2017) |> 
  ggplot() +
  geom_sf(aes(fill = age_adjusted_death_rate), color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Diabetes age adjusted death rate by state",
       fill = "Death Rate") +
  theme_void()

