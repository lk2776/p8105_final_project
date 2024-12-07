#load packages
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggridges)
library(patchwork)
library(plotly)
library(purrr)
library(broom)
library(readxl)
library(ggplot2)
library(sf)
library(usmap)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(leaflet)

#read milk cows data
milk_cows = readxl::read_excel("./data/milkcowsandprod.xlsx", 
                               sheet="Milk cows", 
                               skip=1,
                               col_names = TRUE) |>
  janitor::clean_names() |>
  #drop_na() |>
  rename(region = x1) |>
  rename(state = back_to_content_page) |>
  filter_all(any_vars(!is.na(.))) |>
  rename_with(~ gsub("^x", "", .), starts_with("x")) |>
  mutate(region = case_when(
    state %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", 
                 "Connecticut", "New York", "New Jersey", "Pennsylvania", 
                 "Delaware", "Maryland") ~ "Northeast",
    state %in% c("Michigan","Wisconsin","Minnesota") ~ "Lake States",
    state %in% c("Ohio","Indiana","Illinois","Iowa","Missouri") ~ "Corn Belt",
    state %in% c("North Dakota", "South Dakota", "Nebraska", "Kansas") ~ "Northern Plains",
    state %in% c("Virginia", "West Virginia", "North Carolina", "Kentucky", "Tennessee") ~ "Appalachia",
    state %in% c("South Carolina", "Georgia", "Florida", "Alabama") ~ "Southeast",
    state %in% c("Mississippi", "Arkansas","Louisiana") ~ "Delta States",
    state %in% c("Oklahoma","Texas") ~ "Southern Plains",
    state %in% c("Montana","Idaho","Wyoming","Colorado","New Mexico","Arizona","Utah","Nevada") ~ "Mountain Region",
    state %in% c("Washington","Oregon","California") ~ "West Coast",
    state %in% c("Alaska","Hawaii") ~ "Other States"
    #TRUE ~ "Other"  # Default case for states not in the Northeast
  )) |>
  filter(!is.na(state)) |>
  filter(region != "Other States") |>
  mutate(
    ##remove non numeric character and convert to numeric
    `2019` = gsub("[^0-9,.-]", "", `2019`) |> as.numeric(), 
    `2020` = gsub("[^0-9,.-]", "", `2020`) |> as.numeric(),
    `2021` = gsub("[^0-9,.-]", "", `2021`) |> as.numeric(),
    `2022` = gsub("[^0-9,.-]", "", `2022`) |> as.numeric(),
    `2023` = gsub("[^0-9,.-]", "", `2023`) |> as.numeric()
  ) |>
  pivot_longer(
    cols = 3:56,
    names_to = "year",
    values_to = "number_of_cows"
  )  |>
  drop_na()


milk_production = readxl::read_excel("./data/milkcowsandprod.xlsx", sheet="Milk production", skip=1,col_names = TRUE) |>
  janitor::clean_names() |>
  #drop_na() |>
  rename(region = x1) |>
  rename(state = back_to_content_page) |>
  #filter_all(any_vars(!is.na(.))) |>
  rename_with(~ gsub("^x", "", .), starts_with("x")) |>
  select(-matches("^([4-9]|[1-9][0-9]|1[01][0])$")) |>
  select(-c("102", "104", "106", "108")) |>
  mutate(region = case_when(
    state %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", 
                 "Connecticut", "New York", "New Jersey", "Pennsylvania", 
                 "Delaware", "Maryland") ~ "Northeast",
    state %in% c("Michigan","Wisconsin","Minnesota") ~ "Lake States",
    state %in% c("Ohio","Indiana","Illinois","Iowa","Missouri") ~ "Corn Belt",
    state %in% c("North Dakota", "South Dakota", "Nebraska", "Kansas") ~ "Northern Plains",
    state %in% c("Virginia", "West Virginia", "North Carolina", "Kentucky", "Tennessee") ~ "Appalachia",
    state %in% c("South Carolina", "Georgia", "Florida", "Alabama") ~ "Southeast",
    state %in% c("Mississippi", "Arkansas","Louisiana") ~ "Delta States",
    state %in% c("Oklahoma","Texas") ~ "Southern Plains",
    state %in% c("Montana","Idaho","Wyoming","Colorado","New Mexico","Arizona","Utah","Nevada") ~ "Mountain Region",
    state %in% c("Washington","Oregon","California") ~ "West Coast",
    state %in% c("Alaska","Hawaii") ~ "Other States"
    #TRUE ~ "Other"  # Default case for states not in the Northeast
  )) |>
  filter(!is.na(state)) |>
  filter(!is.na(region)) |>
  filter(region != "Other States") |>
  mutate(across(
    .cols = matches("^\\d{4}$"), 
    .fns = ~ as.numeric(.),  
    .names = "{.col}" 
  )) |>
  pivot_longer(
    cols = 3:56,
    names_to = "year",
    values_to = "milk_production"
  )  

joined_data <- milk_cows |>
  inner_join(milk_production, by = c("region", "state", "year"))
