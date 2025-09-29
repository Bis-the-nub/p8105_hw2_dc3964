## Problem 1
### loading packages
library(tidyverse)
library(lubridate)

### tidying pol-month
pols_month =
  read_csv("./data/pols-month.csv") |> 
  separate(mon, c("year", "month", "day")) |> 
  mutate(month = month.abb[as.integer(month)]) |> 
  mutate(president = case_when(
    prez_gop == 1 ~ "gop",
    prez_dem == 1 ~ "dem",
    TRUE          ~ NA_character_
  )) |> 
  select(-day, -prez_gop, -prez_dem)

### tidying snp
snp = 
  read_csv("./data/snp.csv") |> 
  separate(date, c("month", "day", "year")) |> 
  select(year, month, everything()) |> 
  select(-day) |> 
  mutate(month = month.abb[as.integer(month)]) |> 
  ## changed format from 2 digit to 4 digit 
  mutate(year = if_else(as.integer(year) <= 15,
                          2000 + as.integer(year),
                          1900 + as.integer(year))) |> 
  ## changed format from integer to character
  mutate(year = as.character(year))

### tidying unemployment
unemployment =
  read_csv("./data/unemployment.csv") |> 
  rename(year = Year) |> 
  mutate(year = as.character(year)) |> 
  pivot_longer(Jan:Dec, names_to = "month", values_to = "unemployment_rate")

### Merging all datasets
partial_complete = left_join(pols_month, snp) 
complete_data_p1 = left_join(partial_complete, unemployment)


## Problem 2
