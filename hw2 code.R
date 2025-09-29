## Problem 1
library(tidyverse)
library(lubridate)
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

snp = 
  read_csv("./data/snp.csv") |> 
  separate(date, c("month", "day", "year")) |> 
  select(year, month, everything()) |> 
  select(-day) |> 
  mutate(month = month.abb[as.integer(month)]) |> 
  mutate(year = if_else(as.integer(year) <= 15,
                          2000 + as.integer(year),
                          1900 + as.integer(year))) |> 
  mutate(year = as.character(year))

unemployment =
  read_csv("./data/unemployment.csv") |> 
  rename(year = Year) |> 
  mutate(year = as.character(year)) |> 
  pivot_longer(Jan:Dec, names_to = "month", values_to = "unemployment_rate")

test = left_join(pols_month, snp) 
test2 = inner_join(test, unemployment)
