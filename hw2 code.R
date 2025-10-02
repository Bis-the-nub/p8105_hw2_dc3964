## Problem 1
### loading packages
library(tidyverse)
library(readxl)

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
  mutate(across(c(month:year), as.integer)) |> 
  mutate(month = factor(month.name[month], levels = month.name)) |> 
  ## changed format from 2 digit to 4 digit 
  mutate(year = if_else(as.integer(year) <= 15,
                          2000 + as.integer(year),
                          1900 + as.integer(year))) |> 
  ## changed format from integer to character
  mutate(year = as.character(year)) |> 
  select(year, month, everything()) |> 
  arrange(year, month)

### tidying unemployment
unemployment =
  read_csv("./data/unemployment.csv") |> 
  rename(year = Year) |> 
  mutate(year = as.character(year)) |> 
  pivot_longer(Jan:Dec, names_to = "month", values_to = "unemployment_rate")

### Merging all datasets
partial_complete_p1 = left_join(pols_month, snp) 
complete_data_p1 = left_join(partial_complete_p1, unemployment)


## Problem 2
### Loading and tidying three trash wheels
mr_trashwheel =
  read_excel("./data/202509 Trash Wheel Collection Data.xlsx",
             sheet = "Mr. Trash Wheel",
             range = "A2:N710",
             na = c("")
             ) |> 
  janitor::clean_names() |> 
  filter(!is.na(dumpster)) |> 
  mutate(sports_balls = as.integer(round(sports_balls))) |>
  mutate(wheel_tag = "mr")

professor_trashwheel =
  read_excel("./data/202509 Trash Wheel Collection Data.xlsx",
             sheet = "Professor Trash Wheel",
             range = "A2:M135", 
             na = c("")
             ) |> 
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |> 
  mutate(year = as.character(year)) |> 
  mutate(wheel_tag = "professor")

gwynnda_trashwheel =
  read_excel("./data/202509 Trash Wheel Collection Data.xlsx",
             sheet = "Gwynns Falls Trash Wheel",
             range = "A2:L352",
             na = c("")
             ) |> 
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |> 
  mutate(year = as.character(year)) |> 
  mutate(wheel_tag = "gwynnda")

### Merging data
partial_complete_p2 = full_join(mr_trashwheel, professor_trashwheel)
complete_data_p2 = full_join(partial_complete_p2, gwynnda_trashwheel) |> 
  select(wheel_tag, everything())

### Total weight of trash collected by Professor Trash Wheel
complete_data_p2 |> 
  filter(wheel_tag == "professor") |>   
  pull(weight_tons) |>                   
  sum()      

### Total number of cigarette butts collected by Gwynnda in June of 2022
complete_data_p2 |> 
  filter(wheel_tag == "gwynnda", month == "June", year == "2022") |>   
  pull(cigarette_butts) |>                   
  sum() ## note that in the rmd file I changed the code a bit
        ## just to get rid of scientific symbol

## Problem 3
### Loading and tidying data
zip_codes = 
  read_csv("./data/Zip Codes.csv") |> 
  janitor::clean_names()

zip_zillow =
  read_csv("./data/Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv") |>
  pivot_longer(
    cols = matches("^\\d{4}-\\d{1,2}-\\d{1,2}$"), 
    names_to = "date",
    values_to = "zori"
  ) |> 
  janitor::clean_names() |> 
  rename(zip_code = region_name)

### Merging data
complete_data_p3 = left_join(zip_codes, zip_zillow, 
                             relationship = "many-to-many") |> 
  select(zip_code, county_name, date, zori, everything())

### calculating unique zip codes
unique_zips =
  anti_join(zip_codes, zip_zillow, by = "zip_code") |>
  arrange(zip_code)

### forming the table between different zori date values
covid_19 =
  complete_data_p3 |> 
  select(zip_code, date, zori, county, neighborhood) |> 
  filter(date %in% c("2020-01-31", "2021-01-31")) |> 
  pivot_wider(
    names_from = date,
    values_from = zori,
    names_prefix = "zori_"
 ) |> 
  mutate(
    drop = `zori_2020-01-31` - `zori_2021-01-31`
  ) |> 
  arrange(desc(drop)) |> 
  slice(1:10)

knitr::kable(covid_19)
  