
library(shiny)
library(shinydashboard)
library(rvest)
library(tidyverse)
library(gganimate)
library(shinythemes)
library(highcharter)
library(formattable)
library(lubridate)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(sever)
library(jsonlite)
library(httr)

options(scipen = 999)


mapdata <- read_csv("mapdata.csv")


### table global

res <- GET("https://api.kawalcorona.com/")

dat <- fromJSON(rawToChar(res$content)) %>% 
  pull(attributes)

dat <- dat %>% 
  rename(Country = Country_Region) %>% 
  select(Country, Confirmed, Recovered, Deaths)

selected <- top_n(dat, n = 5, wt = Confirmed) %>% pull(Country)


dat_map <- dat %>% 
  mutate(Country = case_when(
    Country == "US" ~ "United States of America",
    Country == "Korea, South" ~ "South Korea",
    Country == "Czechia" ~ "Czech Republic",
    Country == "Serbia" ~ "Republic of Serbia",
    Country == "Taiwan*" ~ "Taiwan",
    TRUE ~ Country
  )) %>% 
  left_join(mapdata, by = c("Country" = "name"))


#### confirmed

today_confirmed <- dat %>% 
  select(Country, Confirmed) %>% 
  mutate(datetime = Sys.Date() %>% as.character(),
         Country = as.factor(Country))

colnames(today_confirmed) <- colnames(today_confirmed) %>% str_to_lower()

ts_confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

date_seq <- seq(from = ymd("2020-01-22"), 
                length.out = ncol(ts_confirmed) - 4,
                by = "day")

colnames(ts_confirmed) <- c("province_state", "country", "lat", "long", as.character(date_seq))



ts_confirmed_long <- pivot_longer(
  ts_confirmed,
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "confirmed"
) %>% 
  select(country, confirmed, datetime) %>% 
  bind_rows(today_confirmed) %>% 
  arrange(country)



#### recovered

today_recovered <- dat %>% 
  select(Country, Recovered) %>% 
  mutate(datetime = Sys.Date() %>% as.character(),
         Country = as.factor(Country))

colnames(today_recovered) <- colnames(today_recovered) %>% str_to_lower()

ts_recovered <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))

date_seq_recov <- seq(from = ymd("2020-01-22"), 
                      length.out = ncol(ts_recovered) - 4,
                      by = "day")

colnames(ts_recovered) <- c("province_state", "country", "lat", "long", as.character(date_seq_recov))



ts_recovered_long <- pivot_longer(
  ts_recovered,
  cols = -c(province_state, country, lat, long), 
  names_to = "datetime", 
  values_to = "recovered"
) %>% 
  select(country, recovered, datetime) %>% 
  bind_rows(today_recovered) %>% 
  arrange(country)




#### deaths


today_deaths <- dat %>% 
  select(Country, Deaths) %>% 
  mutate(datetime = Sys.Date() %>% as.character(),
         Country = as.factor(Country))

colnames(today_deaths) <- colnames(today_deaths) %>% str_to_lower()

ts_deaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))


date_seq_deaths <- seq(from = ymd("2020-01-22"), 
                       length.out = ncol(ts_deaths) - 4,
                       by = "day")


colnames(ts_deaths) <- c("province_state", "country", "lat", "long", as.character(date_seq_deaths))



ts_deaths_long <- pivot_longer(
  ts_deaths, 
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "deaths"
) %>% 
  select(country, deaths, datetime) %>% 
  bind_rows(today_deaths) %>% 
  arrange(country)

### Indonesia

indo <- GET("https://api.kawalcorona.com/indonesia/provinsi")
dat_indo <- fromJSON(rawToChar(indo$content)) %>% pull(attributes) %>% 
  mutate(Provinsi = case_when(
    
    Provinsi == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta",
    Provinsi == "Kepulauan Bangka Belitung" ~ "Bangka Belitung",
    Provinsi == "Papua Barat" ~ "Irian Jaya Barat",
    
    TRUE ~ Provinsi
    
  ))



map_indo <- read.csv("map-indo.csv") %>% 
  left_join(dat_indo, by = "Provinsi") %>% 
  select(woe_name, kasus = Kasus_Posi)

dat_indo <- dat_indo %>% 
  rename(Province = Provinsi, 
         Confirmed = Kasus_Posi, 
         Recovered = Kasus_Semb, 
         Death = Kasus_Meni) %>% 
  mutate_if(is.numeric, ~comma(.,digits = 0)) %>% 
  arrange(desc(Confirmed)) %>% 
  select(Province, Confirmed, Recovered, Death)

ts_confirmed_indo <- ts_confirmed_long %>% 
  filter(country == "Indonesia")

ts_recovered_indo <- ts_recovered_long %>% 
  filter(country == "Indonesia")

ts_deaths_indo <- ts_deaths_long %>% 
  filter(country == "Indonesia")

ts_indo <- ts_confirmed_indo %>% 
  bind_cols(ts_recovered_indo) %>% 
  bind_cols(ts_deaths_indo) %>% 
  select(datetime, confirmed, recovered, deaths)

ts_indo_longer <- pivot_longer(
  ts_indo,
  cols = -c(datetime),
  names_to = "variable",
  values_to = "count"
)

infobox_ts_indo <- ts_indo %>% slice(nrow(.))
