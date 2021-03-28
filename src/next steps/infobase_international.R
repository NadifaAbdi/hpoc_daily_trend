## International_Infobase script

library(PHACTrendR)
library(dplyr)
library(readr)
library(stringr)
library(zoo)
library(googlesheets4)

df_int<-PHACTrendR::import_international_data() %>%
  filter(!is.na(continent)) %>% #OWID has some continent-wide groupings that we can ignore
  select(iso_code, location, date, total_cases, new_cases, population, total_deaths, new_deaths)%>%
  left_join(PHACTrendR::EN_FR_country_names, by=c("location"="name_en")) %>%
  ungroup()

df_int_final<-df_int %>%
  group_by(location) %>%
  mutate(new_cases_14_days=rollsumr(new_cases, 14, fill=NA),
         new_deaths_14_days=rollsumr(new_deaths,14, fill=NA),
         new_cases_14_days_100k=(new_cases_14_days/population)*100000,
         new_deaths_14_days_100k=(new_deaths_14_days/population)*100000,
         total_cases_100k=(total_cases/population)*100000,
         total_deaths_100k=(total_deaths/population)*100000) %>%
  rename(name_en=location) %>%
  select(iso_code, name_en, name_fr, date, 
         new_cases, new_cases_14_days, new_cases_14_days_100k, total_cases, total_cases_100k,
         new_deaths, new_deaths_14_days, new_deaths_14_days_100k, total_deaths, total_deaths_100k)
  

#if wanting code to be run interactively, can do the following:
gs4_auth()

#if wanting code to be run non-interactively, can do the following (this JSON file must be saved locally)
Username<-Sys.getenv("USERNAME")
gs4_auth(path = paste0("C:/Users/",Username,"/.R/gargle/Update infobase international-c0ed98f022ba.json"))
  
  spreadsheet_URL<-"https://docs.google.com/spreadsheets/d/1N0pAuEKgEDBkOMCiuh7jN-7xstMbgPGzCKOlhnWhru4"
  sheet_name<-"data_sheet"
  
  write_sheet(data=df_int_final,
              ss = spreadsheet_URL,
              sheet = sheet_name)
