# # Infobase lab testing

#this code uses the data sets from the "Daily SALT Code_v2.R" code

SALT2_info <- SALT %>%
  filter(Date <= max(update_date)-2) %>% #this gives N-2 data
  select(-Latest.Update.Date,-update_date)%>%
  arrange(Jurisdiction,datetime)

SALT3_info<-SALT2_info %>%
  mutate(reported="Yes") %>%
  complete(Date, Jurisdiction, fill = list(reported="No"))

  

# calculate 7MA for PTs 
SALT_PT_info <- SALT3_info %>%
  group_by(Jurisdiction) %>%
  mutate(daily_percent_positive = (positive_tests/tests_performed),
         tests_performed_7ma=rollapply(tests_performed,7, mean,na.rm=TRUE,fill=NA,align="right"),
         tests_performed_7_sum=rollapply(tests_performed, 7, sum, na.rm=TRUE,fill=NA, align="right"),
         tests_positive_7_sum=rollapply(positive_tests, 7,sum, na.rm=TRUE, fill=NA, align="right"),
         percent_positive_7ma = tests_positive_7_sum/tests_performed_7_sum ) %>%
  select(Jurisdiction, Date, tests_performed, positive_tests, daily_percent_positive, tests_performed_7ma,percent_positive_7ma, reported) %>%
  ungroup()


# calculate Canadian totals by summing all provinces
SALT_national_info <- SALT_PT_info %>%
  group_by(Date) %>%
  summarise(tests_performed = sum(tests_performed, na.rm=TRUE),
            positive_tests = sum(positive_tests, na.rm=TRUE)) %>%
  #tests_performed_7ma = sum(tests_performed_7ma))
  mutate(daily_percent_positive = (positive_tests/tests_performed),
         tests_performed_7ma = rollmean(tests_performed, k=7, fill=NA, align="right"),
         tests_performed_7_sum=rollsum(tests_performed, k=7, fill=NA, align="right"),
         tests_positive_7_sum=rollsum(positive_tests, k=7, fill=NA, align="right"),
         percent_positive_7ma = tests_positive_7_sum/tests_performed_7_sum,
         Jurisdiction = "Canada",
         reported="NA") %>%
  select(Jurisdiction, Date, tests_performed, positive_tests, daily_percent_positive, tests_performed_7ma, percent_positive_7ma, reported)

# combine PT and National data
SALT_complete_info <- rbind(SALT_PT_info,SALT_national_info) %>%
  ungroup()

correct_national_numbers_info<-function(input_date){
  input_date<-as.Date(input_date)
  
  SALT_corrections_info<-SALT_complete_info %>%
    filter(!Jurisdiction=="Canada") %>%
    filter(Date<= input_date & Date>= input_date-6) %>%
    group_by(Jurisdiction) %>%
    summarise(weekly_total_tests_performed=sum(tests_performed, na.rm=TRUE),
              weekly_total_tests_positive=sum(positive_tests, na.rm=TRUE),
              weekly_tests_performed_7ma=mean(tests_performed, na.rm=TRUE),
              .groups="drop_last") %>%
    summarise(weekly_total_tests_performed=sum(weekly_total_tests_performed, na.rm=TRUE),
              weekly_total_tests_positive=sum(weekly_total_tests_positive, na.rm=TRUE),
              weekly_tests_performed_7ma=sum(weekly_tests_performed_7ma, na.rm=TRUE),
              weekly_percent_positive=weekly_total_tests_positive/weekly_total_tests_performed) %>%
    mutate(Jurisdiction="Canada",
           Date=input_date) %>%
    select(Date, Jurisdiction, weekly_tests_performed_7ma, weekly_percent_positive)
  
  corrected_7ma<-SALT_corrections_info$weekly_tests_performed_7ma
  corrected_perc_pos<-SALT_corrections_info$weekly_percent_positive
  
  SALT_complete_info[SALT_complete_info$Jurisdiction=="Canada"&SALT_complete_info$Date==input_date, "tests_performed_7ma"]<-corrected_7ma
  SALT_complete_info[SALT_complete_info$Jurisdiction=="Canada"&SALT_complete_info$Date==input_date,"percent_positive_7ma"] <- corrected_perc_pos
  return(SALT_complete_info)
}

n_minus_eight<-n_minus_two-6


correction_dates_info<-seq.Date(from=n_minus_eight,to = n_minus_two,by = 1)

for (i in correction_dates_info){ 
  SALT_complete_info<-correct_national_numbers_info(input_date=i)
}

# Final dataset: date, jurisdiction, cumulative tests performed, 7dma tests, 7dma tests/100k, 7dma % positivity

SALT_final_info<-SALT_complete_info %>%
  arrange(Date) %>%
  group_by(Jurisdiction) %>%
  mutate(cumulative_tests=cumsum(tests_performed)) %>%
  left_join(PHACTrendR::latest_can_pop, by="Jurisdiction") %>%
  mutate(tests_performed_7ma_per_100k=round((tests_performed_7ma/Population)*100000,digits = 1),
         tests_performed_7ma=round(tests_performed_7ma, digits = 1),
         percent_positive_7ma=ifelse(is.na(tests_performed_7ma), NA, 
                                     ifelse(is.na(percent_positive_7ma), 0, round(percent_positive_7ma*100, digits=2)))) %>%
  mutate(update = ifelse(reported == "Yes", 1,
                         ifelse(reported == "No", 0, NA))) %>%
  select(Date, Jurisdiction, cumulative_tests, tests_performed_7ma, tests_performed_7ma_per_100k, percent_positive_7ma, update)%>%
  rename(date=Date,
         prname=Jurisdiction,
         numtests=cumulative_tests,
         avgtests_last7=tests_performed_7ma,
         avgratetests_last7=tests_performed_7ma_per_100k,
         avgpositivity_last7=percent_positive_7ma)



# #if wanting code to be run interactively, can do the following:
# gs4_auth()

#if wanting code to be run non-interactively, can do the following (this JSON file must be saved locally in the designated folder)
# Username<-Sys.getenv("USERNAME")
# token_name<-"Update infobase international-c0ed98f022ba.json"
# gs4_auth(path = paste0("C:/Users/",Username,"/.R/gargle/",token_name))

spreadsheet_URL<-"https://docs.google.com/spreadsheets/d/1QbFC51QJq8H4s5Q4UOjikEcuGczQXtxfXRrEVpFgiX8"
sheet_name<-"data_sheet"

write_sheet(data=SALT_final_info,
            ss = spreadsheet_URL,
            sheet = sheet_name)
