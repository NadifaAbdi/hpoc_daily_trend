#import SALT data
#salt_raw <- PHACTrendR::import_SALT_data()
salt_raw <- read.csv(file="C:/rmd/hpoc_daily_trend/SALT2.csv")

#rename variables 
SALT <- salt_raw %>%
  select(Report.Date,Jurisdiction,Tests.Performed,Positive.Test.Results,Percent.Positive.Test.Results, Latest.Update.Date) %>%
  rename(tests_performed=Tests.Performed,
         positive_tests=Positive.Test.Results,
         percent_positive=Percent.Positive.Test.Results) %>%
  mutate(update_date = as.Date(str_sub(Latest.Update.Date, 1, 10)),
         Date = as.Date(str_sub(Report.Date, 1, 10)),
         Time = as_hms(str_sub(Report.Date, 13, 20)),
         datetime = strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S"),
         positive_tests = ifelse (!is.na(positive_tests), positive_tests, round(tests_performed*(percent_positive/100))),  #some PTs (AB, ON) only report % positive
         percent_positive = ifelse (!is.na(percent_positive), percent_positive, round((positive_tests/tests_performed)*100, digits = 3)))

SALT2 <- SALT %>%
  select(-Latest.Update.Date,-update_date)%>%
  #mutate(Start_of_week=floor_date(Date, "week"),
         #End_of_week=date(Start_of_week)+6,
         #Week=paste(str_sub(months(Start_of_week),1,3),"-",day(Start_of_week), " to ", str_sub(months(End_of_week),1,3),"-",day(End_of_week)),
         #Week_before=paste(str_sub(months(date(Start_of_week)-7),1,3),"-",day(date(Start_of_week)-7), " to ", str_sub(months(date(End_of_week)-7),1,3),"-",day(date(End_of_week)-7))) %>%
  filter(Date <= Sys.Date()-2) %>% #this gives N-2 data
  #mutate(Current_week=ifelse(date(Date)+7 <= max(Date),"No","Yes")) %>%
  filter(Date>="2021-01-23") %>% #Issues with historical data missing for some PTs - only taking last two weeks data for now.
  arrange(Jurisdiction,datetime)

# calculate 7MA for PTs 
SALT_PT <- SALT2 %>%
  group_by(Jurisdiction) %>%
  mutate(daily_percent_positive = (positive_tests/tests_performed),
    tests_performed_7ma = rollmean(tests_performed, k=7, fill=NA, align=c("right")),
    percent_positive_7ma = rollmean(daily_percent_positive, k=7, fill=NA, align=c("right")) ) %>%
  select(Jurisdiction, Date, tests_performed, positive_tests, daily_percent_positive, tests_performed_7ma, percent_positive_7ma)

# calculate Canadian totals by summing all provinces
SALT_national <- SALT_PT %>%
  group_by(Date) %>%
  summarise(tests_performed = sum(tests_performed),
            positive_tests = sum(positive_tests) ) %>%
            #tests_performed_7ma = sum(tests_performed_7ma))
  mutate(daily_percent_positive = (positive_tests/tests_performed),
         tests_performed_7ma = rollmean(tests_performed, k=7, fill=NA, align=c("right")),
         percent_positive_7ma = rollmean(daily_percent_positive, k=7, fill=NA, align=c("right")), 
         Jurisdiction = "Canada") %>%
  select(Jurisdiction, Date, tests_performed, positive_tests, daily_percent_positive, tests_performed_7ma, percent_positive_7ma)


# combine PT and National data
SALT_complete <- rbind(SALT_PT,SALT_national) %>%
  ungroup()%>%
  filter(Date ==max(Date) | (Date==max(Date)-7)) %>%
  group_by(Jurisdiction) %>%
  select(Jurisdiction, Date, tests_performed_7ma, percent_positive_7ma) %>%

  mutate(percent_positive_7ma = ifelse(round(percent_positive_7ma,digits=4) < 0.001, percent(percent_positive_7ma,accuracy = 0.01), percent(percent_positive_7ma,accuracy = 0.1)),
         tests_performed_7ma = number(tests_performed_7ma,big.mark = "," ,accuracy = 1)) %>%
  
  pivot_wider(names_from = Date, values_from=c(tests_performed_7ma,percent_positive_7ma)) %>%
  factor_PT_west_to_east(size="big",Canada_first = TRUE) %>%
  arrange(Jurisdiction)


  
  
  
 



