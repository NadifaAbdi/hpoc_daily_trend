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
  filter(Date <= max(update_date)-1) %>% #this gives N-2 data
  select(-Latest.Update.Date,-update_date)%>%
  filter(Date>="2021-01-23") %>% #Issues with historical data missing for some PTs - only taking last two weeks data for now.
  arrange(Jurisdiction,datetime)

# calculate 7MA for PTs 
SALT_PT <- SALT2 %>%
  group_by(Jurisdiction) %>%
  mutate(daily_percent_positive = (positive_tests/tests_performed),
         tests_performed_7ma = rollmean(tests_performed, k=7, fill=NA, align="right"),
         tests_performed_7_sum=rollsum(tests_performed, k=7, fill=NA, align="right"),
         tests_positive_7_sum=rollsum(positive_tests, k=7, fill=NA, align="right"),
         percent_positive_7ma = tests_positive_7_sum/tests_performed_7_sum ) %>%
  select(Jurisdiction, Date, tests_performed, positive_tests, daily_percent_positive, tests_performed_7ma, percent_positive_7ma) %>%
  ungroup()


# calculate Canadian totals by summing all provinces
SALT_national <- SALT_PT %>%
  group_by(Date) %>%
  summarise(tests_performed = sum(tests_performed),
            positive_tests = sum(positive_tests) ) %>%
  #tests_performed_7ma = sum(tests_performed_7ma))
  mutate(daily_percent_positive = (positive_tests/tests_performed),
         tests_performed_7ma = rollmean(tests_performed, k=7, fill=NA, align="right"),
         tests_performed_7_sum=rollsum(tests_performed, k=7, fill=NA, align="right"),
         tests_positive_7_sum=rollsum(positive_tests, k=7, fill=NA, align="right"),
         percent_positive_7ma = tests_positive_7_sum/tests_performed_7_sum,
         Jurisdiction = "Canada") %>%
  select(Jurisdiction, Date, tests_performed, positive_tests, daily_percent_positive, tests_performed_7ma, percent_positive_7ma)

# combine PT and National data
SALT_complete <- rbind(SALT_PT,SALT_national) %>%
  ungroup()%>%
  filter(Date ==max(Date) | (Date==max(Date)-7)) %>%
  group_by(Jurisdiction) %>%
  select(Jurisdiction, Date, tests_performed_7ma, percent_positive_7ma) 


##code to calculate accurate Canada-wide stats (to protect against reporting lag from a PT)
corrected_Can <- SALT2 %>%
  filter(Date>=max(Date)-6) %>%
  group_by(Jurisdiction) %>%
  summarise(tests_performed_7sum=sum(tests_performed), #calculate 7day sums for each PT
            positive_tests_7sum=sum(positive_tests),
            .groups="drop_last") %>%
  ungroup()%>%
  summarise(Can_tests_performed_7sum=sum(tests_performed_7sum, na.rm=TRUE), #get 7 day sum for Canada
            Can_positive_tests_7sum=sum(positive_tests_7sum, na.rm=TRUE)) %>%
  mutate(Can_tests_performed_7ma = Can_tests_performed_7sum/7,
        Can_percent_positive_7ma = Can_positive_tests_7sum/Can_tests_performed_7sum) %>%
  as.numeric()

#replace the data for the max date
SALT_complete[SALT_complete$Jurisdiction=="Canada"&SALT_complete$Date==max(SALT_complete$Date),"tests_performed_7ma"] <- corrected_Can[3]
SALT_complete[SALT_complete$Jurisdiction=="Canada"&SALT_complete$Date==max(SALT_complete$Date),"percent_positive_7ma"] <- corrected_Can[4]



##use a function to calculate and replace accurate PT stats (to protect against reporting lag from a PT)
PT_corrections <- function(Jurisdiction="") {
  
  corrected_pt <- SALT2 %>%
    filter(Date>=max(Date)-6) %>%
    filter(Jurisdiction == Jurisdiction) %>%
    summarise(tests_performed_7sum=sum(tests_performed), #calculate sum for each PT
              positive_tests_7sum=sum(positive_tests),
              tests_performed_7ma = mean(tests_performed)) %>% #calculate 7MA for each PT
    mutate(pt_percent_positive_7ma = positive_tests_7sum/tests_performed_7sum) %>%
    as.numeric()
  
  #replace the data for the max date
  SALT_complete[SALT_complete$Jurisdiction==Jurisdiction&SALT_complete$Date==max(SALT_complete$Date),"tests_performed_7ma"] <- corrected_pt[3]
  SALT_complete[SALT_complete$Jurisdiction==Jurisdiction&SALT_complete$Date==max(SALT_complete$Date),"percent_positive_7ma"] <- corrected_pt[4]
}

PT_corrections(Jurisdiction = "Alberta")
PT_corrections(Jurisdiction = "British Columbia")
PT_corrections(Jurisdiction = "Ontario")
PT_corrections(Jurisdiction = "Manitoba")
PT_corrections(Jurisdiction = "Quebec")
PT_corrections(Jurisdiction = "New Brunswick")
PT_corrections(Jurisdiction = "Prince Edward Island")
PT_corrections(Jurisdiction = "New foundland and Labrador")
PT_corrections(Jurisdiction = "Nova Scotia")
PT_corrections(Jurisdiction = "Saskatchewan")
PT_corrections(Jurisdiction = "Nunavut")
PT_corrections(Jurisdiction = "Northwest Territories")
PT_corrections(Jurisdiction = "Yukon")


#reformat the table
table <- SALT_complete %>%
  mutate(percent_positive_7ma = ifelse(round(percent_positive_7ma,digits=4) < 0.001, percent(percent_positive_7ma,accuracy = 0.01), percent(percent_positive_7ma,accuracy = 0.1)),
         tests_performed_7ma = number(tests_performed_7ma,big.mark = "," ,accuracy = 1)) %>%
  
  pivot_wider(names_from = Date, values_from=c(tests_performed_7ma,percent_positive_7ma)) %>%
  factor_PT_west_to_east(size="big",Canada_first = TRUE) %>%
  arrange(Jurisdiction) 


  
  
 



