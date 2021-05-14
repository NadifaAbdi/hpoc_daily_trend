#import SALT data
#salt_raw <- PHACTrendR::import_SALT_data()

salt_raw <- read.csv("Submitted+Reports.csv")
  
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

n_minus_two<-max(SALT$update_date)-2 

SALT2 <- SALT %>%
  filter(Date <= max(update_date)-2) %>% #this gives N-2 data
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
  ungroup()


#this function is ran 7 times because it uses the For loop below containing the most recent 7 days
#this function recalculates the national numbers for all 7 days
correct_national_numbers<-function(input_date){
  input_date<-as.Date(input_date)
  
  SALT_corrections<-SALT_complete %>%
    filter(!Jurisdiction=="Canada") %>%
    filter(Date<= input_date & Date>= input_date-6) %>% #gives most recent 7 days
    group_by(Jurisdiction) %>%
    summarise(weekly_total_tests_performed=sum(tests_performed),
              weekly_total_tests_positive=sum(positive_tests),
              weekly_tests_performed_7ma=mean(tests_performed), #if a PT doesn't report on the 6th and 7th day, it would contribute a 5 day MA in the national 7MA 
              .groups="drop_last") %>%
    summarise(weekly_total_tests_performed=sum(weekly_total_tests_performed),
              weekly_total_tests_positive=sum(weekly_total_tests_positive),
              weekly_tests_performed_7ma=sum(weekly_tests_performed_7ma),
              weekly_percent_positive=weekly_total_tests_positive/weekly_total_tests_performed) %>%
    mutate(Jurisdiction="Canada",
           Date=input_date) %>%
    select(Date, Jurisdiction, weekly_tests_performed_7ma, weekly_percent_positive)
  
  corrected_7ma<-SALT_corrections$weekly_tests_performed_7ma
  corrected_perc_pos<-SALT_corrections$weekly_percent_positive
  
  SALT_complete[SALT_complete$Jurisdiction=="Canada"&SALT_complete$Date==input_date, "tests_performed_7ma"]<-corrected_7ma
  SALT_complete[SALT_complete$Jurisdiction=="Canada"&SALT_complete$Date==input_date,"percent_positive_7ma"] <- corrected_perc_pos
  return(SALT_complete)
}

n_minus_eight<-n_minus_two-6


correction_dates<-seq.Date(from=n_minus_eight,to = n_minus_two,by = 1) #gives the most recent 7 days in N-2 way of reporting

for (i in correction_dates){ 
  SALT_complete<-correct_national_numbers(input_date=i)
}





# want to replace and add a new row of data for each PT (for example, if a PT doesn't report today, data for that PT would be missing)
missing_date<-n_minus_two

  PT_corrections<-SALT_complete %>%
    filter(!Jurisdiction=="Canada") %>% 
    filter(Date<= missing_date & Date>= missing_date-6) %>% #gives most recent 7 days #for ex. may 4-10, then 3-19, then 2-8 , etc
    group_by(Jurisdiction) %>%
    summarise(weekly_total_tests_performed=sum(tests_performed),
              weekly_total_tests_positive=sum(positive_tests),
             tests_performed_7ma2=mean(tests_performed), #if a PT doesn't report on the 6th and 7th day, it would contribute a 5 day MA in the national 7MA 
              .groups="drop_last") %>%
    mutate(Date = missing_date,
           percent_positive_7ma2 = weekly_total_tests_positive/weekly_total_tests_performed) %>%
    select(Jurisdiction, Date, tests_performed_7ma2, percent_positive_7ma2)
  
# prepare the data set for merging
SALT_complete2 <- SALT_complete %>%
    select(Jurisdiction, Date, tests_performed_7ma, percent_positive_7ma)  

# merge the data sets  
Combine <- SALT_complete2 %>% 
  full_join(PT_corrections, by=c("Jurisdiction","Date"))

# replace missing values (if any) with the newly calculated data  
replace <- Combine %>%
    mutate(tests_performed_7ma_final = ifelse(is.na(tests_performed_7ma), tests_performed_7ma2, tests_performed_7ma),
           percent_positive_7ma_final = ifelse(is.na(percent_positive_7ma), percent_positive_7ma2, percent_positive_7ma)) %>%
  select(Jurisdiction, Date, tests_performed_7ma_final, percent_positive_7ma_final) 
           
    
# keep this week's date and last week's date
table <- replace %>%
  filter(Date ==max(Date) | (Date==max(Date)-7)) %>%
  mutate(week_label=ifelse(Date == max(Date), "thisweek","lastweek")) %>%
  select(Jurisdiction,week_label,tests_performed_7ma_final, percent_positive_7ma_final) %>%
  pivot_longer(cols = c("tests_performed_7ma_final","percent_positive_7ma_final"),
               names_to="type",
               values_to="value") %>%
  pivot_wider(names_from = c(week_label, type),
              names_glue= "{type}_{week_label}" ,
              values_from=value) 
















# Final dataset: date, jurisdiction, cumulative tests performed, 7dma tests, 7dma tests/100k, 7dma % positivity

SALT_final<-SALT_complete %>%
  arrange(Date) %>%
  group_by(Jurisdiction) %>%
  mutate(cumulative_tests=cumsum(tests_performed)) %>%
  left_join(PHACTrendR::latest_can_pop, by="Jurisdiction") %>%
  mutate(tests_performed_7ma_per_100k=round((tests_performed_7ma/Population)*100000,digits = 1),
         tests_performed_7ma=round(tests_performed_7ma, digits = 1),
         percent_positive_7ma=ifelse(is.na(percent_positive_7ma), 0, round(percent_positive_7ma*100, digits=2))) %>%
  select(Date, Jurisdiction, cumulative_tests, tests_performed_7ma, tests_performed_7ma_per_100k, percent_positive_7ma)%>%
  rename(`Seven day rolling percent positivity`=percent_positive_7ma,
         `Cumulative tests`=cumulative_tests,
         `Total tests performed, 7-day moving average`=tests_performed_7ma,
         `Total tests performed per 100k population, 7-day moving average`=tests_performed_7ma_per_100k)



  
  
  
 



