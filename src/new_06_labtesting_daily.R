#import incremental SALT data
salt_raw <- PHACTrendR::import_SALT_data()

#salt_raw <- read.csv("Submitted+Reports.csv")
  
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


SALT1b <- SALT %>%
  filter(Date <= max(update_date)-2) %>% #this gives N-2 data
  select(-Latest.Update.Date,-update_date, -Time, -Report.Date)%>%
  filter(Date>="2021-01-23") %>% #Issues with historical data missing for some PTs - only taking last two weeks data for now.
  arrange(Jurisdiction,datetime) %>%
  select(Date,Jurisdiction,positive_tests,tests_performed,percent_positive) 


# create a date for each Jurisdiction using dummy data
dummy_data<-expand.grid(Date=tidyr::full_seq(SALT1b$Date,1),
                        Jurisdiction=unique(SALT1b$Jurisdiction), #extracts unique PTs
                        positive_tests = NA,
                        tests_performed = NA,
                        percent_positive = NA)

SALT2 <- rbind(SALT1b,dummy_data) %>%
  arrange(Jurisdiction, Date) %>% # sort your data (most sorts will put the NAs last. So they will be the duplicates omitted in the next step) 
  distinct(Jurisdiction, Date, .keep_all = TRUE) # .keep_all will keep all the variables


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
  summarise(tests_performed = sum(tests_performed, na.rm = TRUE),
            positive_tests = sum(positive_tests, na.rm = TRUE) ) %>%
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
    summarise(weekly_total_tests_performed=sum(tests_performed,na.rm = TRUE),
              weekly_total_tests_positive=sum(positive_tests,na.rm = TRUE),
              weekly_tests_performed_7ma=mean(tests_performed, na.rm = TRUE), #if a PT doesn't report on the 6th and 7th day, it would contribute a 5 day MA in the national 7MA 
              .groups="drop_last") %>%
    summarise(weekly_total_tests_performed=sum(weekly_total_tests_performed, na.rm = TRUE),
              weekly_total_tests_positive=sum(weekly_total_tests_positive, na.rm = TRUE),
              weekly_tests_performed_7ma=sum(weekly_tests_performed_7ma, na.rm = TRUE),
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

########################################################################################################################################################
        # Correcting values for this week and last week
########################################################################################################################################################


# these are the dates for this week
start_this_week <- format(n_minus_eight, "%b %d")
end_this_week <- format(n_minus_two, "%b %d")
label_this_week <- paste0(start_this_week,"-",end_this_week)

# recalculate the PT numbers for this week in case a PT doesn't report. For example, a PT will have a 5-day MA if it didn't report on days 6 and 7 (and 5 days worth of tests performed)
this_week <- SALT_complete %>%
  filter(Date <= n_minus_two & Date >= n_minus_eight) %>% #7 days of this week
  group_by(Jurisdiction) %>%
  summarise(tests_performed_7ma=mean(tests_performed, na.rm = TRUE),
            total_tests_performed_this_week=sum(tests_performed, na.rm = TRUE),
            total_positive_tests_this_week=sum(positive_tests, na.rm = TRUE) ) %>%
  mutate(percent_positive_7ma = total_positive_tests_this_week/total_tests_performed_this_week) %>%
  rename(tests_performed_7ma_this_week = tests_performed_7ma,
         percent_positive_7ma_this_week = percent_positive_7ma) %>%
  filter(Jurisdiction != "Canada")

# now that the PTs for last week have been recalculated, we need to recalculate the numbers for Canada again
this_week_canada <- this_week %>% 
  ungroup() %>%
  filter(Jurisdiction != "Canada") %>%
  summarise(tests_performed_7ma_this_week=sum(tests_performed_7ma_this_week, na.rm = TRUE),
            total_tests_performed_this_week=sum(total_tests_performed_this_week, na.rm = TRUE),
            total_positive_tests_this_week=sum(total_positive_tests_this_week, na.rm = TRUE) ,
            Jurisdiction = "Canada") %>%
  mutate(percent_positive_7ma_this_week = total_positive_tests_this_week/total_tests_performed_this_week) 

# put the national values back into the data set
this_week <- bind_rows(this_week, this_week_canada) %>%
  select(-total_positive_tests_this_week)

########################################################################################################################################################  

# these are the dates for last week
n_minus_nine <- n_minus_eight-1
n_minus_fifteen <- n_minus_nine-6

start_last_week <- format(n_minus_fifteen, "%b %d")
end_last_week <- format(n_minus_nine, "%b %d")
label_last_week <- paste0(start_last_week,"-",end_last_week)

# recalculate the PT numbers for last week in case a PT doesn't report. For example, a PT will have a 5-day MA if it didn't report on days 6 and 7 
last_week <- SALT_complete %>%
  filter(Date <= n_minus_nine & Date >= n_minus_fifteen) %>% #7 days of last week
  group_by(Jurisdiction) %>%
  summarise(tests_performed_7ma=mean(tests_performed, na.rm = TRUE),
            total_tests_performed_last_week=sum(tests_performed, na.rm = TRUE),
            total_positive_tests_last_week=sum(positive_tests, na.rm = TRUE) ) %>%
  mutate(percent_positive_7ma = total_positive_tests_last_week/total_tests_performed_last_week) %>%
  rename(tests_performed_7ma_last_week = tests_performed_7ma,
         percent_positive_7ma_last_week = percent_positive_7ma) %>%
  filter(Jurisdiction != "Canada")
  
# now that the PTs for last week have been recalculated, we need to recalculate the numbers for Canada again
last_week_canada <- last_week %>% 
  ungroup() %>%
  filter(Jurisdiction != "Canada") %>%
  summarise(tests_performed_7ma_last_week=sum(tests_performed_7ma_last_week, na.rm = TRUE),
            total_tests_performed_last_week=sum(total_tests_performed_last_week, na.rm = TRUE),
            total_positive_tests_last_week=sum(total_positive_tests_last_week, na.rm = TRUE) ,
            Jurisdiction = "Canada") %>%
  mutate(percent_positive_7ma_last_week = total_positive_tests_last_week/total_tests_performed_last_week) 

# put the national values back into the data set
last_week <- bind_rows(last_week, last_week_canada) %>%
  select(-total_positive_tests_last_week)

########################################################################################################################################################  

# combine both weeks together for the final table and calculate percent change            
weeks_combined <- this_week %>%
  left_join(last_week, by="Jurisdiction") %>%
  select(-total_tests_performed_this_week, -total_tests_performed_last_week) %>%
  PHACTrendR::factor_PT_west_to_east(size = "big", Canada_first = TRUE) %>%
  arrange(Jurisdiction) %>%
  mutate(change_in_tests=(tests_performed_7ma_this_week-tests_performed_7ma_last_week)/tests_performed_7ma_last_week,
         change_in_positivity=(percent_positive_7ma_this_week-percent_positive_7ma_last_week)/percent_positive_7ma_last_week) %>%
  mutate(tests_performed_7ma_this_week = round(tests_performed_7ma_this_week,digits = 1),
         tests_performed_7ma_last_week = round(tests_performed_7ma_last_week,digits = 1)) %>%
  select(Jurisdiction,tests_performed_7ma_this_week,tests_performed_7ma_last_week,change_in_tests,percent_positive_7ma_this_week,percent_positive_7ma_last_week,change_in_positivity) %>%  
  rename(!!paste0("Tests Performed (7MA) (",label_last_week,")") := tests_performed_7ma_last_week,
         !!paste0("Percent Positivity (7MA) (",label_last_week,")") := percent_positive_7ma_last_week,
         !!paste0("Tests Performed (7MA) (",label_this_week,")") := tests_performed_7ma_this_week,
         !!paste0("Percent Positivity (7MA) (",label_this_week,")") := percent_positive_7ma_this_week)

# mutate(percent_positive_7ma_last_week=ifelse(round(percent_positive_7ma_last_week,digits=4) < 0.001, percent(percent_positive_7ma_last_week,accuracy = 0.01), percent(percent_positive_7ma_last_week,accuracy = 0.1)),
#        percent_positive_7ma_this_week=ifelse(round(percent_positive_7ma_this_week,digits=4) < 0.001, percent(percent_positive_7ma_this_week,accuracy = 0.01), percent(percent_positive_7ma_this_week,accuracy = 0.1)),
#        change_in_positivity=ifelse(round(change_in_positivity,digits=4) < 0.001, percent(change_in_positivity,accuracy = 0.1), percent(change_in_positivity,accuracy = 0.1)),
#        change_in_tests=ifelse(round(change_in_tests,digits=4) < 0.001, percent(change_in_tests,accuracy = 0.1), percent(change_in_tests,accuracy = 0.1)),
#        tests_performed_7ma_this_week = number(tests_performed_7ma_this_week,big.mark = "," ,accuracy = 0.1),
#        tests_performed_7ma_last_week = number(tests_performed_7ma_last_week,big.mark = "," ,accuracy = 0.1) ) 


# data set for the national testing figure
figure <- SALT_complete %>%
  filter(Jurisdiction=="Canada")


########################################################################################################################################################  


##########
#Creating "key_" R variables for inclusion in the lab slide text, and summary slide of the .Rmd


can_current_lab_testing<-this_week_canada

can_last_week_lab_testing<-last_week_canada


key_Can_weekly_tests<-scales::comma(can_current_lab_testing$total_tests_performed_this_week)
key_Can_avg_tests_per_day<-scales::comma(can_current_lab_testing$tests_performed_7ma_this_week)
key_Can_weekly_perc_positive<-percent(can_current_lab_testing$percent_positive_7ma_this_week, accuracy = 0.1)

key_Can_avg_tests_change<-PHACTrendR::turn_num_to_percent_change((can_current_lab_testing$tests_performed_7ma_this_week-can_last_week_lab_testing$tests_performed_7ma_last_week)/can_last_week_lab_testing$tests_performed_7ma_last_week)
key_Can_weekly_perc_positive_change<-PHACTrendR::turn_num_to_percent_change((can_current_lab_testing$percent_positive_7ma_this_week-can_last_week_lab_testing$percent_positive_7ma_last_week)/can_last_week_lab_testing$percent_positive_7ma_last_week)

# this step doesn't work if a PT hasn't reported for 7 days or more
PTs_missing_lab_days_current_week<-SALT_complete %>%
  filter(Date <= n_minus_two & Date >= n_minus_eight) %>% #7 days of this week
  group_by(Jurisdiction) %>%
  mutate(missing = is.na(tests_performed)) %>%
  filter(missing == FALSE) %>%
  summarise(days_reported = n()) %>%
  filter(days_reported != 7) %>%
  recode_PT_names_to_small() %>%
  mutate(text_var=paste0(Jurisdiction," (",days_reported," days of reporting)")) %>%
  ungroup() %>%
  select(text_var)
  
# this step doesn't work if a PT hasn't reported for 7 days or more
any_PTs_missing_current_week_lab_days_flag<-(nrow(PTs_missing_lab_days_current_week)>0)
if (any_PTs_missing_current_week_lab_days_flag==TRUE){
  key_labtesting_table_footnote<-paste0("The following PTs did not report all 7 days in the current week: ",PHACTrendR::turn_char_vec_to_comma_list(PTs_missing_lab_days_current_week$text_var))
}

# For footnote on daily testing figure
PTs_missing_latest_lab_date<-SALT1b %>%
  group_by(Jurisdiction) %>%
  filter(Date==max(Date)) %>%
  ungroup %>%
  filter(Date < n_minus_two) %>%
  recode_PT_names_to_small(geo_variable = "Jurisdiction") %>%
  factor_PT_west_to_east() %>%
  arrange(Jurisdiction)%>%
  mutate(Date=format(Date, "%b %d")) %>%
  mutate(text_var=paste0(Jurisdiction, " (last reported: ",Date,")")) 

PTs_missing_latest_lab_date<- as.character(PTs_missing_latest_lab_date$text_var)

key_lab_update<-format(max(SALT$update_date), "%B %d")

any_PTs_missing_latest_lab_date_flag<-(length(PTs_missing_latest_lab_date)>0)

if (any_PTs_missing_latest_lab_date_flag==TRUE){
  key_PTs_missing_latest_lab_date<-PHACTrendR::turn_char_vec_to_comma_list(PTs_missing_latest_lab_date)
}

if(any_PTs_missing_latest_lab_date_flag==TRUE){
  key_lab_figure_footnote<-paste0("Note: recent lab testing values may be underestimated as the following PTs are not caught up on lab reporting: ",key_PTs_missing_latest_lab_date, ". ","\nUpdated daily (Sun-Thurs). Data as of ",key_lab_update,".")
} else {
  key_lab_figure_footnote<-paste0("Updated daily (Sun-Thurs). Data as of ",key_lab_update,". ")
}






########################################################################################################################################################  

#    OLD CODE for the national daily testing figure

# # Create dataset for daily testing figure 
# 
# SALT2a<-SALT %>%
#   select(-Latest.Update.Date,-update_date)%>%
#   mutate(Current_week = ifelse(date(Date) + 7 <= max(Date), "No", "Yes")) %>%
#   arrange(Jurisdiction, datetime)
# 
# SALT3a <- SALT2a %>%
#   group_by(Jurisdiction,Date) %>%
#   filter(datetime==max(datetime)) #gives most recent data from each PT
# 
# SALT4a <- SALT3a %>%
#   group_by(Jurisdiction) %>%
#   rename(daily_tests_performed=tests_performed,
#          daily_tests_positive=positive_tests) %>%
#   mutate(daily_tests_negative=daily_tests_performed-daily_tests_positive,
#          percent_positive=daily_tests_positive/daily_tests_performed)
# 
# National_Daily_a <- SALT4a %>%
#   select(Date, Jurisdiction, daily_tests_performed, daily_tests_positive, daily_tests_negative) %>%
#   group_by(Date) %>%
#   summarise(across(where(is.numeric),sum),
#             .groups="drop_last") %>%
#   mutate(Jurisdiction="Canada") %>%
#   mutate(percent_positive = daily_tests_positive/daily_tests_performed) %>%
#   arrange(Date)
# 
# max_lab_test_fig_date<-max(National_Daily_a$Date-1) #Gives N-2 data
# 
# #Question about whether or not the "daily tests performed" should be a 7dMA. I think in SAS right now it isn't.
# National_Daily <- National_Daily_a %>%
#   # mutate(tests_performed=rollmean(daily_tests_performed,k=7,fill=NA,align="right")) %>%
#   mutate(tests_performed=daily_tests_performed,
#          tests_performed_7MA=rollmean(tests_performed,k=7,fill=NA,align="right"),
#          percent_positive_7MA=rollmean(percent_positive,k=7,fill=NA,align="right")) %>%
#   select(Date,Jurisdiction,tests_performed,tests_performed_7MA,percent_positive, percent_positive_7MA)  %>%
#   filter(Date>"2021-01-23") %>%   # can remove this filter once ready to present historical lab testing data
#   filter(Date<=max_lab_test_fig_date)

























# Final dataset: date, jurisdiction, cumulative tests performed, 7dma tests, 7dma tests/100k, 7dma % positivity

# SALT_final<-SALT_complete %>%
#   arrange(Date) %>%
#   group_by(Jurisdiction) %>%
#   mutate(cumulative_tests=cumsum(tests_performed)) %>%
#   left_join(PHACTrendR::latest_can_pop, by="Jurisdiction") %>%
#   mutate(tests_performed_7ma_per_100k=round((tests_performed_7ma/Population)*100000,digits = 1),
#          tests_performed_7ma=round(tests_performed_7ma, digits = 1),
#          percent_positive_7ma=ifelse(is.na(percent_positive_7ma), 0, round(percent_positive_7ma*100, digits=2))) %>%
#   select(Date, Jurisdiction, cumulative_tests, tests_performed_7ma, tests_performed_7ma_per_100k, percent_positive_7ma)%>%
#   rename(`Seven day rolling percent positivity`=percent_positive_7ma,
#          `Cumulative tests`=cumulative_tests,
#          `Total tests performed, 7-day moving average`=tests_performed_7ma,
#          `Total tests performed per 100k population, 7-day moving average`=tests_performed_7ma_per_100k)



  
  
  
 



