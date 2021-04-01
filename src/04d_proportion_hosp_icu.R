library(PHACTrendR)
library(dplyr)
library(zoo)
library(scales)
library(ggplot2)
library(ggthemes)
library(lubridate)


qry_cases_raw<-PHACTrendR::import_DISCOVER_data()




##################### Delete above code once integrated in trend report


##################### Looking at data quality first


hosp_by_PT<-qry_cases_raw %>%
  group_by(pt) %>%
  summarise(`Hospitalized: Yes`=sum(hosp=="yes", na.rm=TRUE),
            `Hospitalized: No`=sum(hosp=="no", na.rm=TRUE),
            `Hospitalized: Unknown`=sum(hosp=="unknown", na.rm=TRUE),
            `Hospitalized: Blank` =sum(is.na(hosp))) %>%
  rename(Jurisdiction=pt) %>%
  mutate(Jurisdiction=toupper(Jurisdiction)) %>%
  recode_PT_names_to_big() %>%
  filter(!Jurisdiction=="REPATRIATE") %>%
  factor_PT_west_to_east(size="big") %>%
  arrange(Jurisdiction)

icu_by_PT<-qry_cases_raw %>%
  group_by(pt) %>%
  summarise(`ICU: Yes`=sum(icu=="yes", na.rm=TRUE),
            `ICU: No`=sum(icu=="no", na.rm=TRUE),
            `ICU: Unknown`=sum(icu=="unknown", na.rm=TRUE),
            `ICU: Blank` =sum(is.na(icu))) %>%
  rename(Jurisdiction=pt) %>%
  mutate(Jurisdiction=toupper(Jurisdiction)) %>%
  recode_PT_names_to_big() %>%
  filter(!Jurisdiction=="REPATRIATE") %>%
  factor_PT_west_to_east(size="big") %>%
  arrange(Jurisdiction)


# The numbers of missing hosp and ICU are quite large, especially for ON and QC. 



create_proportion_dataset<-function(type="", missing_as_no=FALSE){
  
    data<-qry_cases_raw %>%
      select(phacid, earliestdate, age, agegroup10, agegroup20, as.name(type)) %>%
      filter(agegroup10 != "unknown") %>%
      filter(if (missing_as_no==FALSE)  !!as.name(type) %in% c("yes","no") else TRUE) %>% # this line looks to see if `missing_as_no` is set to TRUE, if it isn't, we exclude missing values 
      group_by(earliestdate, agegroup10) %>%
      summarise(yes_outcome=sum(!!as.name(type)=="yes", na.rm=TRUE),
                no_outcome=sum(!!as.name(type) %in% c("no","unknown") | is.na(!!as.name(type))),
                .groups="drop_last") 
    
    dummy_data<-expand.grid(earliestdate=tidyr::full_seq(data$earliestdate,1),
                                agegroup10=unique(data$agegroup10),
                                yes_outcome=0,
                                no_outcome=0)
    
    data_2<-bind_rows(data, dummy_data)%>%
      group_by(earliestdate,agegroup10)%>%
      summarise(yes_outcome=sum(yes_outcome),
                no_outcome=sum(no_outcome),
                .groups="drop_last") %>%
      ungroup() %>%
      arrange(earliestdate)%>%
      group_by(agegroup10) %>%
      mutate(yes_outcome_7day_sum=rollsumr(yes_outcome, 7, na.pad = TRUE),
             no_outcome_7day_sum=rollsumr(no_outcome, 7, na.pad=TRUE),
             total_7day_sum=yes_outcome_7day_sum+no_outcome_7day_sum,
             proportion_7day=yes_outcome_7day_sum/total_7day_sum) %>%
      mutate(proportion_7day=ifelse(yes_outcome_7day_sum==0, 0, proportion_7day)) %>%
      arrange(earliestdate, agegroup10) %>%
      filter(earliestdate>="2020-03-31")
    

return(data_2)  
}

plot_7MA<-function(type=""){
  
data<-create_proportion_dataset(type=type)

if(type=="hosp"){
  event_name<-"hospitalization"
} else if (type=="icu"){
  event_name<-"ICU"  
}


  ggplot(data, aes(x=earliestdate, y=proportion_7day, colour=agegroup10))+
    geom_line(size=1.5)+
    scale_y_continuous(paste0("Proportion of cases resulting in ",event_name),
                       labels=label_percent()) +
    scale_x_date(
      "Date of illness onset",
      breaks = ("month"),
      labels = label_date("%d%b")) +
    geom_rect(aes(xmin = max(data$earliestdate)-14,
                  xmax = max(data$earliestdate),
                  ymin = -Inf,
                  ymax = Inf),
              alpha = 0.01, fill = "grey", inherit.aes = FALSE) +
    scale_color_tableau()+
    guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
    labs(caption = paste0("* Shaded area represents approximate lag in reporting")) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 26, face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 16),
      legend.key.size = unit(3, "line"),
      text = element_text(size = 20),
      plot.caption = element_text(hjust = 0)
    )
             
}

plot_7MA(type="hosp")
plot_7MA(type="icu")



plot_monthly<-function(type=""){
  data<-create_proportion_dataset(type=type) %>%
    select(earliestdate, agegroup10, yes_outcome, no_outcome)%>%
    mutate(total_outcome=yes_outcome+no_outcome,
           earliestdate_month=lubridate::floor_date(earliestdate, unit="month")) %>%
    group_by(earliestdate_month, agegroup10) %>%
    summarise(yes_outcome=sum(yes_outcome),
              no_outcome=sum(no_outcome),
              total_outcome=sum(total_outcome),
              .groups="drop_last") %>%
    mutate(proportion_outcome=yes_outcome/total_outcome,
           proportion_outcome=ifelse(is.na(proportion_outcome),0,proportion_outcome))
    
  
  if(type=="hosp"){
    event_name<-"hospitalization"
  } else if (type=="icu"){
    event_name<-"ICU"  
  }
  
  
  ggplot(data, aes(x=earliestdate_month, y=proportion_outcome, colour=agegroup10))+
    geom_line(size=1.5)+
    scale_y_continuous(paste0("Proportion of cases resulting in ",event_name),
                       labels=label_percent()) +
    scale_x_date(
      "Date of illness onset",
      breaks = ("month"),
      labels = label_date("%d%b")) +
    geom_rect(aes(xmin = max(data$earliestdate_month)-14,
                  xmax = max(data$earliestdate_month),
                  ymin = -Inf,
                  ymax = Inf),
              alpha = 0.01, fill = "grey", inherit.aes = FALSE) +
    scale_color_tableau()+
    guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
    labs(caption = paste0("* Shaded area represents approximate lag in reporting")) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 26, face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 16),
      legend.key.size = unit(3, "line"),
      text = element_text(size = 20),
      plot.caption = element_text(hjust = 0)
    )
}

plot_monthly(type="hosp")
plot_monthly(type="icu")

plot_weekly<-function(type="", age_cat_filter=FALSE){
  
  data<-create_proportion_dataset(type=type) %>%
    select(earliestdate, agegroup10, yes_outcome, no_outcome)%>%
    mutate(total_outcome=yes_outcome+no_outcome,
           earliestdate_week=lubridate::floor_date(earliestdate, unit="week")) %>%
    filter(earliestdate>="2020-12-13")%>%
    filter(if (age_cat_filter==TRUE) agegroup10 %in% c("20 to 29","30 to 39") else TRUE) %>%
    group_by(earliestdate_week, agegroup10) %>%
    summarise(yes_outcome=sum(yes_outcome),
              no_outcome=sum(no_outcome),
              total_outcome=sum(total_outcome),
              .groups="drop_last") %>%
    mutate(proportion_outcome=yes_outcome/total_outcome,
           proportion_outcome=ifelse(is.na(proportion_outcome),0,proportion_outcome))
  
  
  if(type=="hosp"){
    event_name<-"hospitalization"
  } else if (type=="icu"){
    event_name<-"ICU"  
  }
  
  
  ggplot(data, aes(x=earliestdate_week, y=proportion_outcome, colour=agegroup10))+
    geom_line(size=1.5)+
    scale_y_continuous(paste0("Proportion of cases resulting in ",event_name),
                       labels=label_percent()) +
    scale_x_date(
      "Date of illness onset",
      breaks = ("month"),
      labels = label_date("%d%b")) +
    geom_rect(aes(xmin = max(data$earliestdate_week)-14,
                  xmax = max(data$earliestdate_week),
                  ymin = -Inf,
                  ymax = Inf),
              alpha = 0.01, fill = "grey", inherit.aes = FALSE) +
    scale_color_tableau()+
    guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
    labs(caption = paste0("* Shaded area represents approximate lag in reporting")) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 26, face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 16),
      legend.key.size = unit(3, "line"),
      text = element_text(size = 20),
      plot.caption = element_text(hjust = 0)
    )
}

plot_weekly(type="hosp")
plot_weekly(type="icu")
plot_weekly(type="hosp", age_cat_filter = TRUE)
plot_weekly(type="icu", age_cat_filter = TRUE)
