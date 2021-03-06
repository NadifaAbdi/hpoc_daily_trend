############ NATIONAL CRUDE DATA ###################################################################################################################

#keeping certain variables, and filtering out missing age and missing earliestdate values
DISCOVER_hosp <- qry_cases_raw  %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20, hosp) %>%
  filter(agegroup10 != "unknown" & hosp=="yes") %>% 
  group_by(earliestdate, agegroup10,pt) %>%
  tally() %>%
  mutate(Jurisdiction = "Canada") %>%
  filter(!is.na(earliestdate)) %>%
  mutate(Jurisdiction=PHACTrendR::recode_PT_names_to_big(toupper(pt))) %>%
  mutate(Jurisdiction=ifelse(pt=="yt","Yukon",Jurisdiction)) %>% #for now, as Yukon is listed as YT in Discover, which is not an option in "recode_names_to_big"
  dplyr::rename(hosp = n) %>%
  PHACTrendR::factor_PT_west_to_east(size = "big")%>% #this help put the plot in order from west to east later. size=big is because the PT names are not abbreviated 
  select(-pt)
  
dummy_hosp_data<-expand.grid(earliestdate=tidyr::full_seq(DISCOVER_hosp$earliestdate,1),
                        Jurisdiction=unique(DISCOVER_hosp$Jurisdiction),
                        agegroup10=unique(DISCOVER_hosp$agegroup10),
                        hosp=0)

DISCOVER_hosp2<-bind_rows(DISCOVER_hosp, dummy_hosp_data)%>%
  group_by(earliestdate,Jurisdiction,agegroup10)%>%
  summarise(hosp=sum(hosp),
            .groups="drop_last") %>%
  group_by(Jurisdiction, agegroup10)%>%
  arrange(earliestdate)%>%
  mutate(hosp_7ma=rollmean(hosp, 7, na.pad = TRUE, align = "right")) %>%
  arrange(earliestdate,Jurisdiction, agegroup10) 
  
  
#get number of hosp in Canada
DISCOVER_hosp_national <- DISCOVER_hosp2%>%
  ungroup() %>%
  group_by(earliestdate, agegroup10) %>%
  summarise(hosp=sum(hosp),
            hosp_7ma=sum(hosp_7ma),
            .groups="drop_last") %>%
  mutate(Jurisdiction="Canada") %>%
  arrange(agegroup10, earliestdate) %>% #sort
  group_by(agegroup10) %>%
  mutate(agegroup10 = as.character(agegroup10)) %>%
  ungroup() %>%
  arrange(earliestdate,Jurisdiction,agegroup10)




# deaths
DISCOVER_deaths<-qry_cases_raw  %>%
  select(phacid, pt, earliestdate, agegroup10, agegroup20, coviddeath) %>%
  filter(agegroup20 != "unknown" & coviddeath=="yes") %>% 
  group_by(earliestdate, agegroup20,pt) %>%
  tally() %>%
  filter(!is.na(earliestdate)) %>%
  mutate(Jurisdiction=PHACTrendR::recode_PT_names_to_big(toupper(pt))) %>%
  dplyr::rename(deaths = n) %>%
  select(-pt)


dummy_death_data<-expand.grid(earliestdate=tidyr::full_seq(DISCOVER_deaths$earliestdate,1),
                              Jurisdiction=unique(DISCOVER_deaths$Jurisdiction),
                              agegroup20=unique(DISCOVER_deaths$agegroup20),
                              deaths=0)

DISCOVER_deaths2<-bind_rows(DISCOVER_deaths, dummy_death_data)%>%
  group_by(earliestdate,Jurisdiction,agegroup20)%>%
  summarise(deaths=sum(deaths),
            .groups="drop_last") %>%
  group_by(Jurisdiction, agegroup20)%>%
  arrange(earliestdate)%>%
  mutate(deaths_7ma=rollmean(deaths, 7, na.pad = TRUE, align = "right")) %>%
  arrange(earliestdate,Jurisdiction, agegroup20) 



#get number of deaths in Canada
DISCOVER_deaths_national <- DISCOVER_deaths2%>%
  ungroup() %>%
  group_by(earliestdate, agegroup20) %>%
  summarise(deaths=sum(deaths),
            deaths_7ma=sum(deaths_7ma),
            .groups="drop_last") %>%
  mutate(Jurisdiction="Canada") %>%
  arrange(agegroup20, earliestdate) %>% #sort
  group_by(agegroup20) %>%
  mutate(agegroup20 = as.character(agegroup20)) %>%
  ungroup() %>%
  arrange(earliestdate,Jurisdiction,agegroup20)



# ICU admissions
DISCOVER_icu <- qry_cases_raw  %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20, icu) %>%
  filter(agegroup10 != "unknown" & icu=="yes") %>% 
  group_by(earliestdate, agegroup10,pt) %>%
  tally() %>%
  mutate(Jurisdiction = "Canada") %>%
  filter(!is.na(earliestdate)) %>%
  mutate(Jurisdiction=PHACTrendR::recode_PT_names_to_big(toupper(pt))) %>%
  mutate(Jurisdiction=ifelse(pt=="yt","Yukon",Jurisdiction)) %>% #for now, as Yukon is listed as YT in Discover, which is not an option in "recode_names_to_big"
  dplyr::rename(icu = n) %>%
  PHACTrendR::factor_PT_west_to_east(size = "big")%>% #this help put the plot in order from west to east later. size=big is because the PT names are not abbreviated 
  select(-pt)

dummy_icu_data<-expand.grid(earliestdate=tidyr::full_seq(DISCOVER_icu$earliestdate,1),
                            Jurisdiction=unique(DISCOVER_icu$Jurisdiction),
                            agegroup10=unique(DISCOVER_icu$agegroup10),
                            icu=0)

DISCOVER_icu2<-bind_rows(DISCOVER_icu, dummy_icu_data)%>%
  group_by(earliestdate,Jurisdiction,agegroup10)%>%
  summarise(icu=sum(icu),
            .groups="drop_last") %>%
  group_by(Jurisdiction, agegroup10)%>%
  arrange(earliestdate)%>%
  mutate(icu_7ma=rollmean(icu, 7, na.pad = TRUE, align = "right")) %>%
  arrange(earliestdate,Jurisdiction, agegroup10) 


#get number of icu in Canada
DISCOVER_icu_national <- DISCOVER_icu2%>%
  ungroup() %>%
  group_by(earliestdate, agegroup10) %>%
  summarise(icu=sum(icu),
            icu_7ma=sum(icu_7ma),
            .groups="drop_last") %>%
  mutate(Jurisdiction="Canada") %>%
  arrange(agegroup10, earliestdate) %>% #sort
  group_by(agegroup10) %>%
  mutate(agegroup10 = as.character(agegroup10)) %>%
  ungroup() %>%
  arrange(earliestdate,Jurisdiction,agegroup10)


############ NATIONAL ADJUSTED DATA ###################################################################################################################

# Calculate national hosp per 100K 
Adjusted_national_hosp <- DISCOVER_hosp_national  %>%
  left_join(PHACTrendR::pt_pop10, by=c("Jurisdiction"="Jurisdiction", "agegroup10"="AgeGroup10")) %>%
  mutate(hosp_per = (hosp/Population10)*100000) %>%   #hosp per 100,000
  mutate(hosp_7ma_per = (hosp_7ma/Population10)*100000) %>%   #hosp per 100,000 (7MA)
  mutate(agegroup10 = ifelse(agegroup10 == "80 or plus", "80 plus", agegroup10))%>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")

# Compute national deaths per 100K 
Adjusted_national_deaths <- DISCOVER_deaths_national  %>%
  left_join(PHACTrendR::pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(deaths_per = (deaths/Population20)*100000,
         deaths_7ma_per = (deaths_7ma/Population20)*100000) %>% 
  mutate(agegroup20 = ifelse(agegroup20 == "80 or plus", "80 plus", agegroup20))%>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")

# Calculate national icu per 100K 
Adjusted_national_icu <- DISCOVER_icu_national  %>%
  left_join(PHACTrendR::pt_pop10, by=c("Jurisdiction"="Jurisdiction", "agegroup10"="AgeGroup10")) %>%
  mutate(icu_per = (icu/Population10)*100000) %>%   #icu per 100,000
  mutate(icu_7ma_per = (icu_7ma/Population10)*100000) %>%   #icu per 100,000 (7MA)
  mutate(agegroup10 = ifelse(agegroup10 == "80 or plus", "80 plus", agegroup10))%>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")

############ADJUSTED PT DATA ###################################################################################################################

#get the 6 major PTs we want for hosp
DISCOVER_hosp_big6 <- DISCOVER_hosp2 %>%
  filter(Jurisdiction %in% PHACTrendR::recode_PT_names_to_big(PHACTrendR::PTs_big6)) #this filter gets the major 6 PTs we want using the PHACTrendR::PTs_big6 function

# Calculate hosp per 100K for PTs
Adjusted_hosp_big6 <- DISCOVER_hosp_big6  %>%
  left_join(PHACTrendR::pt_pop10, by=c("Jurisdiction"="Jurisdiction", "agegroup10"="AgeGroup10")) %>%
  mutate(hosp_per = (hosp/Population10)*100000,
         hosp_7ma_per = (hosp_7ma/Population10)*100000) %>%
  mutate(agegroup10 = ifelse(agegroup10 == "80 or plus", "80 plus", agegroup10))%>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big") %>%
  ungroup()



#get the major PTs we want for deaths
DISCOVER_deaths_big6 <- DISCOVER_deaths2 %>%
  filter(Jurisdiction %in% PHACTrendR::recode_PT_names_to_big(PHACTrendR::PTs_big6))

# Calculate deaths per 100K for PTs
Adjusted_deaths_big6 <- DISCOVER_deaths_big6  %>%
  left_join(PHACTrendR::pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(deaths_per = (deaths/Population20)*100000,
         deaths_7ma_per = (deaths_7ma/Population20)*100000) %>%
  mutate(agegroup20 = ifelse(agegroup20 == "80 or plus", "80 plus", agegroup20))%>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big") %>%
  ungroup()



#get the 6 major PTs we want for icu
DISCOVER_icu_big6 <- DISCOVER_icu2 %>%
  filter(Jurisdiction %in% PHACTrendR::recode_PT_names_to_big(PHACTrendR::PTs_big6) | Jurisdiction=="Nova Scotia") #this filter gets the major 6 PTs we want using the PHACTrendR::PTs_big6 function

# Calculate icu per 100K for PTs
Adjusted_icu_big6 <- DISCOVER_icu_big6  %>%
  left_join(PHACTrendR::pt_pop10, by=c("Jurisdiction"="Jurisdiction", "agegroup10"="AgeGroup10")) %>%
  mutate(icu_per = (icu/Population10)*100000,
         icu_7ma_per = (icu_7ma/Population10)*100000) %>%
  mutate(agegroup10 = ifelse(agegroup10 == "80 or plus", "80 plus", agegroup10))%>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big") %>%
  ungroup()

############ ALL HOSP PLOTS ###################################################################################################################


cat('\n')  
cat("# Cases resulting in hospitalization by age (crude), Canada", "\n") 

### Plot for national crude hosp ###
ggplot(Adjusted_national_hosp, aes(x = earliestdate, y = hosp_7ma, colour = agegroup10)) +
  geom_line(size = 1.5) +
   scale_y_continuous("Number of reported hospitalizations \n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("2 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_national_hosp %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_national_hosp %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend()+
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Mon-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text=element_text(size=20),
    axis.title = element_text(size=26),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 26, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 22),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 

# ggsave("output/national crude hosp.png", width = 20, height = 10,dpi=300)


cat('\n')  
cat("# Cases resulting in hospitalization by age (population-adjusted), Canada", "\n") 

### Plot for national adjusted hosp ###
ggplot(Adjusted_national_hosp, aes(x = earliestdate, y = hosp_7ma_per, colour = agegroup10)) +
  geom_line(size = 1.5) +
  scale_y_continuous("Number of hospitalizations per 100,000\n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("2 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_national_hosp %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_national_hosp %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend()+
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Mon-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text=element_text(size=20),
    axis.title = element_text(size=26),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 22),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 
# ggsave("output/national adjusted hosp.png", width = 20, height = 10,dpi=300)

cat('\n')  
cat("# Cases resulting in hospitalization by age (population-adjusted), select PTs", "\n") 

### Plot for PT adjusted hosp ###
ggplot(Adjusted_hosp_big6, aes(x = earliestdate, y = hosp_7ma_per, colour = agegroup10)) +
  geom_line(size=1.5) +
  facet_wrap(~Jurisdiction, scales = "free") +
  scale_y_continuous("Number of hospitalizations per 100,000 \n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("3 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_hosp_big6 %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_hosp_big6 %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend()+
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Mon-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=26),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 26, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 
# ggsave("output/PT adjusted hosp.png", width = 20, height = 10,dpi=300)


############ ALL ICU PLOTS ###################################################################################################################


cat('\n')  
cat("# Cases resulting in ICU admission by age (crude), Canada", "\n") 

### Plot for national crude icu ###
ggplot(Adjusted_national_icu, aes(x = earliestdate, y = icu_7ma, colour = agegroup10)) +
  geom_line(size = 1.5) +
  scale_y_continuous("Number of ICU admissions \n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("2 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_national_icu %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_national_icu %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend()+
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Mon-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text=element_text(size=20),
    axis.title = element_text(size=26),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 26, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 22),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 

# ggsave("output/national crude icu.png", width = 20, height = 10,dpi=300)


cat('\n')  
cat("# Cases resulting in ICU admissions by age (population-adjusted), Canada", "\n") 

### Plot for national adjusted icu ###
ggplot(Adjusted_national_icu, aes(x = earliestdate, y = icu_7ma_per, colour = agegroup10)) +
  geom_line(size = 1.5) +
  scale_y_continuous("Number of  ICU admission per 100,000\n(7 day moving average)", labels = comma_format(accuracy = 0.1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("2 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_national_icu %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_national_icu %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
 PHACTrendR::scale_colour_trend()+
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Mon-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=26),
    axis.text=element_text(size=20),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 22),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 
# ggsave("output/national adjusted icu.png", width = 20, height = 10,dpi=300)

cat('\n')  
cat("# Cases resulting in ICU admissions by age (population-adjusted), select PTs", "\n") 

### Plot for PT adjusted icu ###
ggplot(Adjusted_icu_big6, aes(x = earliestdate, y = icu_7ma_per, colour = agegroup10)) +
  geom_line(size=1.5) +
  facet_wrap(~Jurisdiction, scales = "free") +
  scale_y_continuous("Number of ICU admission per 100,000\n(7 day moving average)", labels = comma_format(accuracy = 0.1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("3 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_icu_big6 %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_icu_big6 %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend() +
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nNote that SK data is not displayed due to non-reporting of ICU variable in case report forms. 
        \nUpdated Daily (Mon-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=26),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 26, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 
# ggsave("output/PT adjusted icu.png", width = 20, height = 10,dpi=300)

############ ALL DEATH PLOTS ###################################################################################################################

cat('\n')  
cat("# Cases resulting in death by age (crude), Canada", "\n") 

### Plot for national crude deaths ###
ggplot(Adjusted_national_deaths, aes(x = earliestdate, y = deaths_7ma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  scale_y_continuous("Number of deaths \n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("2 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_national_deaths %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_national_deaths %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend() +
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Mon-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=26),
    axis.text=element_text(size=20),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 

# ggsave("output/national crude deaths.png", width = 20, height = 10, dpi=300)

cat('\n')  
cat("# Cases resulting in death by age (population-adjusted), Canada", "\n") 

### Plot for national adjusted deaths ###
ggplot(Adjusted_national_deaths %>% filter(earliestdate >= "2020-06-01"), aes(x = earliestdate, y = deaths_7ma_per, colour = agegroup20)) +
  geom_line(size = 1.5) +
  scale_y_continuous("Number of deaths per 100,000\n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("2 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_national_deaths %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_national_deaths %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend() +
  guides(colour = guide_legend(override.aes = list(size=3), nrow=1))+
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Mon-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text=element_text(size=20),
    axis.title = element_text(size=26),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  )

cat('\n') 

# ggsave("output/national adjusted deaths.png", width = 20, height = 10, dpi=300)

cat('\n')  
cat("# Cases resulting in death by age (population-adjusted), select PTs", "\n") 

### Plot for PT adjusted deaths ###
# Deaths (Adjusted) Plot
ggplot(Adjusted_deaths_big6, aes(x = earliestdate, y = deaths_7ma_per, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(~Jurisdiction, scales = "free") +
  scale_y_continuous("Number of deaths per 100,000\n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("3 months"),
    labels = label_date("%b/%y")
  ) +
  geom_rect(aes(
    xmin = Adjusted_deaths_big6 %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_deaths_big6 %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.02, fill = "grey", inherit.aes = FALSE
  ) +
  PHACTrendR::scale_colour_trend() +
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Mon-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=26),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 26, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  )

# ggsave("output/PT adjusted deaths.png", width = 20, height = 10, dpi=300)







