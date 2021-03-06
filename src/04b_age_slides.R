jurisdiction <- if (Sys.getenv("age_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")

# Filter province
qry_cases_filter <- qry_cases %>%
    filter(Jurisdiction %in% jurisdiction) %>%
    mutate(earliestdate = as.Date(earliestdate)) %>%
    filter(!is.na(earliestdate)) %>%
    arrange(Jurisdiction, agegroup20, earliestdate) %>%
    group_by(Jurisdiction, agegroup20) %>%
    mutate(sdma = rollmean(cases, 7, na.pad = TRUE, align = "right")) %>%
    mutate(agegroup20 = as.character(agegroup20)) %>%
    filter(agegroup20 != "Unknown") %>%
    filter(agegroup20 != "NaN") %>%
    filter(agegroup20 != "unknown") %>%
    filter(agegroup20 != "") %>%
    ungroup() 

# Compute cases per 100K

qry_cases_per <- qry_cases_filter %>%
  left_join(pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(cases_per = (cases/Population20)*100000) %>%
  mutate(sdma_per = rollmean(cases_per, 7, na.pad = TRUE, align = "right")) %>%
  mutate(agegroup20 = ifelse(agegroup20 == "80 or plus", "80 plus", agegroup20))%>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")

# qry_cases_per$Jurisdiction <- recode(qry_cases_per$Jurisdiction, "Canada"="", "British Columbia"="BC","Alberta"="AB","Saskatchewan"="SK","Manitoba"="MB","Quebec"="QC","Ontario"="ON")

# Plot
plot<-ggplot(qry_cases_per, aes(x = earliestdate, y = sdma_per, colour = agegroup20)) +
    geom_line(size = 1.5) +
    facet_wrap(~Jurisdiction, scales = "free") +
    scale_y_continuous("Number of cases per 100,000\n(7 day moving average)", labels = comma_format(accuracy = 1)) +
    scale_x_date(
        "Date of illness onset",
        breaks = scales::breaks_width("2 months"),
        labels = label_date("%b/%y")
    ) +
    geom_rect(aes(
        xmin = qry_cases_filter %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
        xmax = qry_cases_filter %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
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
        axis.text=element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 26, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 26),
        legend.key.size = unit(3,"line"),
        plot.caption = element_text(hjust = 0, size=20)
    )
if (jurisdiction[1]=="Canada"){
  plot<-plot+
    theme(strip.text=element_blank(),
          axis.text = element_text(size=20))
}
plot



### For summary bullets

if (jurisdiction=="Canada"){
  key_age_case_rates<-qry_cases_per %>%
    filter(earliestdate==Sys.Date()-14) %>%
    arrange(desc(sdma_per)) %>%
    select(agegroup20, sdma_per) %>%
    mutate(text_var=paste0("those ",agegroup20, " (",number(sdma_per,accuracy=0.1),")"))
  
  key_age_case_highest<-key_age_case_rates %>%
    head(1) %>%
    select(text_var)
  
  key_age_case_others<-key_age_case_rates %>%
    tail(nrow(key_age_case_rates)-1) %>%
    select(text_var)


}

