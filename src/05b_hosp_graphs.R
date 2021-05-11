#if missing dataset, import here (05a is not run in Chief Science report)
if (!exists('all_hosp_data')){
  all_hosp_data<-PHACTrendR::import_hosp_data()
}

# Filter province

max_hosp_date_all_PTs<-all_hosp_data %>%
  group_by(Date) %>%
  summarise(total_PTs=n(),
            .groups="drop_last") %>%
  filter(total_PTs==30)%>% #13PTs+travellers and national number *2
  filter(Date==max(Date)) %>%
  select(Date)%>%
  pull()

pt_hosp_icu_filter <- all_hosp_data %>%
    filter(Date >= "2020-04-01" & !Jurisdiction=="Repatriated Travellers") %>%
    group_by(Jurisdiction) %>%
    filter(Date <= max_hosp_date_all_PTs) %>% # to prevent dip from AB
    mutate(Jurisdiction=as.character(Jurisdiction)) %>%
    factor_PT_west_to_east(size="big")

cat('\n')  
cat("# COVID-19 patients in hospital daily across Canada", "\n") 

# Plot National
ggplot(pt_hosp_icu_filter %>% filter(Jurisdiction=="Canada"), aes(Date, cases, colour = type)) +
    geom_line(size = 2) +
    facet_wrap(vars(Jurisdiction), scales = "free_y") +
    scale_x_date(
        NULL,
        breaks = scales::breaks_width("1 month"),
        labels = label_date("%b/%y")
    ) +
    scale_y_continuous(
        "Number of cases",
        labels = comma_format(accuracy = 1)
    ) +
    scale_color_manual(labels = c("Total hospitalizations", "Total ICU"), values = c("darkblue", "red")) +
  guides(colour = guide_legend(override.aes = list(size=3)))+
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 26),
        legend.key.size = unit(3,"line"),
        text = element_text(size = 20),
        plot.caption = element_text(hjust = 0)) +
    labs(caption = paste0("Source: Provincial and territorial website data. \nNote: Hospitalization values are up to ", format(max_hosp_date_all_PTs, "%B %d")," as this is the last date with data from all PTs.",
                          "\nUpdated daily (Sun-Thurs). Data as of: ",format(max(all_hosp_data$Date), "%B %d")))

cat('\n') 

cat('\n')  
cat("# COVID-19 patients in hospital daily, select PTs", "\n") 


# Plot by PT


pt_hosp_icu_filter_big_6<-pt_hosp_icu_filter %>% 
  filter(Jurisdiction %in% PHACTrendR::recode_PT_names_to_big(PHACTrendR::PTs_big6))

ggplot(pt_hosp_icu_filter_big_6, aes(Date, cases, colour = type)) +
  geom_line(size = 2) +
  facet_wrap(vars(Jurisdiction), scales = "free") +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("3 months"),
    labels = label_date("%b/%y")
  ) +
  scale_y_continuous(
    "Number of cases",
    labels = comma_format(accuracy = 1)
  ) +
  scale_color_manual(labels = c("Total hospitalizations", "Total ICU"), values = c("darkblue", "red")) +
  guides(colour = guide_legend(override.aes = list(size=3)))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 26, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  ) +
  labs(caption = paste0("Source: Provincial and territorial website data. 
                        \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(all_hosp_data$Date), "%B %d")))

cat('\n') 
