#need to first run the libraries in the trend report.rmd file

#import qry_allcases dataset 
qry_cases_raw<-PHACTrendR::import_DISCOVER_data()



############ NATIONAL CRUDE DATA ###################################################################################################################

#keeping certain variables, and filtering out missing age and missing earliestdate values #help: SK is missing a bunch of dates
DISCOVER_hosp <- qry_cases_raw  %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20, hosp) %>%
  filter(!is.na(age)&hosp=="yes") %>%
  group_by(earliestdate, agegroup20,pt) %>%
  tally() %>%
  mutate(Jurisdiction = "Canada") %>%
  filter(!is.na(earliestdate)) %>%
  mutate(Jurisdiction=PHACTrendR::recode_PT_names_to_big(toupper(pt))) %>%
  dplyr::rename(hosp = n) %>%
  PHACTrendR::factor_PT_west_to_east(size = "big") #this help put the plot in order from west to east later. size=big is because the PT names are not abbreviated

#get number of hosp in Canada
DISCOVER_hosp_national <- DISCOVER_hosp%>%
  ungroup() %>%
  group_by(earliestdate, agegroup20) %>%
  tally() %>%
  mutate(Jurisdiction="Canada") %>%
  dplyr::rename(hosp = n)

# Filter out missing values and calculate crude hosp (national) 
Crude_hosp_national <- DISCOVER_hosp_national %>%
  mutate(earliestdate = as.Date(earliestdate)) %>%
  filter(!is.na(earliestdate)) %>%
  arrange(agegroup20, earliestdate) %>% #sort
  group_by(agegroup20) %>%
  mutate(sdma = rollmean(hosp, 7, na.pad = TRUE, align = "right")) %>%   #hosp 7MA
  mutate(agegroup20 = as.character(agegroup20)) %>%
  filter(agegroup20 != "Unknown") %>%
  filter(agegroup20 != "NaN") %>%
  filter(agegroup20 != "unknown") %>%
  filter(agegroup20 != "") %>%
  ungroup()

# deaths
DISCOVER_deaths<-qry_cases_raw  %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20, coviddeath) %>%
  filter(!is.na(age)&coviddeath=="yes") %>%
  group_by(earliestdate, agegroup20,pt) %>%
  tally() %>%
  filter(!is.na(earliestdate)) %>%
  mutate(Jurisdiction=PHACTrendR::recode_PT_names_to_big(toupper(pt))) %>%
  dplyr::rename(deaths = n)

#get number of deaths in Canada
DISCOVER_deaths_national<-DISCOVER_deaths%>%
  ungroup() %>%
  group_by(earliestdate, agegroup20) %>%
  tally() %>%
  mutate(Jurisdiction="Canada") %>%
  dplyr::rename(deaths = n)

# Filter out missing values and calculate crude deaths (for PTs)
Crude_deaths_national <- DISCOVER_deaths_national %>%
  mutate(earliestdate = as.Date(earliestdate)) %>%
  filter(!is.na(earliestdate)) %>%
  arrange(agegroup20, earliestdate) %>%
  group_by(agegroup20) %>%
  mutate(sdma = rollmean(deaths, 7, na.pad = TRUE, align = "right")) %>%   #deaths 7MA
  mutate(agegroup20 = as.character(agegroup20)) %>%
  filter(agegroup20 != "Unknown") %>%
  filter(agegroup20 != "NaN") %>%
  filter(agegroup20 != "unknown") %>%
  filter(agegroup20 != "") %>%
  ungroup()



############ NATIONAL ADJUSTED DATA ###################################################################################################################

# Calculate national hosp per 100K 
Adjusted_national_hosp <- Crude_hosp_national  %>%
  left_join(PHACTrendR::pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(hosp_per = (hosp/Population20)*100000) %>%   #hosp per 100,000
  mutate(sdma_per = rollmean(hosp_per, 7, na.pad = TRUE, align = "right")) %>%   #hosp per 100,000 (7MA)
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")

# Compute national deaths per 100K 
Adjusted_national_deaths <- Crude_deaths_national  %>%
  left_join(PHACTrendR::pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(deaths_per = (deaths/Population20)*100000) %>%
  mutate(sdma_per = rollmean(deaths_per, 7, na.pad = TRUE, align = "right")) %>% 
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")



############ADJUSTED PT DATA ###################################################################################################################

#get the 6 major PTs we want for hosp
DISCOVER_hosp_big6 <- DISCOVER_hosp %>%
  filter(Jurisdiction %in% PHACTrendR::recode_PT_names_to_big(PHACTrendR::PTs_big6)) #this filter gets the major 6 PTs we want using the PHACTrendR::PTs_big6 function

# Filter out missing values and calculate crude hosp (for PTs)
hosp_crude_filter_big6 <- DISCOVER_hosp_big6 %>%
  mutate(earliestdate = as.Date(earliestdate)) %>%
  filter(!is.na(earliestdate)) %>%
  arrange(Jurisdiction, agegroup20, earliestdate) %>%
  group_by(Jurisdiction, agegroup20) %>%
  mutate(sdma = rollmean(hosp, 7, na.pad = TRUE, align = "right")) %>%
  mutate(agegroup20 = as.character(agegroup20)) %>%
  filter(agegroup20 != "Unknown") %>%
  filter(agegroup20 != "NaN") %>%
  filter(agegroup20 != "unknown") %>%
  filter(agegroup20 != "") %>%
  ungroup()

# Calculate hosp per 100K for PTs
Adjusted_hosp_big6 <- hosp_crude_filter_big6  %>%
  left_join(PHACTrendR::pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(hosp_per = (hosp/Population20)*100000) %>%
  mutate(sdma_per = rollmean(hosp_per, 7, na.pad = TRUE, align = "right")) %>% 
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")



#get the major PTs we want for deaths
DISCOVER_deaths_big6 <- DISCOVER_deaths %>%
  filter(Jurisdiction %in% PHACTrendR::recode_PT_names_to_big(PHACTrendR::PTs_big6))

# Filter out missing values and calculate crude deaths (for PTs)
deaths_crude_filter_big6 <- DISCOVER_deaths_big6 %>%
  mutate(earliestdate = as.Date(earliestdate)) %>%
  filter(!is.na(earliestdate)) %>%
  arrange(Jurisdiction, agegroup20, earliestdate) %>%
  group_by(Jurisdiction, agegroup20) %>%
  mutate(sdma = rollmean(deaths, 7, na.pad = TRUE, align = "right")) %>%     #deaths 7MA
  mutate(agegroup20 = as.character(agegroup20)) %>%
  filter(agegroup20 != "Unknown") %>%
  filter(agegroup20 != "NaN") %>%
  filter(agegroup20 != "unknown") %>%
  filter(agegroup20 != "") %>%
  ungroup()

# Compute deaths per 100K for PTs
qry_deaths_per_big6 <- deaths_crude_filter_big6  %>%
  left_join(PHACTrendR::pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(deaths_per = (deaths/Population20)*100000) %>%
  mutate(sdma_per = rollmean(deaths_per, 7, na.pad = TRUE, align = "right")) %>% 
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")



############ ALL HOSP PLOTS ###################################################################################################################

### Plot for national crude hosp ###
ggplot(Crude_hosp_national %>% filter(earliestdate >= "2020-06-01"), aes(x = earliestdate, y = sdma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(vars(Jurisdiction), scales = "free_y") +
  scale_y_continuous("Number of reported hospitalizations, 7 Day moving average", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = Crude_hosp_national %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Crude_hosp_national %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Sun-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
  )


### Plot for national adjusted hosp ###
ggplot(Adjusted_national_hosp, aes(x = earliestdate, y = sdma_per, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(~Jurisdiction, scales = "free") +
  scale_y_continuous("Number of reported hospitalizations per 100,000\n(7 Day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = Adjusted_national_hosp %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_national_hosp %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Sun-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
  )


### Plot for PT adjusted hosp ###
ggplot(Adjusted_hosp_big6, aes(x = earliestdate, y = sdma_per, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(~Jurisdiction, scales = "free") +
  scale_y_continuous("Number of reported hospitalizations per 100,000\n(7 Day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = Adjusted_hosp_big6 %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Adjusted_hosp_big6 %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Sun-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
  )




############ ALL DEATH PLOTS ###################################################################################################################

### Plot for national crude deaths ###
ggplot(Crude_deaths_national %>% filter(earliestdate >= "2020-06-01"), aes(x = earliestdate, y = sdma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(vars(Jurisdiction), scales = "free_y") +
  scale_y_continuous("Number of reported deaths, 7 Day moving average", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = Crude_deaths_national %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Crude_deaths_national %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Sun-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
  )

### Plot for national adjusted deaths ###
ggplot(Crude_deaths_national %>% filter(earliestdate >= "2020-06-01"), aes(x = earliestdate, y = sdma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(vars(Jurisdiction), scales = "free_y") +
  scale_y_continuous("Number of reported deaths, 7 Day moving average", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = Crude_deaths_national %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = Crude_deaths_national %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Sun-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
  )

### Plot for PT adjusted deaths ###
# Deaths (Adjusted) Plot
ggplot(qry_deaths_per_big6, aes(x = earliestdate, y = sdma_per, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(~Jurisdiction, scales = "free") +
  scale_y_continuous("Number of reported deaths per 100,000\n(7 Day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = qry_deaths_per_big6 %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = qry_deaths_per_big6 %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Sun-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
  )










#Comment out for now because only adjusted PT is needed
########### HOSP CRUDE PT PLOT ####################################################################################################################


# Plot for PT crude hosp
# ggplot(hosp_crude_filter_big6 %>% filter(earliestdate >= "2020-06-01"), aes(x = earliestdate, y = sdma, colour = agegroup20)) +
#   geom_line(size = 1.5) +
#   facet_wrap(vars(Jurisdiction), scales = "free_y") +
#   scale_y_continuous("Number of reported hospitalizations, 7 Day moving average", labels = comma_format(accuracy = 1)) +
#   scale_x_date(
#     "Date of illness onset",
#     breaks = scales::breaks_width("6 weeks"),
#     labels = label_date("%d%b")
#   ) +
#   geom_rect(aes(
#     xmin = hosp_crude_filter_big6 %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
#     xmax = hosp_crude_filter_big6 %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
#     ymin = -Inf,
#     ymax = Inf
#   ),
#   alpha = 0.01, fill = "grey", inherit.aes = FALSE
#   ) +
#   scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
#   #scale_colour_wsj() +
#   labs(caption = paste0(
#     "* Shaded area represents approximate lag in reporting
#     \nUpdated Daily (Sun-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(colour = "black"),
#     strip.background = element_blank(),
#     strip.text = element_text(hjust = 0, size = 26, face = "bold"),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.key=element_blank(),
#     legend.text = element_text(size = 26),
#     legend.key.size = unit(3,"line"),
#     text = element_text(size = 20),
#     plot.caption = element_text(hjust = 0)
#   )

########### DEATHS CRUDE PT PLOT ####################################################################################################################
#comment out for now because only adjusted PT is needed

# Plot for PT crude deaths
# ggplot(deaths_crude_filter_big6 %>% filter(earliestdate >= "2020-06-01"), aes(x = earliestdate, y = sdma, colour = agegroup20)) +
#   geom_line(size = 1.5) +
#   facet_wrap(vars(Jurisdiction), scales = "free_y") +
#   scale_y_continuous("Number of reported deaths, 7 Day moving average", labels = comma_format(accuracy = 1)) +
#   scale_x_date(
#     "Date of illness onset",
#     breaks = scales::breaks_width("6 weeks"),
#     labels = label_date("%d%b")
#   ) +
#   geom_rect(aes(
#     xmin = deaths_crude_filter_big6 %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
#     xmax = deaths_crude_filter_big6 %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
#     ymin = -Inf,
#     ymax = Inf
#   ),
#   alpha = 0.01, fill = "grey", inherit.aes = FALSE
#   ) +
#   scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
#   #scale_colour_wsj() +
#   labs(caption = paste0(
#     "* Shaded area represents approximate lag in reporting
#     \nUpdated Daily (Sun-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(colour = "black"),
#     strip.background = element_blank(),
#     strip.text = element_text(hjust = 0, size = 26, face = "bold"),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.key=element_blank(),
#     legend.text = element_text(size = 26),
#     legend.key.size = unit(3,"line"),
#     text = element_text(size = 20),
#     plot.caption = element_text(hjust = 0)
#   )
