jurisdiction <- if (Sys.getenv("age_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")

qry_cases_raw<-PHACTrendR::import_DISCOVER_data()

# code for when wanting to switch over to Metabase method
# note - this requires some setup with the keyring package: 
# https://www.r-bloggers.com/2019/06/how-to-hide-a-password-in-r-with-the-keyring-package/#:~:text=The%20keyring%20package%20is%20a,plaintext%20in%20an%20R%20script.

# think about how best to include metabase_user automation - probably can take advantage of Sys.getenv("USERNAME") and store a datatable with
# people's usernames and their metabase usernames to automatically grab metabase_user

# metabase_user<-""
# metabase_pass<-keyring::key_get("Metabase",metabase_user)
# 
# qry_cases_raw<-PHACTrendR::import_DISCOVER_data(method="metabaser",
#                                                 metabase_user = metabase_user,
#                                                 metabase_pass = metabase_pass)



qry_canada <- qry_cases_raw %>%
  janitor::clean_names() %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20) %>%
  filter(!is.na(age)) %>%
  group_by(earliestdate, agegroup20) %>%
  tally() %>%
  mutate(Jurisdiction = "Canada") %>%
  filter(!is.na(earliestdate))

qry_cases <- qry_cases_raw %>%
  janitor::clean_names() %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20) %>%
  mutate(Jurisdiction = toupper(pt)) %>%
  recode_PT_names_to_big() %>%
  group_by(earliestdate, agegroup20, Jurisdiction) %>%
  dplyr::tally() %>%
  dplyr::filter(!is.na(earliestdate)) %>%
  dplyr::bind_rows(qry_canada) %>%
  filter(Jurisdiction %in% c("Canada", recode_PT_names_to_big(PHACTrendR::PTs_big6))) %>%
  factor_PT_west_to_east(Canada_first=TRUE, size="big") %>%
  dplyr::rename(cases = n)


# Filter province
qry_crude_filter <- qry_cases %>%
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

qry_crude_filter$Jurisdiction <- recode(qry_crude_filter$Jurisdiction, "Canada"="")

qry_crude_filter <- qry_crude_filter %>% 
  filter(earliestdate >= "2020-06-01") %>% 
  mutate(agegroup20 = ifelse(agegroup20 == "80 or plus", "80 plus", agegroup20))


# Plot Crude Cases (Canada)
ggplot(qry_crude_filter, aes(x = earliestdate, y = sdma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(vars(Jurisdiction), scales = "free_y") +
  scale_y_continuous("Number of cases \n(7 day moving average)", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = "6 weeks",
    limits=c(min(qry_crude_filter$earliestdate), max(qry_crude_filter$earliestdate)),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = qry_crude_filter %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = qry_crude_filter %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","gold","#9B59B6")) +
  guides(colour = guide_legend(override.aes = list(size=3)))+
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nThe earliest of the following dates were used as the date of illness onset: symptom onset date, specimen collection date,
    laboratory testing date, date reported to province or territory, or date reported to PHAC.
    \nUpdated Daily (Sun-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
    plot.caption = element_text(hjust = 0,size=20),
    text = element_text(size = 26)
  )
