df_int<-import_international_data()

countries_of_interest<-c("AUS","CAN","DNK","FRA","DEU","IRL","ISR","ZAF","GBR","USA")

int_cases <- df_int %>% 
  filter(iso_code %in% countries_of_interest) %>%
  select(date, location, population, new_cases, new_cases_smoothed, new_cases_smoothed_per_million) %>%
  filter(new_cases_smoothed_per_million != is.na(new_cases_smoothed_per_million)) %>%
  mutate(label = if_else(date == max(date), as.character(location), NA_character_))

int_deaths <- df_int %>%
  filter(iso_code %in% countries_of_interest) %>%
  select(date, location, population, new_deaths, new_deaths_smoothed, new_deaths_smoothed_per_million) %>%
  filter(new_deaths_smoothed_per_million != is.na(new_deaths_smoothed_per_million)) %>%
  mutate(label = if_else(date == max(date), as.character(location), NA_character_))

int_vaccinations<-df_int%>%
  filter(iso_code %in% countries_of_interest) %>%
  select(date, 
         location, 
         population, 
         total_vaccinations, 
         total_vaccinations_per_hundred, 
         people_vaccinated, 
         people_vaccinated_per_hundred, 
         people_fully_vaccinated, 
         people_fully_vaccinated_per_hundred, 
         new_vaccinations,
         new_vaccinations_smoothed, 
         new_vaccinations_smoothed_per_million) %>%
  mutate(people_vaccinated_percentage=people_vaccinated_per_hundred/100) %>%
  filter(!is.na(total_vaccinations))

cat('\n')  
cat("# Daily Cases by Country (7-day moving average, population adjusted)", "\n") 

# Plot cases
ggplot(int_cases, aes(date, new_cases_smoothed_per_million, group = location, colour = location)) +
         geom_line(size = 2) +
        # geom_text_repel(aes(label = label),
        #         size = 6,
        #         nudge_x = 1,
        #         nudge_y = 1,
        #         na.rm = TRUE
        # ) +
        scale_y_continuous("Daily cases per 1,000,000 population", expand = c(0, 0), limits = c(0, NA)) +
        scale_x_date("Date", 
                     breaks = scales::breaks_width("1 month"),
                     labels = label_date("%b-%Y")
                     ) +
        scale_colour_tableau(palette = "Tableau 10") +
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "right",
                legend.title = element_blank(),
                legend.key=element_blank(),
                legend.text = element_text(size = 26),
                legend.key.size = unit(3,"line"),
                text = element_text(size = 20),
                plot.caption = element_text(hjust = 0)
        ) +
        labs(caption = paste0("Source: Our World in Data, https://ourworldindata.org/coronavirus
                        \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(int_cases$date), "%B %d")," (International data is lagged by one day)"))


cat('\n') 

cat('\n')  
cat("# Daily Deaths by Country (7-day moving average, population adjusted)", "\n") 

#Plot deaths
ggplot(int_deaths, aes(date, new_deaths_smoothed_per_million, group = location, colour = location)) +
  geom_line(size = 2) +
  # geom_text_repel(aes(label = label),
  #         size = 6,
  #         nudge_x = 1,
  #         nudge_y = 1,
  #         na.rm = TRUE
  # ) +
  scale_y_continuous("Daily deaths per 1,000,000 population", expand = c(0, 0), limits = c(0, NA)) +
  scale_x_date("Date", 
               breaks = scales::breaks_width("1 month"),
               labels = label_date("%b-%Y")
  ) +
  scale_colour_tableau(palette = "Tableau 10") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  ) +
  labs(caption = paste0("Source: Our World in Data, https://ourworldindata.org/coronavirus
                        \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(int_deaths$date), "%B %d")," (International data is lagged by one day)"))

cat('\n') 


# 
# # exploring a possible vaccination chart
# 
# cat('\n')  
# cat("# Vaccination by Country (percent vaccinated, any dose)", "\n") 
# 
# #Plot vaccinations
# ggplot(int_vaccinations, aes(date, people_vaccinated_percentage, group = location, colour = location)) +
#   geom_line(size = 2) +
#   # geom_text_repel(aes(label = label),
#   #         size = 6,
#   #         nudge_x = 1,
#   #         nudge_y = 1,
#   #         na.rm = TRUE
#   # ) +
#   scale_y_continuous("Percent vaccinated (any dose)", 
#                      label=label_percent()) +
#   scale_x_date("Date", 
#                breaks = scales::breaks_width("1 month"),
#                labels = label_date("%b-%Y")
#   ) +
#   scale_colour_tableau(palette = "Tableau 10") +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(colour = "black"),
#     legend.position = "right",
#     legend.title = element_blank(),
#     legend.key=element_blank(),
#     legend.text = element_text(size = 26),
#     legend.key.size = unit(3,"line"),
#     text = element_text(size = 20),
#     plot.caption = element_text(hjust = 0)
#   ) +
#   labs(caption = paste0("Source: Our World in Data, https://ourworldindata.org/coronavirus
#                         \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(int_deaths$date), "%B %d")," (International data is lagged by one day)"))
# 
# cat('\n') 
# 
# 
# cat('\n')  
# cat("# International comparison of cases, deaths, and vaccination coverage", "\n") 
# 
# 
# ## Facetted plot
# 
# facetted_plot_countries<-c("Canada","Israel","United States")
# 
# int_combined_data<-int_cases %>%
#   filter(location %in% facetted_plot_countries) %>%
#   left_join(int_deaths,by=c("date"="date","location"="location")) %>%
#   left_join(int_vaccinations,by=c("date"="date","location"="location")) %>%
#   select(-starts_with("label"),-starts_with("population")) %>%
#   select(date, location, new_cases_smoothed_per_million,new_deaths_smoothed_per_million, people_vaccinated_per_hundred)%>%
#   pivot_longer(cols=c(new_cases_smoothed_per_million, new_deaths_smoothed_per_million, people_vaccinated_per_hundred),
#                names_to="metric",
#                values_to="value") %>%
#   mutate(metric=case_when(metric=="new_cases_smoothed_per_million" ~ "New cases per million (7MA)",
#                           metric=="new_deaths_smoothed_per_million" ~ "New deaths per million (7MA)",
#                           metric=="people_vaccinated_per_hundred" ~ "Percent vaccinated (any dose)"))
# 
# 
# ggplot(int_combined_data, aes(date, value, group = location, colour = location)) +
#   geom_line(size = 2) +
#   facet_grid(rows=vars(metric), scales = "free_y")+
#   # geom_text_repel(aes(label = label),
#   #         size = 6,
#   #         nudge_x = 1,
#   #         nudge_y = 1,
#   #         na.rm = TRUE
#   # ) +
#   scale_y_continuous("", expand = c(0, 0)) +
#   scale_x_date("Date", 
#                breaks = scales::breaks_width("2 month"),
#                labels = label_date("%b-%Y")
#   ) +
#   scale_colour_tableau(palette = "Tableau 10") +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.border = element_rect(colour = "black", fill=NA),
#     axis.line = element_line(colour = "black"),
#     legend.position = "right",
#     legend.title = element_blank(),
#     legend.key=element_blank(),
#     legend.text = element_text(size = 26),
#     legend.key.size = unit(3,"line"),
#     text = element_text(size = 16),
#     plot.caption = element_text(hjust = 0)
#   ) +
#   labs(caption = paste0("Source: Our World in Data, https://ourworldindata.org/coronavirus
#                         \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(int_deaths$date), "%B %d")," (International data is lagged by one day)"))
# 
# cat('\n') 
# 
# 
# 
# 













### For summary bullets slide:

top_3_cases<-int_cases %>%
  filter(date==max(date)) %>%
  arrange(desc(new_cases_smoothed_per_million)) %>%
  head(3) %>%
  select(location, new_cases_smoothed_per_million) %>%
  mutate(text_var= paste0(location, " (",comma(new_cases_smoothed_per_million),")"))

key_int_top_3_cases<-PHACTrendR::turn_char_vec_to_comma_list(top_3_cases$text_var)


top_3_deaths<-int_deaths %>%
  filter(date==max(date)) %>%
  arrange(desc(new_deaths_smoothed_per_million)) %>%
  head(3) %>%
  select(location, new_deaths_smoothed_per_million) %>%
  mutate(text_var= paste0(location, " (",number(new_deaths_smoothed_per_million,accuracy = 0.01),")"))

key_int_top_3_deaths<-PHACTrendR::turn_char_vec_to_comma_list(top_3_deaths$text_var)
