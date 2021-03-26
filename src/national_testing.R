#national testing figure attempt in R

#need to rescale percent positive to be close to tests performed in order to plot on same axis in R

#scaling factor allows us to plot percent positivity next to number of tests. increase if you want percent positive to be higher on the graph
scaling_factor<-1800000

National_Daily$percent_positive_rescaled<-National_Daily$percent_positive_7MA*scaling_factor
ggplot(data=National_Daily)+
  # geom_area(aes(x=Date, y=tests_performed_7MA,colour="lightblue"), alpha=0.8, fill="lightblue") +
  geom_bar(aes(x=Date,y=tests_performed, colour="lightblue"),stat="identity",fill="lightblue")+
  geom_line(aes(x=Date, y=percent_positive_rescaled,colour="red"),size=1.25)+
  scale_y_continuous(name = "Number of tests",
                     labels = label_number(big.mark = ","),
                     breaks=seq(0,150000,25000),
                     sec.axis = sec_axis(~./(scaling_factor/100), name = "Percent Positive (%)"))+
  scale_x_date(breaks = ("month"),
               labels = label_date("%b %Y"),
               expand = c(0, 0))+
  scale_colour_manual(name = "",
                      values =c('lightblue'='lightblue','red'='red'), labels = c('Number of tests','Percent positive'))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid=element_blank(),
        plot.caption = element_text(hjust = 0,size=20),
        legend.position = "bottom",
        axis.text = element_text(size=20),
        axis.title = element_text(size=26),
        legend.text = element_text(size=20))

  

## Alternative graph - using facet_wrap to compare lab testing numbers and % positive without plotting on same graph
# 
label_tests_performed<-"Daily tests performed"
label_tests_performed_7MA<-"7 day average of tests performed"
label_percent_positive_7MA<-"7 day average of percent positivity (%)"

National_Daily_long<-National_Daily %>%
  select(Date, Jurisdiction, tests_performed, tests_performed_7MA, percent_positive, percent_positive_7MA) %>%
  mutate(percent_positive=percent_positive*100,
         percent_positive_7MA=percent_positive_7MA*100) %>%
  pivot_longer(cols = c(tests_performed:percent_positive_7MA),
               names_to ="metric",
               values_to="value") %>%
  mutate(time_unit=ifelse(str_detect(metric, "7MA"), "7MA","daily"),
         metric=ifelse(str_detect(metric,"tests"),"Tests performed","Percent positivity"))

ggplot(data=National_Daily_long, aes(x=Date, y=value))+
  geom_bar(data=subset(National_Daily_long,metric=="Tests performed"&time_unit=="daily"),stat="identity",fill="lightblue")+
  geom_line(data=subset(National_Daily_long,metric=="Tests performed"&time_unit=="7MA"),colour="darkblue",size=1.25)+
  geom_point(data=subset(National_Daily_long,metric=="Percent positivity"&time_unit=="daily"),colour="pink")+
  geom_line(data=subset(National_Daily_long,metric=="Percent positivity"&time_unit=="7MA"),colour="red",size=1.25)+
  facet_grid(rows=vars(metric),
             scales = "free_y",
             switch = "y")+
  scale_x_date(breaks = ("month"),
               labels = label_date("%b %y"),
               expand = c(0, 0))+
  scale_y_continuous(name="",labels=scales::label_comma())+
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid=element_blank(),
        plot.caption = element_text(hjust = 0,size=20),
        legend.position = "bottom",
        axis.text = element_text(size=20),
        axis.title = element_text(size=26),
        legend.text = element_text(size=20),
        strip.background = element_blank(),
        strip.text=element_text(size=rel(1.2)),
        strip.placement = "outside")

