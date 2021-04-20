#07 - VOC analyses


#load libraries from trend_report.rmd

gs4_deauth()
library(keyring)


sheet_URL<-"https://docs.google.com/spreadsheets/d/1ssDpnmunPjPBP-Z87s787KZLtdBkQPkFgbFvFnG5hq8"

metabase_user<-"michael.elten@canada.ca"
metabase_pass<-keyring::key_get("Metabase",metabase_user)

screening_classification_info<-read_sheet(ss = sheet_URL,sheet = "screening") %>%
  select(free_text_field_upper, screening_classification)

sequencing_classification_info<-read_sheet(ss = sheet_URL,sheet = "sequencing") %>%
  select(free_text_field_upper, sequencing_classification)



#~27 seconds
system.time(
qry_cases_raw <-  PHACTrendR::import_DISCOVER_data()
)

#~177 seconds
system.time(
  qry_cases<-PHACTrendR::import_DISCOVER_data(method="metabaser",metabase_user=metabase_user,metabase_pass = metabase_pass)
)
#~18 seconds
system.time(
  VOC_data<-PHACTrendR::import_VOC_data(metabase_user=metabase_user,metabase_pass = metabase_pass)
)
#0.2 seconds
system.time(
  linked_data<-qry_cases %>%
    left_join(VOC_data, by="phacid")
)

sort(unique(qry_cases_raw$variantscreenresult))
sort(unique(qry_cases_raw$variantsequenceresult))



VOC_data<-qry_cases_raw %>%
 mutate(variantscreenresult=toupper(variantscreenresult),
        variantsequenceresult=toupper(variantsequenceresult)) %>%
  left_join(screening_classification_info, by=c("variantscreenresult"="free_text_field_upper")) %>%
  left_join(sequencing_classification_info, by=c("variantsequenceresult"="free_text_field_upper")) %>%
  mutate(screening_classification=ifelse(is.na(variantscreenresult),"NOT_COLLECTED",screening_classification),
         sequencing_classification=ifelse(is.na(variantsequenceresult),"NOT_COLLECTED",sequencing_classification))




NML_linelist_data<-read.csv(file.choose())

### EDA

summary(as.factor(VOC_data$screening_classification))
summary(as.factor(VOC_data$sequencing_classification))

unclassified_screening<-VOC_data %>%
  filter(is.na(screening_classification))

unclassified_sequencing<-VOC_data %>%
  filter(is.na(sequencing_classification))


(PTs_reporting_DISCOVER<-VOC_data %>%
  filter(!screening_classification=="NOT_COLLECTED" | !sequencing_classification=="NOT_COLLECTED") %>%
  group_by(pt) %>%
  count())

PTs_reporting_NML<-NML_linelist_data%>%
  group_by(PT)%>%
  count()


### Visualization

recognized_VOCs<-c("B.1.1.7","B.1.351","P.1")

# stacked epi curve, by VOC

VOC_data %>%
  filter(sequencing_classification %in% recognized_VOCs) %>%
  ggplot(aes(x=earliestdate))+
  geom_bar(aes(fill=sequencing_classification),position = "stack",width = 1)+
  geom_rect(aes(xmin = max(VOC_data$earliestdate) - days(14),
                xmax = max(VOC_data$earliestdate),
                ymin = -Inf,
                ymax = Inf),
            alpha = 0.01, fill = "grey", inherit.aes = FALSE)+ 
  theme_trend(text_size = 12)+
  scale_fill_trend()


#proportion of cases in each age group, by VOC

VOC_data %>%
  group_by(sequencing_classification, agegroup20) %>%
  summarise(total=n(),
            .groups="drop_last") %>%
  mutate(variant_total=sum(total),
         variant_proportion=total/variant_total) %>%
  ggplot(aes(x=agegroup20, y=variant_proportion))+
  geom_bar(stat="identity", aes(fill=sequencing_classification),position = "dodge")+
  # facet_grid(rows=vars(sequencing_classification))+
  theme_trend(text_size = 12)+
  scale_fill_trend()
             


