#07 - VOC analyses


#load libraries from trend_report.rmd

gs4_deauth()

sheet_URL<-"https://docs.google.com/spreadsheets/d/1ssDpnmunPjPBP-Z87s787KZLtdBkQPkFgbFvFnG5hq8"


screening_classification_info<-read_sheet(ss = sheet_URL,sheet = "screening") %>%
  select(free_text_field_upper, screening_classification)

sequencing_classification_info<-read_sheet(ss = sheet_URL,sheet = "sequencing") %>%
  select(free_text_field_upper, sequencing_classification)


VOC_qry_cases_raw <-  readRDS("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/CaseReportForm/trendvoc_extract.rds"  ) %>%
  dplyr::mutate(onsetdate = as.Date(onsetdate),
                episodedate = as.Date(episodedate),
                earliestlabcollectiondate = as.Date(earliestlabcollectiondate),
                earliestdate = as.Date(earliestdate)) %>%
  dplyr::rename(age = age_years)

sort(unique(VOC_qry_cases_raw$variantscreenresult))
sort(unique(VOC_qry_cases_raw$variantsequenceresult))



VOC_data<-VOC_qry_cases_raw %>%
 mutate(variantscreenresult=toupper(variantscreenresult),
        variantsequenceresult=toupper(variantsequenceresult)) %>%
  left_join(screening_classification_info, by=c("variantscreenresult"="free_text_field_upper")) %>%
  left_join(sequencing_classification_info, by=c("variantsequenceresult"="free_text_field_upper")) %>%
  mutate(screening_classification=ifelse(is.na(variantscreenresult),"NOT_COLLECTED",screening_classification),
         sequencing_classification=ifelse(is.na(variantsequenceresult),"NOT_COLLECTED",sequencing_classification))
