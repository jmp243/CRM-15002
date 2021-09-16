# Jung Mee Park
# CRM-15002 
# September 14, 2021
# cases did
# https://jira.arizona.edu/browse/CRM-15002

library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(tibble)
library(tidyverse)
library(ggstatsplot)
library(lubridate)
library(forcats)
library(purrr)
# read in data
setwd("~/Documents/Trellis/CRM-15002/data")

# cases_did <- read.csv("cases_did.csv", header = TRUE, na.strings=c("","NA"))

# table(cases_did$RecordTypeId)
# # try to filter the data
# # cases_type <- cases_did %>%
# #   dplyr::filter(cases_did, RecordTypeId == "0122S0000002Tl3QAE")
# cases_type <- mutate(cases_did, RecordType = (RecordTypeId == '0122S0000002Tl3QAE'))
# 
# cases_type2 <- subset(cases_type, RecordType == TRUE)
# write.csv(cases_type2, file = "cases_type2.csv")

cases_type2 <- read.csv("cases_type2.csv", header = TRUE, na.strings=c("","NA"))

# changing date and time from UTC to AZ time
# date_value4 = cases_type2$CreatedDate
# 
# Date_Time4 = ymd_hms(cases_type2$CreatedDate)
# # head(Date_Time) #2020-07-02 21:56:42 UTC
# cases_type2 <- dplyr::mutate(cases_type2, Date_Time4)
# 
# # change to AZ time 
# t4 <- as.POSIXct(Date_Time4, tz = "GMT")
# attributes(t4)$tzone
# AZ_time4 <- lubridate::with_tz(t4, "MST")
# 
# cases_type2 <- dplyr::mutate(cases_type2, AZ_time4)
# 
# # convert time to decimals 
# tm4.dechr <- hour(cases_type2$AZ_time4) + minute(cases_type2$AZ_time4)/60 + 
#   second(cases_type2$AZ_time4)/3600
cases_type2 <- cases_type2 %>% 
   mutate(AZ_date4 = as.Date(AZ_time4))
# 
# cases_type2 <- subset(cases_type2, select = -c(1, 2))
# write.csv(cases_type2, file = "cases_type2.csv")

# summary statistics
summary(cases_type2)
table(cases_type2$Origin) # 8 categories

# create histogram

p3 <- cases_type2 %>% 
  drop_na(Origin) %>% 
  ggplot(aes(x=AZ_date4, fill = Origin)) +
  geom_histogram(binwidth = 5) + 
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Date", y="Count")

p3 <- p3 + scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                        date_labels = "%b-%y") + labs(title = "Histogram of Cases over Time", 
                                                      subtitle = "full data", fill = "Origin")
p3

### 
