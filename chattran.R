# enrollment data
# september 15, 2021
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

setwd("~/Documents/Trellis/CRM-15002/data")

chattran <- read.csv("chattran.csv", header = TRUE, na.strings=c("","NA"))

# recode developer name
# chattran$DeveloperName = recode_factor(chattran$LiveChatDeployment.DeveloperName, 
                                       # "X24_7_Chat" = "24/7",
                                       # "University_Services_Chat" = "Other",
                                       # "Think_Tank_Chat" = "Other",
                                       # "Study_Abroad_Chat" = "Other",
                                       # "SOS_Chat" = "SOS",
                                       # "SECD_Chat" = "Other",
                                       # "Registrar_Chat" = "Registrar",
                                       # "Psych_Dept_Chat" = "Other",
                                       # "OSFA_Chat" = "OSFA", 
                                       # "LifeLab_Chat" = "Other",
                                       # "College_of_Engineering_Chat" = "Other")
### time stamps
# date_value3 = chattran$CreatedDate
# day_of_the_week3 = date(date_value3) %>%
#   wday()
# # day_of_the_week
# 
# chattran = dplyr::mutate(chattran, day_of_the_week3)
# chattran$day_of_the_week <- recode_factor(chattran$day_of_the_week, 
#                                           "1" = "Sunday", 
#                                           "2" = "Monday",
#                                           "3" = "Tuesday",
#                                           "4" = "Wednesday",
#                                           "5" = "Thursday",
#                                           "6" = "Friday",
#                                           "7" = "Saturday")

# check that the date time is in the right time zone
# ymd_hms("2020-10-08T18:06:08.000Z")
# table(chat2021$CreatedDate)
Date_Time3 = ymd_hms(chattran$CreatedDate)
# # head(Date_Time) #2020-07-02 21:56:42 UTC
# chattran <- dplyr::mutate(chattran, Date_Time3)
# 
# # change to AZ time 
# t3 <- as.POSIXct(Date_Time3, tz = "GMT")
# attributes(t3)$tzone
# AZ_time3 <- lubridate::with_tz(t3, "MST")
# 
# chattran <- dplyr::mutate(chattran, AZ_time3)

# # convert time to decimals 
# tm3.dechr <- hour(chattran$AZ_time) + minute(chattran$AZ_time)/60 + second(chattran$AZ_time)/3600
# # tm1.dechr
# 
# chattran <- dplyr::mutate(chattran, tm3.dechr) 
chattran <- chattran %>%
     mutate(AZ_date3 = as.Date(AZ_time3))
# chattran <- subset(chattran, select = -c(1))

# write.csv(chattran, file = "chattran.csv")

