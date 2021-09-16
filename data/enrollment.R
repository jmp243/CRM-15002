# enrollment data
# September 16, 2021
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

enroll <- read.csv("active_enroll_did.csv", header = TRUE, na.strings=c("","NA"))

#
enroll$class_standing = recode_factor(enroll$Class_Standing__c, 
                                            "Freshman" = "Freshman",
                                            "Sophomore" = "Sophomore",
                                            "Junior" = "Junior",
                                            "Senior" = "Senior",
                                            "Graduate" = "Graduate",
                                            "Doctoral" = "Graduate",
                                            "Masters" = "Graduate",
                                            "Prof 1" = "Graduate",
                                            "Prof 2" = "Graduate",
                                            "Prof 3" = "Graduate",
                                            "Prof 4" = "Graduate")

# join data
left_enroll <- left_join(chattran, enroll, by = "new_ID") # created a ton of duplicates
# left join produced 44,888

left_enroll = mutate(left_enroll, Graduate = (class_standing == 'Graduate')) #yay, this works
left_enroll$Students <- as.factor(left_enroll$Graduate)
left_enroll$Students <- recode_factor(left_enroll$Students, "TRUE" = "Graduate", "FALSE" = "Undergraduate") # this worked

# prune the data and remove redundant variables
left_enroll <- subset(left_enroll, select = -c(Graduate, X, day_of_the_week3))


