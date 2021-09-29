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

# install.packages("TraMinr")
# library(TraMinr)
# read in data
setwd("~/Documents/Trellis/CRM-15002/data")

enroll <- read.csv("active_enroll_did.csv", header = TRUE, na.strings=c("","NA"))

left_enroll <- read.csv("left_enroll2.csv", header = TRUE, na.strings=c("","NA"))

left_cases <- read.csv("left_cases.csv", header = TRUE, na.strings=c("","NA"))

# time as POSIXct
# left_enroll <- left_enroll %>% 
#   # group_by(new_ID) %>% 
#   mutate(AZ_time = as.POSIXct(AZ_time3, format = "%y/%m/%d %H:%M:%S"))

# new periods
# nrows = nrow(left_cases)
# period = rep(0, nrows)
# period[which(left_cases$AZ_time4 < "2020-08-06")] = "Summer 2020"			 
# period[which(left_cases$AZ_time4 >= "2020-08-06" & 
#                left_cases$AZ_time4 <= "2020-09-07")] = "Peak Fall 2020"
# period[which(left_cases$AZ_time4 >= "2020-09-08" & 
#                left_cases$AZ_time4 <= "2020-12-31")] = "Non-Peak Fall 2020"
# period[which(left_cases$AZ_time4 >= "2021-01-01" & 
#                left_cases$AZ_time4 <= "2021-02-01")] = "Peak Spring 2021" 
# period[which(left_cases$AZ_time4 >= "2021-02-02" & 
#                left_cases$AZ_time4 <= "2021-05-31")] = "Non-Peak Spring 2021"
# period[which(left_cases$AZ_time4 >= "2021-06-01" & 
#                left_cases$AZ_time4 <= "2021-08-06")] = "Summer 2021"
# period[which(left_cases$AZ_time4 >= "2021-08-07" & 
#                left_cases$AZ_time4 <= "2021-09-14")] = "Peak Fall 2021"
# 
# left_cases$period = period                         #attach new column to dataset
nrows = nrow(left_cases)
period = rep(0, nrows)
period[which(left_cases$AZ_time4 < "2020-08-06")] = "Summer 2020"			 
period[which(left_cases$AZ_time4 >= "2020-08-06" & 
               left_cases$AZ_time4 <= "2020-09-07")] = "Peak Fall 2020"
period[which(left_cases$AZ_time4 >= "2020-09-07" & 
               left_cases$AZ_time4 <= "2020-12-31")] = "Non-Peak Fall 2020"
period[which(left_cases$AZ_time4 >= "2020-12-31" & 
               left_cases$AZ_time4 <= "2021-02-01")] = "Peak Spring 2021" 
period[which(left_cases$AZ_time4 >= "2021-02-01" & 
               left_cases$AZ_time4 <= "2021-05-31")] = "Non-Peak Spring 2021"
period[which(left_cases$AZ_time4 >= "2021-05-31" & 
               left_cases$AZ_time4 <= "2021-08-06")] = "Summer 2021"
period[which(left_cases$AZ_time4 >= "2021-08-06" & 
               left_cases$AZ_time4 <= "2021-09-15")] = "Peak Fall 2021"

left_cases$period = period  
table(left_cases$period)
# enroll$class_standing = recode_factor(enroll$Class_Standing__c, 
                                            # "Freshman" = "Freshman",
                                            # "Sophomore" = "Sophomore",
                                            # "Junior" = "Junior",
                                            # "Senior" = "Senior",
                                            # "Graduate" = "Graduate",
                                            # "Doctoral" = "Graduate",
                                            # "Masters" = "Graduate",
                                            # "Prof 1" = "Graduate",
                                            # "Prof 2" = "Graduate",
                                            # "Prof 3" = "Graduate",
                                            # "Prof 4" = "Graduate")

# # join data
# left_enroll <- left_join(chattran, enroll, by = "new_ID") # created a ton of duplicates
# # left join produced 44,888
# 
# left_enroll = mutate(left_enroll, Graduate = (class_standing == 'Graduate')) #yay, this works
# left_enroll$Students <- as.factor(left_enroll$Graduate)
# left_enroll$Students <- recode_factor(left_enroll$Students, "TRUE" = "Graduate", "FALSE" = "Undergraduate") # this worked
# 
# # prune the data and remove redundant variables
# left_enroll <- subset(left_enroll, select = -c(Graduate, X.x, day_of_the_week3))
# left_enroll <- subset(left_enroll, select = -c(X.y))
# 
# write.csv(left_enroll, file = "left_enroll2.csv")

# make sure times are read in correctly 
# t3 <- as.POSIXct(Date_Time3, tz = "GMT")
# attributes(t3)$tzone
# AZ_time3 <- lubridate::with_tz(t3, "MST")

# left_enroll <- dplyr::mutate(left_enroll, AZ_time3)
left_enroll <- left_enroll %>% 
  mutate(AZ_date3 = as.Date(AZ_time3))

### using the left_enroll data to create graphs
# change new_ID into a factor
left_enroll$new_ID <- as.factor(left_enroll$new_ID)

length(unique(left_enroll[["new_ID"]])) #17725

# subset data for origin
# left_cases <- left_join(cases_type2, enroll, by = "new_ID") # created a ton of duplicates

# left_cases = mutate(left_cases, Graduate = (class_standing == 'Graduate')) #yay, this works
# left_cases$Students <- as.factor(left_cases$Graduate)
# left_cases$Students <- recode_factor(left_cases$Students, "TRUE" = "Graduate", "FALSE" = "Undergraduate") # this worked

left_cases <- subset(left_cases, select = -c(X.1, X))

left_cases <- left_cases %>% group_by(new_ID) %>% 
  mutate(COUNT = n()) 
left_cases <- left_cases %>% 
  group_by(new_ID) %>% 
  # arrange(AZ_time) %>%
  # select(c(new_ID, AZ_time, CaseNumber, Department__c, Origin, COUNT)) %>%
  mutate(AZ_time = as.POSIXlt(AZ_time4, tz = "",
                              tryFormats = c("%Y-%m-%d %H:%M:%OS"), 
                              optional = FALSE)) %>% 
  mutate(seqnum = 1:length(new_ID))

# remove duplicates
left_cases <- left_cases %>%
  arrange(-CaseNumber) %>%
  filter(duplicated(CaseNumber) == FALSE) #down to 145212

write.csv(left_cases, file = "left_cases.csv")
# 
# see unique cases for left_cases
length(unique(left_cases[["new_ID"]])) # 47506

## time series data
# subset the data
# left_chat <- dplyr::filter(left_cases, Origin=="Chat")
# left_email <- dplyr::filter(left_cases, Origin=="Email")
# left_person <- dplyr::filter(left_cases, Origin=="In Person")
# left_offline <- dplyr::filter(left_cases, Origin=="Offline Chat")
# left_phone <- dplyr::filter(left_cases, Origin=="Phone")
# left_text <- dplyr::filter(left_cases, Origin=="Text")
# left_webform <- dplyr::filter(left_cases, Origin=="Webform")
# left_zoom <- dplyr::filter(left_cases, Origin=="Zoom")

# find mean usage
# unique new_ID with different origins
# install.packages("astsa")
library(astsa)

# ts(left_cases, frequency = 4, start = c(2020, 2021))
# origin_cases <- left_cases %>%
#     mutate(origin1 = ifelse(Origin == "Chat",
#                           NA,
#                           Origin))


# ## create next origin
# uniqueCodes = unique(left_cases[["new_ID"]])
# 
# OriginNext = rep(NA, nrows)
# 
# for (i in 1:length(uniqueCodes)){
#   inds = which(left_cases$new_ID==uniqueCodes[i])
#   lenInds = length(inds)
#   
#   if (lenInds > 1) { 
#     for (row in 1:(lenInds-1))
#       OriginNext[inds[row]] = left_cases$newOrigin[inds[row+1]]
#   }
  
  # if (left_cases$AZ_time4[inds[lenInds]]=="2021-09-14 20:35:36") {
  #   OriginNext[inds[lenInds]] = "limit"
  # } else {
  #   OriginNext[inds[lenInds]] = "end"
  # }
# }
# left_cases$OriginNext = OriginNext
# left_cases = left_cases[which(left_cases$OriginNext != "limit"), ]
# ###############################
# ## initial origin
# newOrigin = rep(0, nrows)
# newOrigin[which(left_cases$Origin == left_cases$OriginNext)] = "sym"        #all cases with same
# newOrigin[which(left_cases$Origin != left_cases$OriginNext)] = "asym"       #all cases with different
# # newOrigin[which(xor(left_cases$Origin == NA, left_cases$OriginNext == NA))] = "x"  #all cases in which one but not both are 0
# left_cases$newOrigin = newOrigin   
##
### try using tidyverse
# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/

# change time to as.POSIXct
# origin_cases <- left_cases %>% 
#   # group_by(new_ID) %>% 
#   mutate(time = as.POSIXct(AZ_time4, format = "%y/%m/%d %H:%M:%S"))

# left_cases %>%
#   # na.omit() %>%
#   group_by(new_ID) %>% 
#   ggplot(aes(x = AZ_time4, y = Origin))+
#   geom_point(color = "darkorchid4") 
# # + 
#   facet_wrap( ~ Created_Day_of_Week__c)

# try a Bubble plot
library(plotly)
# fig <- plot_ly(origin_cases, x = ~AZ_date4, y = ~Origin, text = ~ new_ID, type = 'scatter', mode = 'markers',
#                marker = list(size = ~ Origin, opacity = 0.5))
# fig

# y-axis is categorigcal for time series
# https://stackoverflow.com/questions/29060491/how-to-create-a-time-series-analysis-where-y-axis-are-categorical-variable
# library(xts)
# # Convert categorical to some numeric with a mapping:
# OriginNext <- c("Chat" = 1, "Email" = 2, "Phone" = 3, "Zoom" = 4, 
#                 "Webform" = 5, "In Person" = 6, "Offline Chat" = 7, "Text" = 8)
# values1 <- OriginNext[as.character(left_cases$Origin)]

# tidyverse
# sample 100 
# create a multidimensional
# ids, type of action, date/time
# multiple plots

# sample of ID
# to what degree would it be useful
# transitions might be more
# it seems like overall trends in each of the origins
# single student used within two months
# pull out a sample 
set.seed(123)
index  <- sample(1:nrow(left_cases), 5)
# 
left_cases[index,]

# or use sample
# sample_data <- origin_case2 %>% dplyr::slice_sample(n=244) #code stopped working

# # Convert durations to time:
# origin1 <- "2020-05-02 08:01:55"
# duration2 <- as.POSIXct (sprintf("2020-05-02 08:01:55", index$AZ_time4)) - 
#   as.POSIXct("2021-09-14 13:35:36")
# time1 <- as.POSIXct( c(0, cumsum(as.numeric(duration2))), origin=origin1)  
# 
# # create time series
# ts1 <- xts( c(values1, tail(values1,1)),  time1)
# 
# # plot time series
# plot(ts1, type = "s")
# index <- index %>% 
#   mutate(AZ_date4 = as.Date(AZ_time4)) %>% 
#   mutate(AZ_time4 = as.POSIXct(AZ_time4, format = "%y/%m/%d %H:%M:%S"))

# change new_ID into a factor
library(ggrepel)
index$new_ID <- as.factor(index$new_ID)

index_plot <- index %>% 
  # pivot_longer(index) %>% 
  ggplot(aes(x=AZ_date4, y=Origin, group=new_ID)) +
  geom_point(aes(colour=new_ID)) 

index_plot <- index_plot + geom_label_repel(aes(label = new_ID),
                                            box.padding   = 0.35, 
                                            point.padding = 0.5,
                                            segment.color = 'grey50') 
print(index_plot)

# subset data with multiple rows of new_ID
library(directlabels)

# out <- origin_case2[origin_case2$new_ID %in% c(3425744253,309866,848328,2392492,4156525,2558010978,
#                                                5112028646,
#                                                5147509645,
#                                                8030257568,
#                                                6435057,
#                                                6668820976,
#                                                9073399848,
#                                                9206712044,
#                                                9361829403,
#                                                9471620953,
#                                                9798838220,
#                                                667540396), ]
# out <- subset(origin_case2, new_ID %in% c(3425744253,309866,848328,2392492,4156525,2558010978,
#                                           5112028646,
#                                           5147509645,
#                                           8030257568,
#                                           6435057,
#                                           6668820976,
#                                           9073399848,
#                                           9206712044,
#                                           9361829403,
#                                           9471620953,
#                                           9798838220,
#                                           667540396))

out <- left_cases[left_cases$new_ID %in% c(3425744253,2807483500,7376844695,8580613940,
                                               5112028646,
                                               5147509645,
                                               8030257568,
                                               6435057,
                                               6668820976,
                                               9073399848,
                                               9206712044,
                                               9361829403,
                                               9471620953,
                                               9798838220,
                                               667540396), ]
out <- subset(left_cases, new_ID %in% c(3425744253,2807483500,7376844695,8580613940,
                                          5112028646,
                                          5147509645,
                                          8030257568,
                                          6435057,
                                          6668820976,
                                          9073399848,
                                          9206712044,
                                          9361829403,
                                          9471620953,
                                          9798838220,
                                          667540396))

out <- out %>% 
  #   group_by(new_ID) %>% 
  mutate(AZ_time = as.POSIXlt(AZ_time4, tz = "",
                              tryFormats = c("%Y-%m-%d %H:%M:%OS"), 
                              optional = FALSE))
# out$new_ID <- as.factor(out$new_ID)

new_df <- subset(out, select = c(new_ID, Students, Department__c, Origin, 
                                         AZ_date4, AZ_time, period, CaseNumber, seqnum))
write.csv(new_df, file = "sample_data_time.csv")

#####
out_plot <- out %>% 
  # pivot_longer(index) %>% 
  ggplot(aes(x=AZ_time4, y=Origin, group=new_ID)) +
  geom_point(aes(colour=new_ID)) + 
  geom_line(aes(colour=new_ID)) +
  geom_dl(aes(label=new_ID), method="last.points") # or "first.points"

out_plot <- out_plot + geom_label_repel(aes(label = new_ID),
                                            box.padding   = 0.35, 
                                            point.padding = 0.5,
                                            segment.color = 'grey50') 
print(out_plot)

# plot sample with DeveloperName
out_plot2 <- out %>% 
  # pivot_longer(index) %>% 
  ggplot(aes(x=AZ_time4, y=Department__c, group=new_ID)) +
  geom_point(aes(colour=new_ID)) + 
  geom_line(aes(colour=new_ID)) +
  geom_dl(aes(label=new_ID), method="last.points") # or "first.points"

out_plot2 <- out_plot2 + geom_label_repel(aes(label = new_ID),
                                        box.padding   = 0.35, 
                                        point.padding = 0.5,
                                        segment.color = 'grey50') 
print(out_plot2)

# figure out average length of time from one case to the next case per user
origin_cases <- left_cases %>% 
  # group_by(new_ID) %>% 
  mutate(AZ_time = as.POSIXct(AZ_time4, format = "%y/%m/%d %H:%M:%S")) %>% 
  mutate(AZ_date = as.Date(AZ_date4))

# find an average between cases for each unique user
library(zoo)
library(data.table)
# difftime() function in R: calculate the time difference between two times
recent_time <- "2021-09-14 13:35:36"
earlier_time <- "2020-05-02 08:01:55"
difftime(recent_time,earlier_time) #Time difference of 500.2317 days

# https://stackoverflow.com/questions/15505879/date-time-differences-between-rows-in-r
c_time <- as.POSIXlt(origin_case3$AZ_time4)
difftime( c_time[1] , c_time[2:length(c_time)] )

origin_case3 <- origin_case3 %>% 
  arrange(new_ID) %>% 
  mutate(difftime = difftime( c_time[1] , c_time[2:length(c_time)] )) # does not work

# in reverse
c_time <- rev( c_time )
difftime(c_time[1:(length(c_time)-1)] , c_time[2:length(c_time)])

# time structure in R
# change time to POSIXlt in R
origin_case3$AZ_time4 <- strptime(origin_case3$AZ_time4,"%Y-%m-%d %H:%M:%S")

# myfile %>% mutate(V5 = ifelse(V1 == 1 & V2 != 4, 1, ifelse(V2 == 4 & V3 != 1, 2, 0)))

origin_case3 <- origin_case3 %>% 
  group_by(new_ID) %>% 
  arrange(seqnum) %>% 
  mutate(N = length(origin_case3$AZ_time4)) %>% 
  origin_case3$AZ_time4[2:N] - origin_case3$AZ_time4[1:(N-1)] 

# N = length(origin_case3$AZ_time4)
# origin_case3$AZ_time4[2:N] - origin_case3$AZ_time4[1:(N-1)]# for the whole data

library(chron)
####
# https://stackoverflow.com/questions/31904979/loop-over-a-large-list-of-elements-and-calculate-mean-in-r
### remove redundant variables
left_cases <- subset(left_cases, select = -c(X.1, X))

###
# new_ID <- sample(1:100, size = 4018, replace = TRUE)
# # unemployed <- sample(1:100, size = 4018, replace = TRUE)
# insurance <- sample(1:100, size = 4018, replace = TRUE)
# daily_seq <- seq(as.Date("2004-01-01"), as.Date("2014-12-31"), by = "days")
# daily_df <- data.frame(daily_seq, jobs, unemployed, insurance)
# 
# daily_df %>%
#   # mutate(WeekOfYear = week(daily_seq)) %>% # obtain week of year
#   group_by(new_ID) %>% # group by that 
#   select(-daily_seq) %>% # remove variables you don't need to average on
#   summarise_each(funs(mean))

left_cases %>% 
  group_by(new_ID) %>% 
  summarize(duration = mean(as.interval()))
  
###
# https://rdrr.io/cran/timetk/man/summarise_by_time.html 
library(timetk) 
library(tidyquant)

origin_case3 <- origin_case2 %>% 
  #   group_by(new_ID) %>% 
      mutate(AZ_time = as.POSIXlt(AZ_time4, tz = "",
                               tryFormats = c("%Y-%m-%d %H:%M:%OS"), 
                               optional = FALSE))
# David's code
origin_cases3 <- origin_case3 %>% 
  group_by(new_ID) %>% 
  arrange(seqnum) %>% 
  mutate(tmp = lag(AZ_time),
         diff = AZ_time - tmp)
# 
# origin_cases %>% 
#   group_by(new_ID) %>% 
#   summarise_by_time(.date_var = AZ_time,
#                     .by = "period"
#                     )

# How likely are users changing from one mode to a different mode?
# How likely are users changing from one department to a different department?
# 
# origin_cases %>%
#   mutate(date_col = date(AZ_time4)) %>%
#   group_by(date_col) %>% 
#   # summarize(value = sum(value))
install.packages("gridExtra")
library(gridExtra) # tile several plots next to each other
library(scales)

# ts_cases <- origin_cases %>% 
#   pivot_longer(c("Origin", "AZ_time4", "period", "Students", 
#                  "Department__c"),
#                names_to = "date", 
#                values_to = "time") %>%
#   # mutate(date = mdy(date)) %>% 
#   group_by(Origin, AZ_time4) %>% 
#   summarise(time = mean(AZ_time4)) %>% 
#   ungroup()

# duration is time span measured in seconds
# primary representation in sec
# interval/dhours()

# period is time span in year
recent_time <- ("2021-09-14 13:35:36")
earlier_time <- ("2020-05-02 08:01:55")

# working with time


library(TraMineR)
no_NA_cases <- left_cases[ !is.na(left_cases$Origin), ]

actcal.seqe <- seqecreate(id = factor(no_NA_cases$new_ID), 
                          timestamp = no_NA_cases$AZ_time, 
                          event = no_NA_cases$Origin, use.labels=FALSE)
