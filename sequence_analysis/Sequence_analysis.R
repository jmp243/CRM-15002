# TraMineR
# jmpark@arizona.edu
# Oct. 4, 2021
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
library(foreign)
library(cluster)
library(gridExtra)
library(seqinr)
library(adegenet)
library(ape)
library(viridis)
library(ggplot2)
library(TraMineR)
library(WeightedCluster)
library(googleVis)
library(directlabels)
library(stringr)
# read in data 
# setwd("~/Documents/Trellis/CRM-15002/data")
# origin_case2 <- read.csv("origin_case2.csv", header = TRUE, na.strings=c("","NA"))
# 
# origin_case2 <- origin_case2 %>% 
#   arrange(AZ_time4) %>% 
#   # group_by(new_ID) %>% 
#   # # mutate(seq_time = seq_len(n())) %>% 
#   # mutate(end_time = 1:n())
#   mutate(new_time = seq(1:145212))
# 
# # drop some columns
# origin_case2 %>% select(-1, -2, -3) 

#### for peak fall data
setwd("~/Documents/Trellis/CRM-15002/sequence_analysis/fall2021_data")

peak_fall2021 <- read.csv("peak_fall2021.csv", header = TRUE, na.strings=c("","NA"))
fall_2021_dept <- read.csv("fall2021_dept.csv", header = TRUE, na.strings=c("","NA"))
fall_2021_mode <- read.csv("fall2021_mode.csv", header = TRUE, na.strings=c("","NA"))
peak_fall2021$new_ID <- as.factor(peak_fall2021$new_ID)
peak_fall2021$AZ_date <- as.Date(peak_fall2021$AZ_date4)

# basic descriptive analysis
first_mode_2021 <- peak_fall2021 %>% 
  filter(seqnum == 1)

table(first_mode_2021$Mode, first_mode_2021$Dept)
# # add next to data
# origin_case6 <- origin_case2 %>% 
#   group_by(new_ID) %>% 
#   mutate(next_mode=lead(Mode)) %>% 
#   replace_na(replace = list(next_mode = "none")) %>% 
#   mutate(next_dept=lead(Dept)) %>% 
#   replace_na(replace = list(next_dept = "none")) %>% 
#   ungroup()
# 
# case6_pruned <- subset(origin_case6, select = c(new_ID, CaseNumber, Students, 
#                                                 Origin, Department__c, period, new_time,
#                                                 AZ_date4,AZ_time, tmp, diff, COUNT,  
#                                                 seqnum, Mode, next_mode, Dept, next_dept))
# 
# write.csv(case6_pruned, file = "case6_pruned.csv")  
# 
# # subset data to students
# student_pruned <- case6_pruned %>% 
#   filter(Students != "Undergraduate"|Students !="Graduate") 
# 
# student_pruned <- student_pruned %>% 
#   mutate(AZ_date = as.Date(AZ_date4))
# 
# ## subset data per period
# # peak_fall2021 <- filter(student_data2, period == "Peak Fall 2021")
# peak_fall2021 <- filter(student_pruned, period == "Peak Fall 2021")
# 
# 
# # redo transition probabilites
# peak_fall2021$next_mode <- as.character(peak_fall2021$next_mode)
# peak_fall2021$next_mode[is.na(peak_fall2021$next_mode)] <- "End"
# peak_fall2021$next_mode <- as.factor(peak_fall2021$next_mode)

# fall2021_prob <- peak_fall2021 %>% 
#   group_by(new_ID) %>% 
#   mutate(next_mode=lead(Mode)) %>% 
#   replace_na(replace = list(next_mode = "End")) %>% 
#   ungroup() %>% 
#   count(Mode, next_mode) %>% 
#   group_by(Mode) %>% 
#   mutate(total_starts = sum(n),
#          prob = n/total_starts)
  

fct_explicit_na(fall2021_prob$next_mode, na_level = "End") 

fall2021_prob$ord_mode <- factor(fall2021_prob$next_mode, 
                                 ordered=TRUE, levels = c("Chat", "Phone", "In Person", 
                                                          "Email", "Webform", "Zoom", "End"))

Mode_tile <-
  ggplot(data=subset(fall2021_prob, !is.na(Mode)), aes(y = Mode, x = ord_mode)) +
  geom_tile(aes(fill=prob), color = "white") +
  # scale_fill_manual(values=colors) +
  labs(title = "Likelihood of Moving to the Another Mode", subtitle = "Student Data from 8/6 - 9/15/2021",
       x = "Next Mode", y = "Starting Mode") +
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c",
                      guide = "colorbar") 
# theme_minimal()

Mode_tile


#### dept transition for Peak Fall 2021 students only
peak_fall2021$next_dept <- as.character(peak_fall2021$next_dept)
peak_fall2021$Dept[is.na(peak_fall2021$Dept)] <- "Null"
peak_fall2021$next_dept[is.na(peak_fall2021$next_dept)] <- "End"
peak_fall2021$next_dept <- as.factor(peak_fall2021$next_dept)

fall2021_dept <- peak_fall2021 %>% 
  group_by(new_ID) %>% 
  mutate(next_dept=lead(Dept)) %>% 
  replace_na(replace = list(next_dept = "End")) %>% 
  ungroup() %>% 
  count(Dept, next_dept) %>% 
  group_by(Dept) %>% 
  mutate(total_starts = sum(n),
         prob = n/total_starts)

fct_explicit_na(fall2021_dept$Dept, na_level = "Null") 
fct_explicit_na(fall2021_dept$next_dept, na_level = "End") 

fall2021_dept$ord_dept <- factor(fall2021_dept$next_dept, 
                                 ordered=TRUE, levels = c("24/7", "Financial Aid", "Other", 
                                                          "Registrar", "SOS", "SECD", "End"))

Dept_tile <-
  ggplot(data=subset(fall2021_dept, !is.na(Dept)), aes(y = Dept, x = ord_dept)) +
  geom_tile(aes(fill=prob), color = "white") +
  # scale_fill_manual(values=colors) +
  labs(title = "Likelihood of Moving to the Another Department", subtitle = "Student Data from 8/6 - 9/15/2021",
       x = "Next Mode", y = "Starting Mode") +
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c",
                      guide = "colorbar") 
# theme_minimal()

Dept_tile

### sample sequences from fall 2021 students
random_fall <- peak_fall2021[peak_fall2021$new_ID %in% c(848328, 5114826, 6258721, 8330416, 15794229,
                                               19400733,29771127,34247487), ]

random_fall$new_ID <- as.factor(random_fall$new_ID)

out_plot <- random_fall %>% 
  # pivot_longer(index) %>% 
  ggplot(aes(x=AZ_date, y=Mode, group=new_ID)) +
  geom_point(aes(colour=new_ID)) + 
  geom_line(aes(colour=new_ID)) +
  geom_dl(aes(label=new_ID), method="last.points") # or "first.points"

out_plot <- out_plot + labs(title = "Sample Cases for Modes Used", 
                            subtitle = "students from 8/6 - 9/15/2021",  fill = "Mode")

print(out_plot)


out_plot2 <- random_fall %>% 
  # pivot_longer(index) %>% 
  ggplot(aes(x=AZ_date, y=Dept, group=new_ID)) +
  geom_point(aes(colour=new_ID)) + 
  geom_line(aes(colour=new_ID)) +
  geom_dl(aes(label=new_ID), method="last.points") # or "first.points"

out_plot2 <- out_plot2 + labs(title = "Sample Cases for Departments", 
                            subtitle = "students from 8/6 - 9/15/2021",  fill = "Dept")

print(out_plot2)
# student_pruned <- student_pruned %>% 
#   # group_by(new_ID) %>% 
#   mutate(AZ_time2 = as.POSIXct(tmp, format = "%y/%m/%d %H:%M:%S"))

# SPELL format would be new_ID, seqnum, AZ_time, tmp, Mode or Dept.
# student_mode <- subset(student_pruned, select = c(new_ID, seqnum, new_time, AZ_date,
#                                                   AZ_time, tmp, Mode))
# 
# actal.seq <- seqdef(student_mode)
# alphabet(actal.seq)
# sts.data <- seqformat(case_dates, from="SPELL", to="STS",   
#                    id="new_ID",begin="seqnum",end="end_time",status="Mode",
#                    limit=125,process=FALSE)
# visualizing sequence data
# par(mfrow = c(2,2))
# seqiplot(actal.seq, main = "index plot", with.legend = FALSE)
# seqdplot(actal.seq, main = "state distribution plot", with.legend = FALSE)
# seqfplot(actal.seq, main = "sequence frequency plot", with.legend = FALSE, pbarw = TRUE)
# seqlegend(actal.seq)

# https://www.analyzecore.com/2014/12/04/sequence-carts-in-depth-analysis-with-r/

# 
# max.date <- max(student_pruned$AZ_date4)  # this did not work with +1
# ids <- unique(student_pruned$new_ID)
# df.new <- data.frame()
# 
# for (i in 1:length(ids)) {
#   df.cache <- student_pruned %>%
#     filter(new_ID==ids[i])
#   ifelse(nrow(df.cache)==1,
#          av.dur <- 30,
#          av.dur <- round(((max(df.cache$AZ_date) -
#                              min(df.cache$AZ_date))/(nrow(df.cache)-1))*1.5, 0))
#   df.cache <- rbind(df.cache, data.frame(new_ID=df.cache$new_ID[nrow(df.cache)],
#                                          sex=df.cache$Mode[nrow(df.cache)],
#                                          orderdate=max(df.cache$AZ_date)+
#                                          av.dur, cart='nopurch'))
#   ifelse(max(df.cache$orderdate) > max.date,
#          df.cache$orderdate[which.max(df.cache$orderdate)] <- max.date, NA)
# 
#   df.cache$to <- c(df.cache$orderdate[2:nrow(df.cache)]-1, max.date)
#   # order# for Sankey diagram
#   df.cache <- df.cache %>%
#     mutate(ord = paste('ord', c(1:nrow(df.cache)), sep=''))
#   df.new <- rbind(df.new, df.cache)
# } # error
# 
# # convert dates to numbers
# 
# df.form <- seqformat(as.data.frame(case_dates), id='New_ID', 
#                      begin='seqnum', end='end_time', status='Dept',
#                      from='SPELL', to='STS', process=FALSE)

write.csv(peak_fall2021, file = "peak_fall2021.csv")  
write.csv(fall2021_dept, file = "fall2021_dept.csv")  
write.csv(fall2021_prob, file = "fall2021_mode.csv")  

# fit data to TraMiner
# first duration is not important
# unequal sequence lengths
# normalize sequence lengths by changing the alphabets



# re-calculate Fall seqnum
# peak_fall2021 <- peak_fall2021 %>%
#   group_by(new_ID) %>%
#   arrange(AZ_time) %>%
#   mutate(seqnum = 1:length(new_ID)) %>% 
#   ungroup()
  
# subset data to new_ID's with more than one sequence

multi_students <- peak_fall2021 %>% 
  filter(seqnum>1) # this is only 11651

table(multi_students$First_Generation__c)
table(multi_students$Mode)

table(unlist(multi_students[, -1]))

# convert modes into factors
multi_students$Mode <- as.factor(multi_students$Mode)
multi_students$next_mode <- as.factor(multi_students$next_mode)

# dfCount <- count(multi_students,  c("Mode", "next_mode")) # error message

# TraMineR package
# seq <- seqdef(multi_students[, -1], xtstep = 1) # error 
# [!] found '-' character in state codes, not recommended
# Error in withCallingHandlers(expr, warning = function(w) if (inherits(w,  : 
#     invalid multibyte string at '<93>Aut<68>entication failed on UAWifi<94>'

# using other functions
library(arules)
library(arulesNBMiner)

# subset_multi <- multi_students %>% 
#   select(new_ID, Mode, next_mode, AZ_date, tmp, new_time)
# 
# eclat(subset_multi, parameter = NULL, control = NULL)
# 
# itemsets <- eclat(subset_multi)
# 
# itemsets_sorted <- sort(itemsets)
# 
# itemsets_sorted[1:5]
# 
# itemsets <- eclat(subset_multi, parameter = list(minlen=9))
# inspect(itemsets)

# subset from Fall 2021 peak data
out <- subset(peak_fall2021, new_ID %in% c(8890918641,
                                           5444850216,
                                           1141922,
                                           3451610,
                                           5114826,
                                           6258721))

out_plot <- out %>% 
  # pivot_longer(index) %>% 
  ggplot(aes(x=AZ_date, y=Mode, group=new_ID)) +
  geom_point(aes(colour=new_ID)) + 
  geom_line(aes(colour=new_ID)) +
  geom_dl(aes(label=new_ID), method="last.points") # or "first.points"

out_plot <- out_plot + labs(title = "Sample Cases for Modes Used", 
                            subtitle = "students from 8/6 - 9/15/2021",  fill = "Mode")

print(out_plot)

# dept
out_plot1 <- out %>% 
  # pivot_longer(index) %>% 
  ggplot(aes(x=AZ_date, y=Dept, group=new_ID)) +
  geom_point(aes(colour=new_ID)) + 
  geom_line(aes(colour=new_ID)) +
  geom_dl(aes(label=new_ID), method="last.points") # or "first.points"

out_plot1 <- out_plot1 + labs(title = "Sample Cases for Depts Used", 
                            subtitle = "students from 8/6 - 9/15/2021",  fill = "Mode")

print(out_plot1)
# ## sample data from TraMineR
# data('mvad')
# head(mvad)
# alphabet = seqstatl(mvad[,17:86])
# 
# fulllabel<- c("employment", "further education",
#               "higher education","joblessness", "school", "training")
# shortlabel<- c("EM", "FE", "HE", "JL", "SC", "TR")
# seq_mvad<- seqdef(mvad[, 17:86], alphabet = alphabet,
#                   states = shortlabel, labels = fulllabel, weights = mvad$weight,
#                   xtstep = 6)
# seq_mvad[1:2,]
# print(seq_mvad[1:2,],format="SPS")
# 
# ## 
# data(actcal)
# actcal.seq <- seqdef(actcal,13:24,
#                      labels=c("> 37 hours", "19-36 hours", "1-18 hours", "no work"))

### traminer with my data
# multi_students <- multi_students %>% 
#   arrange(tmp) %>% 
#   # group_by(new_ID) %>% 
#   # # mutate(seq_time = seq_len(n())) %>% 
#   # mutate(end_time = 1:n())
#   mutate(tmp_time = seq(1:11651))



# multi_students <- multi_students %>% 
#   arrange(AZ_time4) %>% 
#   # group_by(new_ID) %>% 
#   # # mutate(seq_time = seq_len(n())) %>% 
#   # mutate(end_time = 1:n())
#   mutate(newer_time = seq(1:11651))
# df_wide <- read.csv("df_wide.csv", header = TRUE, na.strings=c("","NA"))
# 
# # df.form <- seqformat(multi_students, id='new_ID', begin='tmp_time', end = "new_time",
# #                       status='Mode', from='SPELL', to='STS', process=FALSE)
# 
# write.csv(df.form, file = "df_form.csv")
# df_wide = reshape(multi_students, idvar="new_ID", v.names="Mode", 
#                    timevar="tmp_time", direction='wide') #11677 variables
# 
# df_wide_time = reshape(multi_students, idvar="new_ID", v.names="Mode", 
#                   timevar="AZ_time4", direction='wide') # 11580 variables

df_wide_date <- read.csv("df_wide_date.csv", header = TRUE, na.strings=c("","NA"))

df_wide_date <- reshape(multi_students, idvar="new_ID", v.names="Mode", 
                  timevar="AZ_date", direction='wide') # 66 variables
# write.csv(df_wide_date, file = "df_wide_date.csv")

# df.form <- seqformat(multi_students, id='new_ID', begin='tmp_time', end = "tmp_time", 
                     # status='Mode', from='SPELL', to='STS', process=FALSE)

# multi_seq <- read.csv("multi_seq.csv", header = TRUE, na.strings=c("","NA"))
#Prepare a sequence for TraMineR:
act_vals <- c("1", "2", "3", "4", "5", "6")
act_labels <- c("Chat", "Email", "In Person", "Phone", "Webform", "Zoom")

df_seq <- seqdef(df_wide_date, var=26:65, states=act_vals, labels=act_labels, 
                  id=df_wide_date$new_ID, informat='STS', compressed=TRUE) # check to see if the var columns align

df_seq_left_del <- seqdef(df_wide_date, var=26:65, states=act_vals, labels=act_labels, left = "DEL",
                 id=df_wide_date$new_ID, informat='STS', compressed=TRUE)
# creating an alphabet
alphabet(df_seq)
alphabet(df_seq_left_del)
# multi_seq <- seqdef(df_wide_date, var = 1:300, left="DEL", gaps = "DEL") # cannot figure out the numbering system

# write.csv(multi_seq, file = "multi_seq.csv")

### Frequency Table
df_freq_table <- attr(seqtab(df_seq, idxs = 0, format='STS'), "freq")
length(which(df_freq_table$Freq==1)) # length where Freq is one is 1663 or 1752 with another try
length(which(df_freq_table$Freq>=2))# SO ONLY 353 or 356 HAVE A NONE UNIQUE SEQUENCE.

head(df_freq_table, 20)
sum(df_freq_table$Freq[1:20]) #1258 users experienced the top 20 most common frequencies

df_seq_len = seqlength(df_seq) # longest ones have 39 in it. 
sum(df_seq_len==6) #157 people use all 6 cases

# left deleted freq table
df_freq_table_leftout <- attr(seqtab(df_seq_left_del, idxs = 0, format='STS'), "freq")

ten_left <- seqtab(df_seq_left_del)
ten_left

## new dataset of mulit_seq
# multi_seq <- df_seq %>% 
#   filter 
  
##
df_seq6 <- df_seq[ order(df_seq$`Mode.2021-08-06`,df_seq$`Mode.2021-08-07`, df_seq$`Mode.2021-08-08`,
                         df_seq$`Mode.2021-08-09`, df_seq$`Mode.2021-08-10`,
                         df_seq$`Mode.2021-08-11`, df_seq$`Mode.2021-08-12`, 
                         df_seq$`Mode.2021-08-13`,df_seq$`Mode.2021-08-14`, df_seq$`Mode.2021-08-15`,
                         df_seq$`Mode.2021-08-16`, df_seq$`Mode.2021-08-17`,
                         df_seq$`Mode.2021-08-18`, df_seq$`Mode.2021-08-19`, 
                         df_seq$`Mode.2021-08-20`, df_seq$`Mode.2021-08-21`,
                         df_seq$`Mode.2021-08-22`, df_seq$`Mode.2021-08-23`, 
                         df_seq$`Mode.2021-08-24`,df_seq$`Mode.2021-08-25`, df_seq$`Mode.2021-08-26`,
                         df_seq$`Mode.2021-08-27`, df_seq$`Mode.2021-08-28`,
                         df_seq$`Mode.2021-08-29`, df_seq$`Mode.2021-08-30`, df_seq$`Mode.2021-08-31`,
                         df_seq$`Mode.2021-09-01`,df_seq$`Mode.2021-09-02`, df_seq$`Mode.2021-09-03`,
                         df_seq$`Mode.2021-09-04`, df_seq$`Mode.2021-09-05`,
                         df_seq$`Mode.2021-09-06`, df_seq$`Mode.2021-09-07`, 
                         df_seq$`Mode.2021-09-08`,df_seq$`Mode.2021-09-09`, df_seq$`Mode.2021-09-10`,
                         df_seq$`Mode.2021-09-11`, df_seq$`Mode.2021-09-12`, 
                         df_seq$`Mode.2021-09-13`,df_seq$`Mode.2021-09-14`), ] #it just shows up ordered

### department format
dept_wide_date <- reshape(multi_students, idvar="new_ID", v.names="Dept", 
                        timevar="AZ_date", direction='wide') # 66 variables
# dept.form <- seqformat(multi_students, id='new_ID', begin='tmp_time', end = "new_time", 
#                      status='Dept', from='SPELL', to='STS', process=FALSE) 
# dept_seq <- seqdef(dept.form, 1:300, left="DEL", gaps = "DEL") # cannot figure out the numbering system

act_value <- c("A", "B", "C", "D", "E", "F")
act_label <- c("24/7", "OSFA", "Other", "Registrar", "SECD", "SOS")

dept_seq <- seqdef(dept_wide_date, var=27:65, states=act_value, labels=act_label, 
                 id=df_wide_date$new_ID, informat='STS', compressed=TRUE) 

dept_seq_left_del <- seqdef(dept_wide_date, var=27:65, states=act_value, labels=act_label, left = "DEL",
                                         id=df_wide_date$new_ID, informat='STS', compressed=TRUE) 
# freq table for dept
dept_freq_table <- attr(seqtab(dept_seq, idxs = 0, format='STS'), "freq")

dept_freq_table_left <- attr(seqtab(dept_seq_left_del, idxs = 0, format='STS'), "freq")
# top 10 dept
ten_dept <- seqtab(dept_seq)
ten_dept

#visualizing dept data
par(mfrow = c(1,2))
seqiplot(dept_seq, border = NA, main = "First 10 sequences", with.legend = FALSE) # adding with.missing = FALSE did not change the plot
seqfplot(dept_seq, main = "Top 10 sequences", border = NA, with.missing = FALSE, with.legend = FALSE, pbarw = TRUE)

par(mfrow = c(1,2))
seqdplot(dept_seq, main = "State distribution plot", border = NA, with.legend = FALSE) # state distribution plot
seqlegend(dept_seq)

# visualizing left del dept data
par(mfrow = c(1,2))
seqiplot(dept_seq_left_del, border = NA, main = "First 10 sequences", with.legend = FALSE) # adding with.missing = FALSE did not change the plot
seqfplot(dept_seq_left_del, main = "Top 10 sequences", border = NA, with.missing = FALSE, with.legend = FALSE, pbarw = TRUE)

par(mfrow = c(1,2))
seqdplot(dept_seq_left_del, main = "State distribution plot", border = NA, with.legend = FALSE) # state distribution plot
seqlegend(dept_seq_left_del)

# visualizing data
# par(mfrow = c(2,2))
# looks like i need to transpose some data
par(mfrow = c(1,2))
seqiplot(df_seq6, border = NA, main = "First 10 sequences", with.legend = FALSE) # adding with.missing = FALSE did not change the plot
seqfplot(df_seq6, border = NA, main = "Top 10 sequences", with.missing = FALSE, with.legend = FALSE, pbarw = TRUE)

par(mfrow = c(1,2))
seqdplot(df_seq6, border = NA, main = "State distribution plot", with.legend = FALSE) # state distribution plot
seqlegend(df_seq6)

# visualize left deleted data
par(mfrow = c(1,2))
seqiplot(df_seq_left_del, border = NA, main = "First 10 sequences", with.legend = FALSE) # adding with.missing = FALSE did not change the plot
seqfplot(df_seq_left_del, border = NA, main = "Top 10 sequences", with.missing = FALSE, with.legend = FALSE, pbarw = TRUE)

par(mfrow = c(1,2))
seqdplot(df_seq_left_del, border = NA, main = "State distribution plot", with.legend = FALSE) # state distribution plot
seqlegend(df_seq_left_del)


## transitions for modes
trans = seqtrate(df_seq)
round(trans,2) 

## transitions for modes with left del
transit = seqtrate(df_seq_left_del) # which is the same as the transitions above
round(transit,2) # which is the same as above

## transitions for dept
transition <- seqtrate(dept_seq)
round(transition, 2)
# state frequencies
state_freq <- seqstatd(df_seq)

# visualizing department
par(mfrow = c(1,2))
# seqiplot(dept_seq, border = NA, withlegend = FALSE)
seqdplot(dept_seq, main = "State distribution plot", border = NA, with.legend = FALSE) # state distribution plot
# seqfplot(dept_seq, border = NA, with.missing = FALSE, withlegend = FALSE)
seqlegend(dept_seq)



# sequence lengths
  seqlength(df_seq,)
### matches
match3 <- seqpm(df_seq, "[1].{3,}") # 450 sequences have 3 plus states after chats
match2 <- seqpm(df_seq, "[1].{2,}") # 510 sequences have 2 plus states after chats
match1 <- seqpm(df_seq, "[1].{1,}") # 607 sequences have 1 plus state after chats

# entropy index 
seqHtplot(df_seq)

# 10 most freq seq 
ten_freq <- seqtab(df_seq)
# with missing values
subm <- seqsubm(df_seq, method = "CONSTANT", with.miss = TRUE)
subm

ms.seq <- seqdef(df_seq, left= "DEL", gaps = "DEL", right = "DEL")
data.eseq <- seqecreate(df_seq, tevent = "act_vals", 
                        end.event = attr(df_seq,'void'))

## group data by dept
dept_fall <- df_wide_date %>%
  dplyr::group_by(Dept) %>% 
  dplyr::filter(seqnum > 2)

act_vals <- c("1", "2", "3", "4", "5", "6")
act_labels <- c("Chat", "Email", "In Person", "Phone", "Webform", "Zoom")

fall_seq_left_del <- seqdef(dept_fall, var=26:65, states=act_vals, labels=act_labels, left = "DEL",
                 id=dept_fall$new_ID, informat='STS', compressed=TRUE)

multi_fall_freq_table <- attr(seqtab(fall_seq, idxs = 0, format='STS'), "freq")
####
more_than_3 <- seqtab(df_seq_left_del)
more_than_3
