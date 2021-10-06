# Jung Mee Park
# jmpark@email.arizona.edu
# Sept. 23, 2021

setwd("~/Documents/Trellis/CRM-15002/data")
origin_case4 <- read.csv("prob_cases_mode.csv", header = TRUE, na.strings=c("","NA"))
origin_case5 <- read.csv("prob_cases_dept.csv", header = TRUE, na.strings=c("","NA"))
# working with origin_cases first
#### transition probability matrices ###
# pairCodes = paste(origin_cases$CaseNumber,",",origin_cases$newID,sep="")
# uniqueCodes = unique(origin_cases$new_ID)
# 
# time.curr = rep(0, nrow(origin_cases))
# 
# for (i in 1:length(uniqueCodes)){
#   inds = which(pairCodes==uniqueCodes[i])
#   lenInds = length(inds)
#   
#   if (lenInds > 1) {
#     for (row in 2:(length(inds))){			
#       # There was not a state change, so increase time_curState
#       if (data$dipSym[inds[row]] == data$dipSym[inds[row-1]]) { 
#         time.curr[inds[row]] = time.curr[inds[row-1]] + data$year[inds[row]]- data$year[inds[row-1]]
#         
#         # or, there was a state change, so time.curr is zero
#       } else {
#         time.curr[inds[row]] = 0
#       }
#     }
#   }
# }
# data$time.curr = time.curr

######
library("flexsurv") 
library(mlogit)   #for multinomial logistic regression
library(numDeriv) #for numerical differentiation in the delta method

origin_case2 <- read.csv("origin_case2.csv", header = TRUE, na.strings=c("","NA"))
# subset my data
# dip = subset(origin_cases, select = c(new_ID, Students, Department__c, period, Origin, 
#                                      AZ_date4, AZ_time))
# data = subset(dip, select = c(Origin))
# tm = matrix(c(rep(0,64)),8,8);tm
# tpm = matrix(c(rep(0,64)),8,8);tpm
# 
# # for (i in 1:length(data)-1)
# # tm[data[i],data[i+1]]=(tm[data[i],data[i+1]]+1) #did not work
# # # Error in tm[data[i], data[i + 1]] : invalid subscript type 'list'
# tm

# starting = origin_cases$Origin      #possible starting states, which we will loop through
# # mlogdata = mlogit.data(dip, choice="y", alt.levels=c("Chat","Email","In Person", "Zoom",
# #                                                      "Offline Chat","Phone","Text","Webform"), 
# #                        shape="wide") 

# sequence analysis
library(TraMineR)

dip$new_ID <- as.factor(dip$new_ID)
dip$Students <- as.factor(dip$Students)
dip$Department__c <- as.factor(dip$Department__c)
dip$Origin <- as.factor(dip$Origin)
dip$period <- as.factor(dip$period)
dip$AZ_date4 <- as.Date(dip$AZ_date4)

# dip <- subset(dip, select = -c(CreatedDate))
# 
# seq <- seqdef(dip[,5])
# seqdplot(seq, group=dip$Students, border=NA)

# subset data to multicases
multi_cases <- origin_cases %>% group_by(new_ID) %>% 
  mutate(COUNT = n()) 

multi_cases <- multi_cases %>% 
  filter(COUNT >= 2) # 175723 cases

# get unique length
length(unique(multi_cases[["new_ID"]])) #31039

# remove duplicate cases
multi_case2 <- multi_cases %>%
  arrange(new_ID, -CaseNumber) %>%
  filter(duplicated(CaseNumber) == FALSE) #down to 128,745

## take data from origin_case2 
nrows = nrow(origin_case2)
period = rep(0, nrows)
period[which(origin_case2$AZ_time4 < "2020-08-06")] = "Summer 2020"			 
period[which(origin_case2$AZ_time4 >= "2020-08-06" & 
               origin_case2$AZ_time4 <= "2020-09-07")] = "Peak Fall 2020"
period[which(origin_case2$AZ_time4 >= "2020-09-07" & 
               origin_case2$AZ_time4 <= "2020-12-31")] = "Non-Peak Fall 2020"
period[which(origin_case2$AZ_time4 >= "2020-12-31" & 
               origin_case2$AZ_time4 <= "2021-02-01")] = "Peak Spring 2021" 
period[which(origin_case2$AZ_time4 >= "2021-02-01" & 
               origin_case2$AZ_time4 <= "2021-05-31")] = "Non-Peak Spring 2021"
period[which(origin_case2$AZ_time4 >= "2021-05-31" & 
               origin_case2$AZ_time4 <= "2021-08-06")] = "Summer 2021"
period[which(origin_case2$AZ_time4 >= "2021-08-06" & 
               origin_case2$AZ_time4 <= "2021-09-15")] = "Peak Fall 2021"

origin_case2$period = period  
table(origin_case2$period)

# # create sequence numbers
# origin_case4 <- origin_case3 %>% 
#   group_by(new_ID) %>% 
#   arrange(AZ_time) %>% 
#   mutate(seqnum = 1:length(new_ID))

origin_case2 <- origin_case2 %>% 
  group_by(new_ID) %>% 
  arrange(AZ_time) %>% 
  mutate(seqnum = 1:length(new_ID))

# David's code works but how do I interpret it
origin_case2 <- origin_case2 %>% 
  group_by(new_ID) %>% 
  arrange(seqnum) %>% 
  # mutate(n = n()) %>% # essentially count
  mutate(tmp = lag(AZ_time),
         diff = AZ_time - tmp)


# subset the origin_case2 data
colnames(origin_case2) 

# MCM's example
library(tidyverse)

tibble(user = c("user1", "user2", "user2", "user2", "user2", "user2"),
       mode = c("a", "a", "b", "c", "a", "c")) %>% 
  group_by(user) %>% 
  mutate(next_mode = lead(mode)) %>% 
  replace_na(replace = list(next_mode = "none")) %>% 
  ungroup() %>% 
  count(Mode, next_mode) %>% 
  group_by(Mode) %>% 
  mutate(total_starts = sum(n),
         prob = n/total_starts)

# plot geom_tile x and y axises are first and next mode. 
d %>% 
  expand(mode, next_mode)
d %>%  ggplot(aes(x = mode, y = next_mode)) +
  geom_tile(aes(fill=prob), color = "white") +
  theme_minimal()

# pitch count transition, markov chain visualization. 
###############

# column for starting mode and ending more
origin_case4 <- origin_case2 %>% 
  group_by(new_ID) %>% 
  mutate(next_mode=lead(Mode)) %>% 
  replace_na(replace = list(next_mode = "none")) %>% 
  ungroup() %>% 
  count(Mode, next_mode) %>% 
  group_by(Mode) %>% 
  mutate(total_starts = sum(n),
         prob = n/total_starts)

fct_explicit_na(origin_case4$next_mode, na_level = "End") # did it really do anything
#

# new df for departments
origin_case5 <- origin_case2 %>% 
  group_by(new_ID) %>% 
  mutate(next_dept=lead(Dept)) %>% 
  replace_na(replace = list(next_dept = "none")) %>% 
  ungroup() %>% 
  count(Dept, next_dept) %>% 
  group_by(Dept) %>% 
  mutate(total_starts = sum(n),
         prob = n/total_starts)

write.csv(origin_case5, file = "prob_cases_dept.csv")

# violin plots of starting states
# Mode <- c("Chat", "Phone", "In Person", "Email", "Webform", "Zoom")
# total_starts <- c(23, 41, 32, 58, 26)
# new_data <- origin_case2
# levels(new_data$Students) <- list("Graduate" = "Graduate",        # Change factor levels
#                                "Undergraduate" = "Undergraduate",
#                                "NA" = "Not Students")
## overall pct of modes
mode_pct <- origin_case2 %>% 
  group_by(Mode) %>% 
  drop_na(Mode) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

mode_pct <- 
  ggplot(mode_pct, aes(Mode, 
                           count, fill = Mode)) +
  geom_bar(stat='identity') +
  labs(x = "Mode", y = "Number of Cases") +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.04*max(count))))

mode_pct <- mode_pct + labs(title = "Number of Cases", 
                              subtitle = "full data from 5/2/2020 to 9/14/2021",  fill = "Mode")

print(mode_pct)

# pct of modes for students only 
student_mode_pct <- student_data2 %>% 
  group_by(Mode) %>% 
  drop_na(Mode) %>% 
  summarize(count1 = n()) %>%  # count records by species
  mutate(pct1 = count1/sum(count1))

student_mode_pct <- 
  ggplot(student_mode_pct, aes(Mode, 
                       count1, fill = Mode)) +
  geom_bar(stat='identity') +
  labs(x = "Mode", y = "Number of Cases") +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct1), y = if_else(count1 > 0.1*max(count1), count1/2, count1+ 0.04*max(count1))))

student_mode_pct <- student_mode_pct + labs(title = "Number of Cases", 
                            subtitle = "students only from 5/2/2020 to 9/14/2021",  fill = "Mode")

print(student_mode_pct)
#### stacked bar graph

Mode_graph <- origin_case2 %>% 
  pivot_longer(Mode, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..)) +
  labs(x = "Mode", y = "Count") 

Mode_graph <- Mode_graph + labs(title = "Cases per Mode",
                              # subtitle = "", 
                              fill = "Status") +
                              scale_fill_discrete(
                              # breaks=c("Graduate", "Undergraduate", "NA"),
                              labels=c("Graduate", "Undergraduate", "Not Students"))

print(Mode_graph)

# dept graph
Dept_graph <- origin_case2 %>% 
  pivot_longer(Dept, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Students)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..)) +
  # geom_boxplot() +
  labs(x = "Department", y = "Count") 

Dept_graph <- Dept_graph + labs(title = "Cases per Department",
                                # subtitle = "", 
                                fill = "Status") +
                                scale_fill_discrete(
    # breaks=c("Graduate", "Undergraduate", "NA"),
                                labels=c("Graduate", "Undergraduate", "Not Students"))

print(Dept_graph)

# create time of day for cases
CreatedDate_graph0 <- ggplot(CreatedDate_out, aes(y = tm1.dechr)) +
  geom_boxplot() +
  # ylim(0, 24) +
  labs(x="Overall", y="Hour") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") 

CreatedDate_graph0 <- CreatedDate_graph0 + labs(title = "Chat Time of Day") +
  scale_y_continuous(breaks=seq(0,24,4))

print(CreatedDate_graph0)

## convert time to decimals
tm1.dechr <- hour(left_cases$AZ_time) + minute(left_cases$AZ_time)/60 + second(left_cases$AZ_time)/3600
left_cases <- dplyr::mutate(left_cases, tm1.dechr) 

violin_data <- origin_case2 %>%
  group_by(Mode) %>%
  mutate(Mode_start = n()) 

mode_violin <- ggplot(violin_data, aes(Mode, Mode_start)) + 
  geom_point(scale = "count") + 
  # geom_jitter(height = 0, width = 0.1)
mode_violin

# remove unused dataframes
rm("origin_cases2","origin_cases3")
## graph in R
library(tidyverse)
library(tidygraph)
library(ggraph)

d <- tibble(user = c("user1", "user2", "user2", "user2", "user2", "user2", "user2"),
            mode = c("a", "a", "b", "c", "a", "c", "c")) %>% 
  group_by(user) %>% 
  mutate(next_mode = lead(mode)) %>% 
  replace_na(replace = list(next_mode = "none")) %>% 
  ungroup() %>% 
  count(mode, next_mode) %>% 
  group_by(mode) %>% 
  mutate(total_starts = sum(n),
         prob = n/total_starts)

d %>% 
  ggplot(aes(x = mode, y = next_mode)) +
  geom_tile(aes(fill = prob), color = "white") +
  theme_minimal()

tbl_graph(edges = d) %>% 
  ggraph(layout = "kk") +
  geom_edge_loop(aes(label = round(prob, 2)), 
                 arrow = arrow(),
                 start_cap = circle(5, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = "along",
                 label_dodge = unit(2, "mm")) +
  geom_edge_link(aes(label = round(prob, 2)), 
                 arrow = arrow(), 
                 start_cap = circle(5, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = "along",
                 label_dodge = unit(2, "mm")) +
  geom_node_label(aes(label = name), size = 4, alpha = 0.5) +
  theme_bw()

##
origin_case4$next_mode <- as.character(origin_case4$next_mode)
origin_case4$next_mode[is.na(origin_case4$next_mode)] <- "End"
origin_case4$next_mode <- as.factor(origin_case4$next_mode)

origin_case4$ord_mode <- factor(origin_case4$next_mode, 
                             ordered=TRUE, levels = c("Chat", "Phone", "In Person", 
                                                      "Email", "Webform", "Zoom", "End"))

Mode_tile <-
  ggplot(data=subset(origin_case4, !is.na(Mode)), aes(y = Mode, x = ord_mode)) +
  geom_tile(aes(fill=prob), color = "white") +
  # scale_fill_manual(values=colors) +
  labs(title = "Likelihood of Moving to the Another Mode",
       x = "Next Mode", y = "Starting Mode") +
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c",
                      guide = "colorbar") 
  theme_minimal()

Mode_tile

write.csv(origin_case4, file = "prob_cases_mode.csv")
# department data, change next_dept NA into End
origin_case5$next_dept <- as.character(origin_case5$next_dept)
origin_case5$next_dept[is.na(origin_case5$next_dept)] <- "End"
origin_case5$next_dept <- as.factor(origin_case5$next_dept)

origin_case5$ord.x <- factor(origin_case5$next_dept, 
                             ordered=TRUE, levels = c("24/7", "Financial Aid", "Other", 
                                                      "Registrar", "SOS", "SECD", "End"))
# department tile
# Can put in !is.na(next_dept) to eliminate all NA
Dept_tile <-
  ggplot(data=subset(origin_case5, !is.na(Dept)), aes(y = Dept, x = ord.x)) +
  geom_tile(aes(fill=prob), color = "white") +
  # scale_fill_manual(values=colors) +
  labs(title = "Likelihood of Moving to the Another Department",
       x = "Next Department", y = "Starting Department") +
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c",
                      guide = "colorbar") 
# theme_minimal()

Dept_tile

### building a network loop graph

tbl_graph(edges = origin_case4) %>% 
  ggraph(layout = "kk") +
  geom_edge_loop(aes(label = round(prob, 2)), 
                 arrow = arrow(),
                 start_cap = circle(5, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = "along",
                 label_dodge = unit(2, "mm")) +
  geom_edge_link(aes(label = round(prob, 2)), 
                 arrow = arrow(), 
                 start_cap = circle(5, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = "along",
                 label_dodge = unit(2, "mm")) +
  geom_node_label(aes(label = name), size = 4, alpha = 0.5) +
  theme_bw()

# sandbox version of graph
tbl_graph(edges = origin_case5) %>% 
  ggraph(layout = "kk") +
  geom_edge_loop(aes(label = round(prob, 2)), 
                 arrow = arrow(),
                 start_cap = circle(5, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = "along",
                 label_dodge = unit(2, "mm")) +
  geom_edge_link(aes(label = round(prob, 2)), 
                 arrow = arrow(), 
                 start_cap = circle(5, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = "along",
                 label_dodge = unit(2, "mm")) +
  geom_node_label(aes(label = name), size = 4, alpha = 0.5) +
  theme_bw()
# %>% 
#   filter(!is.na(next_Mode))

#   mutate(starting = ) #group_by starting point and ending point, count that N
# 
# count(staring, ending) %>% 
#   group_by(starting) %>% 
#   mutate(total_starts = sum(n),
#          prob = n/total_starts)
# 
# origin_case3 %>% 
#   group_by()
# # I have some transition from A to B


# department names that can be consolidated
table(origin_case2$Department__c)

# origin_case3 <- subset(origin_case2, select = c(new_ID, Students, Department__c, Cumulative_GPA__c, 
#                                                 period, Origin, COUNT,Created_Day_of_Week__c,
#                                                 AZ_date4, AZ_time4, AZ_time, CaseNumber))
# 

# recode factor for departments
origin_case2$Dept = recode_factor(origin_case2$Department__c, 
                                      "24/7" = "24/7",
                                      "Advising Resource Center" = "Other",
                                      "ASTEC" = "Other",
                                      "Bursar" = "Other",
                                      "College of Engineering" = "Other",
                                      "College of Law" = "Other",
                                      "Dean of Students\xa0" = "Other",
                                      "Financial Aid" = "Financial Aid",
                                      "Libraries" = "Other",
                                      "LifeLab" = "Other",
                                      "Mathematics" = "Other",
                                      "PSYCH" = "Other",
                                      "Registrar" = "Registrar",
                                      "SOS" = "SOS",
                                      "Student Engagement and Career Development" = "SECD",
                                      "Study Abroad" = "Other",
                                      "Think Tank" = "Other",
                                      "Trellis Internal" = "Other",
                                      "University Services" = "Other",
                                      "VETS" = "Other")

table(origin_case2$Dept) # this creates 6 categories

table(origin_case2$Origin) # combine online and offline chat, combine phone to text
origin_case2$Mode = recode_factor(origin_case2$Origin, 
                                  "Offline Chat" = "Chat",
                                  "Text" = "Phone") # SOS uses offline chat
table(origin_case2$Mode) # now also 6 by 6

write.csv(origin_case2, file = "origin_case2.csv")

# # # new sample data
# rand_df <- origin_case4[sample(nrow(origin_case4), size=53), ]
# out <- origin_cases[origin_case4$new_ID %in% c(309866,848328,2392492,4156525,6435057,6668820976), ]
# out <- subset(origin_case4, new_ID %in% c(309866,848328,2392492,4156525,6435057,6668820976))
# 
# everything <-rbind(out, rand_df)
# write.csv(everything, file = "sample_data_time.csv")
origin_case2$new_ID <- as.factor(origin_case2$new_ID)

# remove unused variables
origin_case2 <- subset(origin_case2, select = -c(units,Career__c.x, OwnerId.x, OwnerId.y))

# out_plot3 <- origin_case2 %>% 
#   # pivot_longer(index) %>% 
#   filter(seqnum >= 70) %>% 
#   ggplot(aes(x=AZ_date4, y=Origin, group=new_ID)) +
#   geom_point(aes(colour=new_ID)) + 
#   geom_line(aes(colour=new_ID)) +
#   geom_dl(aes(label=new_ID), method="last.points") # or "first.points"
# 
# out_plot3 <- out_plot3 + geom_label_repel(aes(label = new_ID),
#                                           box.padding   = 0.35, 
#                                           point.padding = 0.5,
#                                           segment.color = 'grey50') 
# print(out_plot3)

# TPM
library(flexsurv)
library(mstate)
library(hesim)
library(markovchain) 

# fit model on the data by ML
library(MSGARCH)
fit <- FitML(spec = Mode, data = origin_case2)

# 6 by 6 mstate
# tmat <- mstate::transMat(x = list(c(2, 3, 5, 6),
#                                   c(4, 5, 6),
#                                   c(4, 5, 6),
#                                   c(5, 6),
#                                   c(),
#                                   c()),
#                          names = c("24/7", "Other", "Financial Aid", "Registrar",
#                                    "SOS", "SECD"))
# print(tmat)
# 
# mode <- origin_case3$Mode
# n <- length(mode)
# modedf <- as.factor(ifelse(mode,0,1)) #Error in storage.mode(test) <- "logical" : 
# # invalid to change the storage mode of a factor
# trf <- table(data.frame(yesterday=modef[1:(n-1)],today=modedf[2:n]))
# trf/rowSums(trf)
