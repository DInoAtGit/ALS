#Load packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("assessments_with_tags.RData")
question_data = question_data6; rm(question_data6)

#Convert to Date
question_data$submission_utc_ts = as.POSIXct(question_data$submission_utc_ts, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#extract latest attempt data
#setDT(question_data)[, .SD[which.max(submission_utc_ts)], by=question_id]
question_data1 = as.data.frame(question_data %>% group_by(question_id,masked_user_id) %>% slice(which.max(submission_utc_ts)))

#Factorize columns
cols = c('country','role_id','org_id','tag1', 'tag2','tag3','tag4','tag5','tag6','tag7')
question_data1[,cols] = lapply(question_data1[,cols], factor)

#Compute answer score & Verify (Keep 0 as NA, for jaccard to function)
question_data2 = as.data.table(question_data1 %>% mutate(latest_answer = case_when(points_earned == 10 ~ 1, TRUE ~ 0)))
question_data2[latest_answer == 0, latest_answer := NA]

#Save Question Master
save(question_data2, file="Master\\Question_Master.RData")

#Remove unwanted transient files
rm(question_data, question_data1, cols)
