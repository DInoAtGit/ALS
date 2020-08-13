#Load packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("user_assessments_with_tags.RData")
dim(question_data6)
question_data = question_data6; rm(question_data6)
head(question_data)


#Convert to Date
question_data$submission_utc_ts = as.POSIXct(question_data$submission_utc_ts, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
str(question_data)

#extract latest attempt data
#setDT(question_data)[, .SD[which.max(submission_utc_ts)], by=question_id]
question_data1 = as.data.frame(question_data %>% group_by(question_id,masked_user_id) %>% slice(which.max(submission_utc_ts)))
question_data1[question_data1$question_id == 6393 & question_data1$masked_user_id == '2211133a',]

#Remove duplicates
question_data1 = unique(question_data1)

#Verify columns
str(question_data1)
cols = c('country','role_id','org_id','tag1', 'tag2','tag3','tag4','tag5','tag6','tag7')
question_data1[,cols] = lapply(question_data1[,cols], factor)
barplot(table(question_data1$role_id, useNA = "ifany"))
barplot(table(question_data1$tag5, useNA = "ifany"))
head(question_data1,4)

#Compute answer score
question_data2 = question_data1 %>% mutate(latest_answer = case_when(points_earned == 10 ~ 1, TRUE ~ 0))
head(question_data2)

#Extract specific country data - Ex: IR data
question_data3 = as.data.table(question_data2[question_data2$country == 'IR',])
head(question_data3)

#Build user~question~answerscore matrix - CF Matrix - Using Barry's lib
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day1\\CF-demolib-v3.R")

users = acast(question_data3, masked_user_id ~ question_id, value.var = "latest_answer")
dim(users)
users[1:10,1:15]

#Testing the approach - Jaccard is Best for both UU and II
fillrate(users)
likethreshold_m =1 #Viewed atleast once
likerate_m = length(which(unlist_users>=likethreshold_m))/length(unlist_users) ; cat("% of decks that are viewed=",likerate_m*100,"%")

# get recommendations for U2 (results with pearson shd be: 3.35 (night), 2.83 (lady), 2.53 (luck))
users[1:2,1:10]
target_u = users["00f76716",]
getrecommendations_UU(target_u, users, simfun=jacardsim)

#Build similarity functions
itemsimsJ_u = getitemsimsmatrix(users, simfun = jacardsim); itemsimsJ_u[1:10, 1:10]  #Jaccard similarity matrix

#Getrecommendations Item-Item 
getrecommendations_II(target_u, itemsimsJ_u)   # using jaccard similarity

#System evalution
numtestusers = 10
test_users_names = sample(rownames(users), min(numtestusers, nrow(users))); test_users_names
train_users_names = setdiff(rownames(users), test_users_names); head(train_users_names,10)
train_users = users[train_users_names,]
test_users = users[test_users_names,]
nrow(users);nrow(train_users);nrow(test_users)

#Prediction using UU
predquestion = predictCF(test_users, train_users, numtestitems = 10, random = FALSE, simfun = jacardsim);predquestion #Jaccard

#Evaluation / Confusion Matrix
cat("Avg UU-based MAEs { For Jaccard: ", avgMAE(predquestion), " }")
showCM(predquestion, like = 0)

#Prediction using II
itemsimsJ = getitemsimsmatrix(train_users, simfun = jacardsim)
preddeck_J = predictCF(test_users, itemsims = itemsimsJ, numtestitems = 10, random = FALSE); preddeck_J

#Evaluation / Confusion Matrix
cat("Avg II-based MAEs { For Jaccard: ", avgMAE(preddeck_J), " }")


