#Load packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("assessments_with_tags.RData")
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

#Compute answer score & Verify (Keep 0 as NA, for jaccard to function)
question_data2 = as.data.table(question_data1 %>% mutate(latest_answer = case_when(points_earned == 10 ~ 1, TRUE ~ 0)))
table(question_data2$points_earned, question_data2$latest_answer)
question_data2[latest_answer == 0, latest_answer := NA]
head(question_data2)

#Save Question Master
save(question_data2, file="Master\\Question_Master.RData")

#Extract specific country data - Ex: IR data
question_data3 = question_data2[question_data2$country == 'IR',]
head(question_data3)

#Build user~question~answerscore matrix - CF Matrix - Using Barry's lib
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day1\\CF-demolib-v3.R")
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day3\\CF-demolib-implict-v2.R")

users = acast(question_data3, masked_user_id ~ question_id, value.var = "latest_answer")
dim(users)
users[1:15,1:20]

#Testing the approach - Jaccard is Best for both UU and II
# Anything empty
fillrate(users)
  #Verify answer %
  unlist_users = unlist(users)
  attempted = 'NA' #Scored
  answerRate = length(which(unlist_users != attempted))/length(unlist_users) ; cat("% of questions answered=",answerRate*100,"%")

# Target User - Get recommendations based on user-user and item-item
users[1:5,1:10]
target_u = users["026ed114", ]
length(which(!is.na(target_u)));length(which(is.na(target_u)))  # questions answered & unanswered by target 

# User-User based CF
getrecommendations_UUB(target_u, users, simfun=jacardsim, topN = 50)

# Item-Item based CF
  
# Build sim matrix
  itemsims = getitemsimsmatrix(users, simfun=jacardsim)  
  fillrate(itemsims)
  itemsims[1:10,1:5]

getrecommendations_IIB(target_u, itemsims, topN = 50)


#Test Models##
# set-up the train/test sets
numtestusers = 40
testnames  = sample(rownames(users), min(numtestusers,nrow(users))) 
trainnames = setdiff(rownames(users),testnames) 
trainusers = users[trainnames,]
testusers  = users[testnames,]
dim(users); dim(trainusers); dim(testusers)


# Testing User-based CF
# predicting the ratings won't work, it will always return 1
preds = predictCF(testusers, trainusers=trainusers, numtestitems=2, random=FALSE, simfun=jacardsim)
preds
cat("avg MAE =",avgMAE(preds))  #0 because binary data

# instead use similiarity to identify recommendations (see fn def for evalrecs)
topN = 10
recsU = evalrecs(testusers, trainusers=trainusers, numtestitems=10, simfun=jacardsim, topN=topN) # UU
cat("UU: hits=",meanHR(recsU)*100,"%","MPR=",meanPR(recsU),"%") # show hit-rate and percentage-rank

rtestusers = do.call("rbind", replicate(20, testusers, simplify = FALSE)) # duplicate test egs to get more accurate result
recsR = evalrecs(rtestusers, numtestitems=500, topN=topN) # random
cat("RD: hits=",meanHR(recsR)*100,"%","MPR=",meanPR(recsR),"%")

# compute the lift over random
cat("lift=", meanHR(recsU)/meanHR(recsR))


# Testing Item-Item based CF
itemsims_t = getitemsimsmatrix(trainusers, simfun=jacardsim)
recsI = evalrecs(testusers, itemsims=itemsims_t, numtestitems=10, topN=topN) # II
cat("II: hits=",meanHR(recsI)*100,"%","MPR=",meanPR(recsI),"%")
cat("lift=", meanHR(recsI)/meanHR(recsR))

#PR
mean(unlist(recsI[(nrow(recsI)/2+1):nrow(recsI),]),na.rm=TRUE)
#HR
mean(unlist(recsI[1:nrow(recsI)/2,]),na.rm=TRUE)


#Provide formatted recommendations to GUI
unique(question_data3[question_data3$masked_user_id == '026ed114',9:15])
UserBasedQRec = FormatRec(getrecommendations_UUB(target_u, users, simfun=jacardsim, topN = 50), question_data3);UserBasedQRec
FormatRec(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3)


FormatRec = function(recommendations, masterdata) {
  temp = as.data.frame(recommendations)
  temp$qid = rownames(temp)
  temp$qid = as.integer(temp$qid) 
  rownames(temp) = NULL
  colnames(temp)[1] <-"predscore"
  temp[,c("qid","predscore")]
  temp2 = data.table(unique(masterdata[masterdata$question_id %in% c(temp$qid),c(1,9:15)]))
  merge(temp2, temp, by.x = "question_id", by.y = "qid", all.x = T) %>% arrange(-predscore) %>% distinct()
}



# Load Stream Data

stream_data_4_rec_tags = merge(stream_data[stream_data$country == 'IR',], UserBasedQRec, by.x = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), 
                               by.y = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), all.y = TRUE) %>%
  select(Stream = deck_id, QuestionId = question_id, Tag1 = tag1, Tag2 = tag2, Tag3 = tag3,
         Tag4 = tag4, Tag5 = tag5, Tag6 = tag6, Tag7 = tag7, PredScore = predscore) %>% 
  arrange(-PredScore) %>% distinct()



