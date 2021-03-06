#Source Question Master & Other libraries
source("C:\\Dino\\Git\\ALS\\ALS\\KD\\QuestionMasterPrep.R")
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day1\\CF-demolib-v3.R")
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day3\\CF-demolib-implict-v2.R")


#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("streams_with_tags.RData")
stream_data = stream_data4; rm(stream_data4)
str(stream_data)
stream_data$user_action_d = as.Date(stream_data$user_action_timestamp, "%Y-%m-%d")

#Capture Logged in User
LoginUser = "97d0a65c"
LoginUserRole = 1
LoginCntry = "IN"

#Get Most recent Country Code for the User from Stream or Question Activity
UserStreamCntry = as.data.table(stream_data[stream_data$masked_user_id == LoginUser,] %>% group_by(country) %>% summarise(recent = max(user_action_d)))
UserQuestionCntry = as.data.table(question_data2[question_data2$masked_user_id == LoginUser,] %>% group_by(country) %>% summarise(recent = max(submission_utc_ts)))
if(as.Date(UserStreamCntry$recent) >= as.Date(UserQuestionCntry$recent)){
  LoginUserCntry = as.character(UserStreamCntry$country)
} else{
  LoginUserCntry = as.character(UserQuestionCntry$country)
}

#If User is new
question_data2[question_data2$role_id == LoginUserRole & question_data2$country == LoginCntry, ]


#Extract specific country data - Ex: IR data
question_data3 = question_data2[question_data2$country == LoginUserCntry,]

#Build user~question~answerscore matrix - CF Matrix
users = acast(question_data3, masked_user_id ~ question_id, value.var = "latest_answer")

#Recommend given user_id
target_u = users[LoginUser, ]

# Build sim matrix
itemsims = getitemsimsmatrix(users, simfun=jacardsim)  

# Function to format & return recommendataions
FormatRec = function(recommendations, question_master, stream_master, Cntry) {
  temp = as.data.frame(recommendations)
  temp$qid = rownames(temp)
  temp$qid = as.integer(temp$qid) 
  rownames(temp) = NULL
  colnames(temp)[1] <-"predscore"
  temp[,c("qid","predscore")]
  temp2 = data.table(unique(question_master[question_master$question_id %in% c(temp$qid),c(1,9:15)]))
  temp3 = merge(temp2, temp, by.x = "question_id", by.y = "qid", all.x = T) %>% arrange(-predscore) %>% distinct()
  merge(stream_master[stream_master$country == Cntry,], temp3, by.x = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), 
        by.y = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), all.y = TRUE) %>%
    select(Stream = deck_id, QuestionId = question_id, Tag1 = tag1, Tag2 = tag2, Tag3 = tag3,
           Tag4 = tag4, Tag5 = tag5, Tag6 = tag6, Tag7 = tag7, PredScore = predscore) %>% filter(!is.na(Stream)) %>%
    arrange(-PredScore) %>% distinct()
}

FormatRec(getrecommendations_UUB(target_u, users, simfun=jacardsim, topN = 50), question_data3,stream_data,LoginUserCntry)
FormatRec(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data,LoginUserCntry)


question_data3[question_data3$masked_user_id == '97d0a65c' & question_data3$country == 'IN',]

