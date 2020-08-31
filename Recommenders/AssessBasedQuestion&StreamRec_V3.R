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


# Function to format & return recommendataions - Only for Demo
FormatRec_Demo = function(recommendations, question_master, stream_master, Cntry, What2Rec) {
  temp = as.data.frame(recommendations)
  temp$qid = rownames(temp)
  temp$qid = as.integer(temp$qid) 
  rownames(temp) = NULL
  colnames(temp)[1] <-"predscore"
  temp[,c("qid","predscore")]
  temp2 = data.table(unique(question_master[question_master$question_id %in% c(temp$qid),c(1,9:15)]))
  temp2.1 = data.table(unique(question_master[question_master$question_id %in% c(temp$qid),c(2,1)]))
  temp3 = merge(temp2, temp, by.x = "question_id", by.y = "qid", all.x = T) %>% arrange(-predscore) %>% distinct()
  if (What2Rec == 'Q'){
    merge(temp2.1, temp, by.x = "question_id", by.y = "qid", all.x = T) %>% 
      select(country, question_id, PredScore = predscore) %>%
      arrange(-PredScore) %>% distinct()
  } else if (What2Rec == 'S'){
      merge(stream_master[stream_master$country == Cntry,], temp3, by.x = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), 
          by.y = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), all.y = TRUE) %>%
      select(country, deck_id, PredScore = predscore) %>% filter(!is.na(deck_id)) %>%
      arrange(-PredScore) %>% distinct()
  } 
}

FormatRec(getrecommendations_UUB(target_u, users, simfun=jacardsim, topN = 50), question_data3,stream_data,LoginUserCntry)
FormatRec(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data,LoginUserCntry)


question_data3[question_data3$masked_user_id == '97d0a65c' & question_data3$country == 'IN',]

FormatRec_Demo(getrecommendations_UUB(target_u, users, simfun=jacardsim, topN = 50), question_data3,stream_data,LoginUserCntry,'Q')
FormatRec_Demo(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data,LoginUserCntry,'Q')

FormatRec_Demo(getrecommendations_UUB(target_u, users, simfun=jacardsim, topN = 50), question_data3,stream_data,LoginUserCntry,'S')
FormatRec_Demo(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data,LoginUserCntry,'S')


#create empty table with strucure country, userid, deckid/questionid and predscore
streamdemords = data.frame(country=factor(),deck_id=character(),predscore=double(),masked_user_id=factor()) 
quesitiondemords = data.frame(country=factor(),question_id=character(),predscore=double(),masked_user_id=factor()) 
demousers = c("97d0a65c","b1459d23","c80bffb2","c930cc66","f10f490e","f810564e")
  # u = "97d0a65c"
  # userdf = data.frame(lapply(u, rep, 50))
  # colnames(userdf)[1] <-"masked_user_id"
  # streamdemords = rbind(cbind(streamdemords, userdf, FormatRec_Demo(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data,LoginUserCntry,'S')))
for (u in demousers){
  userdf = data.frame(lapply(u, rep, 50))
  colnames(userdf)[1] <-"masked_user_id"
  tempdf_s = cbind(as.data.frame(FormatRec_Demo(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data,LoginUserCntry,'S')),userdf)
  streamdemords = rbind(streamdemords, tempdf_s)
  tempdf_q = cbind(as.data.frame(FormatRec_Demo(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data,LoginUserCntry,'Q')),userdf)
  quesitiondemords = rbind(quesitiondemords, tempdf_q)
}
streamdemords %>% group_by(masked_user_id) %>% summarise(count = n())
quesitiondemords %>% group_by(masked_user_id) %>% summarise(count = n())

streamdemords = streamdemords %>% select(country, masked_user_id, deck_id, predicted = PredScore)
quesitiondemords = quesitiondemords %>% select(country, masked_user_id, question_id, predicted = PredScore)

save(streamdemords, file="C:\\Dino\\NUS\\CapStone\\DataSet\\Demo\\strength-demo-stream")
save(quesitiondemords, file="C:\\Dino\\NUS\\CapStone\\DataSet\\Demo\\strength-demo-assess")
