#Source Question Master & Other libraries
source("C:\\Dino\\Git\\ALS\\ALS\\KD\\QuestionMasterPrep.R")
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day1\\CF-demolib-v3.R")
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day3\\CF-demolib-implict-v2.R")


#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("streams_with_tags.RData")
stream_data = stream_data4; rm(stream_data4)

#Extract specific country data - Ex: IR data
question_data3 = question_data2[question_data2$country == 'IR',]

#Build user~question~answerscore matrix - CF Matrix
users = acast(question_data3, masked_user_id ~ question_id, value.var = "latest_answer")

#Recommend given user_id
target_u = users["026ed114", ]

# Build sim matrix
itemsims = getitemsimsmatrix(users, simfun=jacardsim)  

# Function to format & return recommendataions
FormatRec = function(recommendations, question_master, stream_master) {
  temp = as.data.frame(recommendations)
  temp$qid = rownames(temp)
  temp$qid = as.integer(temp$qid) 
  rownames(temp) = NULL
  colnames(temp)[1] <-"predscore"
  temp[,c("qid","predscore")]
  temp2 = data.table(unique(question_master[question_master$question_id %in% c(temp$qid),c(1,9:15)]))
  temp3 = merge(temp2, temp, by.x = "question_id", by.y = "qid", all.x = T) %>% arrange(-predscore) %>% distinct()
  merge(stream_master[stream_master$country == 'IR',], temp3, by.x = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), 
        by.y = c("tag1","tag2","tag3","tag4","tag5","tag6","tag7"), all.y = TRUE) %>%
    select(Stream = deck_id, QuestionId = question_id, Tag1 = tag1, Tag2 = tag2, Tag3 = tag3,
           Tag4 = tag4, Tag5 = tag5, Tag6 = tag6, Tag7 = tag7, PredScore = predscore) %>% filter(!is.na(Stream)) %>%
    arrange(-PredScore) %>% distinct()
}

FormatRec(getrecommendations_UUB(target_u, users, simfun=jacardsim, topN = 50), question_data3,stream_data)
FormatRec(getrecommendations_IIB(target_u, itemsims, topN = 50), question_data3,stream_data)

