#Load packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr,magrittr,tm,wordcloud,RColorBrewer,recosystem,
               topicmodels,SnowballC,lsa,slam,proxy,rjson)


#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("assessments_with_tags.RData")
dim(question_data6)
head(question_data6, 4)

#Load data
load("streams_with_tags.RData")
dim(stream_data4)
head(stream_data4, 4)

#Focus only GB Data
question_data = question_data6[question_data6$country == 'GB',]
stream_data = stream_data4[stream_data4$country == 'GB',]
rm(question_data6,stream_data4)

#Combine all tags for feature matrix
question_data$all_tags = paste0(question_data$tag1," ",
                                question_data$tag5," ",
                                question_data$tag6)

sapply(question_data, function(col) sum(is.na(col)))

#Build Question-Tag feature matrix
question_master = unique(question_data[,c(1,9,13:14)])
sapply(question_master, function(col) sum(is.na(col)))
apply(question_master, 2, function(x) length(unique(x)))
question_master[duplicated(question_master) == TRUE, ]
question_master$all_tags = paste0(question_master$tag1," ",
                                  question_master$tag5," ",
                                  question_master$tag6)
corpus <- Corpus(VectorSource(question_master$all_tags))
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightBin))
inspect (dtm)
# dtm_ti <- weightTfIdf(dtm)
# inspect(dtm_ti)
dt <- as.data.table(as.matrix(dtm));dt[1:20,1:10]
dt <- cbind(question_id = question_master$question_id, dt);dt[1:2,1:20]
#save(dt, file = "dt.RData")

#Number of questions/streams for matrixfactorization
topNQs = 50 #Number of related question data
serQs = 2

#Users in the whole data
length(unique(question_data$masked_user_id))
users_list = unique(question_data$masked_user_id)

for (usr in users_list){
  temp_data = question_data[question_data$masked_user_id == usr,] #'263c72d4'
  if (nrow(temp_data[temp_data$no_of_trials== 1,]) >= 1){
    temp_tags = temp_data[temp_data$no_of_trials == 1,] %>% group_by(all_tags) %>% 
      summarise(score = sum(points_earned)) %>% arrange(-score)  %$% all_tags
  } else {
    temp_tags = temp_data %>% group_by(all_tags) %>% 
      summarise(score = sum(points_earned)) %>% arrange(-score)  %$% all_tags
  }
  temp_tags_1 =  data.frame(tags = unlist(strsplit(temp_tags, " ")))
  strength_tags = unique(temp_tags_1[temp_tags_1$tags != 'NA',]) %>% head(10)
  newMat <- data.table("USER STENGTHS" , matrix(0, nrow = 1, ncol = ncol(dt)-1))
  newMat[, which(colnames(dt) %in% strength_tags)] <- 1
  colnames(newMat) <- colnames(dt)
  newMat <- as.data.frame(newMat)[, strength_tags]
  distances <- c(dist(newMat, as.data.frame(dt)[, strength_tags], method = "cosine"))
  choiceUniverse <- data.table(question_id = dt$question_id, dist = distances) %>% arrange(dist) %>% as.data.table %>% head(topNQs)
  question_data_2 <- merge(temp_data, choiceUniverse, by.x = "question_id", by.y = "question_id", all.y = T)
  ser_qlist <- question_data %>% dplyr::filter(!question_id %in% choiceUniverse$question_id) %$% question_id %>% unique
  set.seed(2); ser_questions <- question_data %>% dplyr::filter(question_id %in% sample(ser_qlist, serQs))
  ser_questions$dist =  max(question_data_2$dist) + (1/100)
  question_data_3 = rbind(question_data_2, ser_questions)
  question_data_4 = rbind(question_data_4, question_data_3)
  #question_data_3 =question_data_3[question_data_3$masked_user_id == 'Dino',]; question_data_4= question_data_3
}
question_data_5 = unique(question_data_4)


#Devide the data into 3 parts (70:15:15) based on the date
question_data_5$user_action_d = as.Date(question_data_5$submission_utc_ts, "%Y-%m-%d")
barplot(table(format(question_data_5$user_action_d, "%Y-%b")))
as.data.frame(table(format(question_data$user_action_d, "%Y-%b")))
trainset <- question_data[question_data$user_action_d < '2019-10-15', ];nrow(trainset)
testset <- question_data[question_data$user_action_d >= '2019-10-15' & question_data$user_action_d < '2019-12-01', ];nrow(testset)
validset <- question_data[question_data$user_action_d >= '2019-12-01', ];nrow(validset)
nrow(trainset);nrow(testset);nrow(validset)

#Are they present in test and validation data sets
length(unique(testset[testset$masked_user_id %in% c(users_list), c(masked_user_id)]))
length(unique(validset[validset$masked_user_id %in% c(users_list), c(masked_user_id)]))

# length(unique(trainset_3$masked_user_id))
# table(trainset_4$masked_user_id, trainset_4$question_id)
# trainset_4[trainset_4$masked_user_id == '00a70dfb',]
# dt[,which(colnames(dt) %in% strength_tags)]
# dt[,c(10,16,45,122,127,128,131,132)]
# dt[,tv]
# question_master[question_master$tag7 == 'tv',]
# dt[,tonometer]
# question_master[question_master$tag7 == 'tonometer',]

# CF using reco
# Model evaluation on non-CB data
set.seed(20000)
question_data$user_action_d = as.Date(question_data$submission_utc_ts, "%Y-%m-%d")
ratingsMatrixTrain <- question_data %>% select(user = masked_user_id, item = question_id, ratings = points_earned, 9:15,17)
ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)
trainset <- ratingsMatrixTrain[ratingsMatrixTrain$user_action_d < '2019-11-10', ];nrow(trainset)
testset <- ratingsMatrixTrain[!ratingsMatrixTrain$user_action_d < '2019-11-10', ];nrow(testset)
train_data <- data_memory(user_index = trainset$user, item_index = trainset$item, rating = trainset$ratings, index1= TRUE)
test_data <- data_memory(user_index = testset$user, item_index = testset$item, rating = testset$ratings, index1= TRUE)
reco <- Reco()
reco$train(train_data, opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 50))
test_set = data_memory(testset$user, testset$item)
testset$pred <- reco$predict(test_set, out_memory())
RMSE.CF <- sqrt(mean(testset$pred - testset$ratings)^2);RMSE.CF
MAE.CF <- mean(abs(testset$pred - testset$ratings));MAE.CF


# Model evaluation on CB data
set.seed(2000)
trainset_4 = as.data.table(trainset_4 %>% mutate(corrected_dist = 1-dist))
trainset_4$points_earned_dist = trainset_4$points_earned * trainset_4$corrected_dist
ratingsMatrixTrain <- question_data_sub_2 %>% select(user = masked_user_id, item = question_id, ratings = points_earned_dist, 9:15,17:18)
ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)
uniqUsers <- length(unique(ratingsMatrixTrain$user));uniqUsers
uniqItems <- length(unique(ratingsMatrixTrain$item));uniqItems
trainset <- ratingsMatrixTrain[ratingsMatrixTrain$user_action_d < '2019-10-15', ];nrow(trainset)
testset <- ratingsMatrixTrain[!ratingsMatrixTrain$user_action_d < '2019-10-15', ];nrow(testset)
train_data <- data_memory(user_index = trainset$user, item_index = trainset$item, rating = trainset$ratings, index1= TRUE)
test_data <- data_memory(user_index = testset$user, item_index = testset$item, rating = testset$ratings, index1= TRUE)
reco <- Reco()
reco$train(train_data, opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 50))
test_set = data_memory(testset$user, testset$item)
testset$pred <- reco$predict(test_set, out_memory())
RMSE.CF <- sqrt(mean(testset$pred - testset$ratings)^2);RMSE.CF
MAE.CF <- mean(abs(testset$pred - testset$ratings));MAE.CF




#Simulation - Hold first round recommendataions
Demo_user_list = c('8a54a7e8','9bffe329','fa2f968d','4ffee38a','56888f9f','67d028d2')
CFrecommended$user_id = LoggedInUser
roundone_rec = CFrecommended[CFrecommended$question_id == 000,]
for (usr in Demo_user_list){ #usr = '8a54a7e8'
  closestUser_Index <- rep(which(levels(ratingsMatrixTrain$user) == usr), each = uniqItems)
  pred <- reco$predict(data_memory(closestUser_Index, 1:uniqItems), out_memory())
  tempfinalReco <- data.table(question_id = unique(ratingsMatrixTrain$item), scores = pred)
  question_data_sub_2$question_id = as.factor(question_data_sub_2$question_id)
  CFrecommended = merge(tempfinalReco, question_data_sub_2, by.x = "question_id", by.y = "question_id", all.x = T) %>% select(question_id = question_id, PredScore = scores, 10:16) %>% arrange(-PredScore) %>% distinct()
  temp = as.data.table(CFrecommended)
  temp$user_id = usr
  roundone_rec = rbind(roundone_rec, temp)
}
table(roundone_rec$user_id)
write.csv(roundone_rec, file = "Verify/RoundOneRec.csv")
roundone_rec_bkp = roundone_rec


#Simulation - Score recommended questions and add the simulated data into user assessement activity
Demo_user_list = c('8a54a7e8','9bffe329','fa2f968d','4ffee38a','56888f9f','67d028d2')
simulated_qdata = question_data[question_data$masked_user_id == 'Dino',]
#roundone_10 = rec_qlist[rec_qlist$user_id == 'Dino',]
for (usr in Demo_user_list){ #usr = '8a54a7e8'
  usr_qlist = roundone_rec[roundone_rec$user_id == usr, question_id] 
  rec_qlist = roundone_rec[roundone_rec$user_id == usr,] %>% filter(question_id %in% sample(usr_qlist, 30))
  #roundone_10 = rbind(roundone_10, rec_qlist)
  rec_qdata = question_data[question_data$masked_user_id != usr & question_data$question_id %in% rec_qlist$question_id,]
  rec_qdata_30 = rec_qdata[sample(nrow(rec_qdata), 30),]
  rec_qdata_30$masked_user_id = usr
  rec_qdata_30$points_earned = 10
  rec_qdata_30$submission_utc_ts = '2020-04-15 08:43:15.000'
  simulated_qdata = rbind(simulated_qdata, rec_qdata_30)
}
table(simulated_qdata$masked_user_id);table(roundone_rec$user_id)
question_data = rbind(question_data, simulated_qdata)

table(simulated_qdata$tag7)

write.csv(simulated_qdata, file = "Verify/SimulationData.csv")


#Simulation - Second round recommendataions
Demo_user_list = c('8a54a7e8','9bffe329','fa2f968d','4ffee38a','56888f9f','67d028d2')
CFrecommended$user_id = LoggedInUser
roundtwo_rec = CFrecommended[CFrecommended$question_id == 000,]
for (usr in Demo_user_list){ #usr = '8a54a7e8'
  closestUser_Index <- rep(which(levels(ratingsMatrixTrain$user) == usr), each = uniqItems)
  pred <- reco$predict(data_memory(closestUser_Index, 1:uniqItems), out_memory())
  tempfinalReco <- data.table(question_id = unique(ratingsMatrixTrain$item), scores = pred)
  question_data_sub_2$question_id = as.factor(question_data_sub_2$question_id)
  CFrecommended = merge(tempfinalReco, question_data_sub_2, by.x = "question_id", by.y = "question_id", all.x = T) %>% select(question_id = question_id, PredScore = scores, 10:16) %>% arrange(-PredScore) %>% distinct()
  temp = as.data.table(CFrecommended)
  temp$user_id = usr
  roundtwo_rec = rbind(roundtwo_rec, temp)
}
table(roundtwo_rec$user_id)
write.csv(roundtwo_rec, file = "Verify/RoundTwoRec.csv")
roundtwo_rec_bkp = roundtwo_rec




#Simulation - Score recommended questions and add the simulated data into user assessement activity
Demo_user_list = c('8a54a7e8','9bffe329','fa2f968d','4ffee38a','56888f9f','67d028d2')
roundone_rec = rec_qdata_10[rec_qdata_10$masked_user_id == 'Dino',]
roundone_10 = rec_qlist[rec_qlist$user_id == 'Dino',]
for (usr in Demo_user_list){ #usr = '8a54a7e8'
  usr_qlist = stregth_sample_recommendataions[stregth_sample_recommendataions$user_id == usr, question_id] 
  rec_qlist = stregth_sample_recommendataions[stregth_sample_recommendataions$user_id == usr,] %>% filter(question_id %in% sample(usr_qlist, 10))
  roundone_10 = rbind(roundone_10, rec_qlist)
  rec_qdata = question_data[question_data$masked_user_id != usr & question_data$question_id %in% rec_qlist$question_id,]
  rec_qdata_10 = rec_qdata[sample(nrow(rec_qdata), 10),]
  rec_qdata_10$masked_user_id = usr
  rec_qdata_10$points_earned = 10
  rec_qdata_10$submission_utc_ts = '2020-04-15 08:43:15.000'
  roundone_rec = rbind(roundone_rec, rec_qdata_10)
}
table(roundone_rec$masked_user_id);table(roundone_10$user_id)
question_data = rbind(question_data, roundone_rec)


rec_second = CFrecommended
rec_second[rec_second$question_id %in% c(2856,10131,10303,12882),]
round_two_strength = as.data.frame(question_data_sub %>% group_by(all_tags) %>%  summarise(score = sum(points_earned)) %>% filter(score >= 10))
rec_bef_aft = rbind(rec_first,rec_second)
str_bef_aft = rbind(round_one_strength,round_two_strength)
write.csv(rec_bef_aft, file = "rec_bef_aft.csv")
write.csv(str_bef_aft, file = "str_bef_aft.csv")
#simulation_2 = 
unique(question_data[question_data$tag1 %in% c('tag-1972c7f2'),]$question_id)
unique(question_data[question_data$tag1 %in% c('tag-1ab2e7bb'),]$question_id)
rec_bef_aft[rec_bef_aft$tag1 == 'tag-1972c7f2',]
rec_bef_aft[rec_bef_aft$tag1 == 'tag-1ab2e7bb',]
question_data_sub[question_data_sub$tag1 %in% c('tag-1972c7f2','tag-1ab2e7bb'),]

#verification
ratingsMatrixTrain[ratingsMatrixTrain$user == '019d90f6',]
question_data_sub_3[question_data_sub_3$masked_user_id == '019d90f6' & question_data_sub_3$question_id %in% c(CFrecommended$question_id),]
question_data_sub_3 %>% group_by(masked_user_id) %>% summarise(score = sum(points_earned)) 
question_data[question_data$masked_user_id == '019d90f6' & question_data$question_id %in% c(CFrecommended$question_id),]
choiceUniverse[choiceUniverse$question_id %in% CFrecommended$question_id, ]
strength_tags
question_data[question_data$masked_user_id == '019d90f6' & question_data$question_id == 14521,]
question_data[question_data$question_id %in% c(14521,14516),]
question_data_sub[question_data_sub$masked_user_id == '019d90f6' & question_data_sub$question_id == '14521',]
question_data_sub[question_data_sub$question_id %in% choiceUniverse$question_id,]
choiceUniverse[choiceUniverse$question_id %in% c(9491,14382,14383,14384,14388,14516),]

#Now stream recommendation based on user's strengh in question activity
#1. Get stream-strength matrix
#2. Apply viewed /un-viewed gradient
#3. Show top 50

#Build Stream-Tag feature matrix
stream_master = unique(stream_data[,c(1,14:20)])
sapply(stream_master, function(col) sum(is.na(col)))
apply(stream_master, 2, function(x) length(unique(x)))
stream_master[duplicated(stream_master) == TRUE, ]
stream_master$all_tags = paste0(stream_master$tag1," ",
                                stream_master$tag2," ",
                                stream_master$tag3," ",
                                stream_master$tag4," ",
                                stream_master$tag5," ",
                                stream_master$tag6," ",
                                stream_master$tag7)
Scorpus <- Corpus(VectorSource(stream_master$all_tags))
Sdtm <- DocumentTermMatrix(Scorpus, control = list(weighting=weightBin))
inspect (Sdtm)
Sdt <- as.data.table(as.matrix(Sdtm));Sdt[1:2,1:20]
Sdt <- cbind(deck_id = stream_master$deck_id, Sdt);Sdt[1:2,1:20]
save(Sdt, file = "Sdt.RData")

#create a new binary matrix to filter out relevant Tags based on user-strength tags - Streams
SnewMat <- data.table("USER STENGTHS" , matrix(0, nrow = 1, ncol = ncol(Sdt)-1)) ; SnewMat[1:2,1:10];Sdt[1:2,1:10]
SnewMat[, which(colnames(Sdt) %in% strength_tags)] <- 1; SnewMat[1:2,1:10];Sdt[1:2,1:10]
colnames(SnewMat) <- colnames(Sdt); SnewMat[1:2,1:10];Sdt[1:2,1:10]
SnewMat <- as.data.frame(SnewMat)[, strength_tags]; SnewMat[1:1,];Sdt[1:2,1:10]

# calculate based on cosine distance, output top 100 closest questions 
S_distances <- c(dist(SnewMat, as.data.frame(Sdt)[, strength_tags], method = "cosine"))
FS_choiceUniverse = data.table(deck_id = Sdt$deck_id, dist = S_distances) %>% arrange(dist) %>% as.data.table
S_choiceUniverse <- data.table(deck_id = Sdt$deck_id, dist = S_distances) %>% arrange(dist) %>% as.data.table %>% head(topNQs)

#Stream
strength_stream_data = stream_data[stream_data$deck_id %in% S_choiceUniverse$deck_id,]
stream_view_data[stream_view_data$masked_user_id == LoggedInUser,]
strength_stream_data[strength_stream_data$masked_user_id == LoggedInUser,]

#View Score
strength_stream_data$view_score = ifelse(strength_stream_data$action == "STREAM_COMPLETE", 1, 0)
strength_stream_data$serve_score = ifelse(strength_stream_data$action == "STREAM_RECIEVED", 1, 0)
stream_view_data = as.data.table(strength_stream_data %>% group_by(deck_id,masked_user_id) %>% summarise(vs = sum(view_score), ss = sum(serve_score)))
stream_view_data$view_status[stream_view_data$ss == 0 & stream_view_data$masked_user_id == LoggedInUser] = 1  ##Fully Viewed
stream_view_data$view_status[stream_view_data$ss > 0 & stream_view_data$vs > 0 & stream_view_data$masked_user_id == LoggedInUser] = -1  ##Partially Viewed
stream_view_data$view_status[is.na(stream_view_data$view_status)] = 0 ##Not viewed at all
apply(stream_view_data, 2, function(x) length(unique(x)))
sapply(stream_view_data, function(col) sum(is.na(col))) 
table(stream_view_data$view_status, useNA = "ifany")
stream_view_dist = merge(stream_view_data, FS_choiceUniverse, by.x = "deck_id", by.y = "deck_id", all.x = T ) %>% 
  select(deck_id = deck_id, view_status =view_status, distance = dist) %>% 
  arrange(-distance) %>% 
  distinct()
stream_view_dist$dist_rec = stream_view_dist$view_status + stream_view_dist$distance
stream_recommend = stream_view_dist %>% select(deck_id = deck_id, rec_sclae = dist_rec, view_status = view_status) %>% arrange(rec_sclae) %>% head(topNQs)
# streams_tags = merge(stream_recommend, stream_master, by.x = "deck_id", by.y = "deck_id", all.x = T ) %>% 
#   select(deck_id = deck_id, rec_sclae = rec_sclae, view_status=view_status, tag1 = tag1, tag2=tag2,tag3=tag3,tag4=tag4,tag5=tag5,tag6=tag6,tag7=tag7 ) %>% arrange(rec_sclae)
# head(streams_tags,10)

#For Shiny UI
stream_recommend$user_id = LoggedInUser
temp3 = stream_recommend %>% select(user_id = user_id, deck_id = deck_id, score = rec_sclae) %>% head(50)
stregth_sample_recommendataions_streams = rbind(stregth_sample_recommendataions_streams, temp3)
saveRDS(stregth_sample_recommendataions_streams, file = "Demo/strength_recommendataions_streams")
