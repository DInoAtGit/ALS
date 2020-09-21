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
                                question_data$tag2," ",
                                question_data$tag3," ",
                                question_data$tag4," ",
                                question_data$tag5," ",
                                question_data$tag6," ",
                                question_data$tag7)

#How roles are:
table(question_data$role_id)

#check unique values & NA in each
apply(question_data, 2, function(x) length(unique(x)))
sapply(question_data, function(col) sum(is.na(col))) 
apply(stream_data, 2, function(x) length(unique(x)))
sapply(stream_data, function(col) sum(is.na(col)))

#Unique Tags
apply(question_data[,c(9:15)], 2, function(x) length(unique(x)))
apply(stream_data[,c(14:20)], 2, function(x) length(unique(x)))

#Missing Tags
apply(question_data[,c(9:15)], 2, function(x) sum(is.na(x)))
apply(stream_data[,c(14:20)], 2, function(x) sum(is.na(x)))


# How questions are distrubited across various descriptive tags
question_tags = unique(question_data[,c(1,9:15)])
barplot(sapply(question_tags, function(col) sum(is.na(col)))) #missing tags
question_tags$all_desc_tags = paste0(question_tags$tag5," ",question_tags$tag6)
corpusQTags <- Corpus(VectorSource(question_tags$all_desc_tags))
corpusQTagsdtm <- DocumentTermMatrix(corpusQTags, control = list(weighting = weightTfIdf))
QTagsMat <- as.matrix(corpusQTagsdtm);QTagsMat[1:5,1:5]
tagnames <- colnames(QTagsMat)
sort(colSums(QTagsMat), decreasing = T) %>% head
wordcloud(tagnames, colSums(QTagsMat), colors=dark2 <- brewer.pal(6, "Dark2"), random.order = F, max.words = 100)
rm(question_tags,corpusQTagsdtm,tagnames,corpusQTags,QTagsMat)

#Build Question-Tag feature matrix
question_master = unique(question_data[,c(1,9:15)])
sapply(question_master, function(col) sum(is.na(col)))
apply(question_master, 2, function(x) length(unique(x)))
question_master[duplicated(question_master) == TRUE, ]
question_master$all_tags = paste0(question_master$tag1," ",
                                  question_master$tag2," ",
                                  question_master$tag3," ",
                                  question_master$tag4," ",
                                  question_master$tag5," ",
                                  question_master$tag6," ",
                                  question_master$tag7)
corpus <- Corpus(VectorSource(question_master$all_tags))
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightBin))
inspect (dtm)
# dtm_ti <- weightTfIdf(dtm)
# inspect(dtm_ti)
dt <- as.data.table(as.matrix(dtm));dt[1:2,1:20]
dt <- cbind(question_id = question_master$question_id, dt);dt[1:2,1:20]
save(dt, file = "dt.RData")


#Verify if cosine similarity returns right set of questions based on feature matrix
sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm))/(sqrt(col_sums(t(dtm)^2) %*% t(col_sums(t(dtm)^2))))
as.matrix(sim_mat_cos)[1:5, 1:10]

question_master[1,];question_master[4,] #closest with 6 tags matching (2 actual, 4 NAs - are data quality issues)
question_master[3,];question_master[5,] #farthest with 4 tags matching (1 actual, 3 NAs)

recQst = function (simm, quest, k) {
  found <- sort(simm[, quest], decreasing = T)[2:(k+1)]
  #print(found)
  cat(paste0("Selected Question: <ID>", question_master[quest, 1], "\n<Tags>", question_master[quest, 9]),"\n")
  cat("\nRecommended Questions:")
  resindex <- as.integer(names(found))
  #print(resindex)
  for (i in 1:k) {
    cat(paste0("\n",i,"-", resindex[i], " <question_id>", question_master[resindex[i], 1], "\n<Tags>", question_master[resindex[i], 9]))
    #cat(paste0("\n",i,".", " <question_id>", question_master1[resindex[i], 1]))
    #print(question_master1[resindex[i], 1])
  }
}

recQst(sim_mat_cos, 283, 2)
question_master[question_master$question_id %in% c(14516,16454),]
question_master[question_master$tag1 == 'tag-13b3f31a' & question_master$tag2 == 'tag-ce9cc2e6',]


# new user will input a role_id and some tags   (For existing user, get latest role_id from user master & pref_tag from best scores - Strength)
LoggedInUser = 'Dinakar' #'019d90f6'
role <- 1
strength_tags <- c("electronics","computers", "audio", "video") %>% tolower  #CB Matrix
topNQs = 100 #Number of related question data
serendipityNum = 20 #Numer of non-related question data

#Check if user exists - Yes: get his activity data, Else, get his role associated assessment data
if (!(LoggedInUser %in% question_data[question_data$masked_user_id == LoggedInUser,]$masked_user_id)) {
  question_data_sub = question_data[question_data$role_id == role,]
}else {
  question_data_sub = question_data[question_data$masked_user_id == LoggedInUser,]
  temp_tags = question_data_sub %>% group_by(all_tags) %>% 
    summarise(score = sum(points_earned)) %>% filter(score >= 10) %$% all_tags
  temp_tags_1 =  data.frame(tags = unlist(strsplit(temp_tags, " ")))
  strength_tags = unique(temp_tags_1[temp_tags_1$tags != 'NA',])
}

  
#create a new binary matrix to filter out relevant Tags based on user-strength tags
newMat <- data.table("USER STENGTHS" , matrix(0, nrow = 1, ncol = ncol(dt)-1)) ; newMat[1:2,1:10];dt[1:2,1:10]
newMat[, which(colnames(dt) %in% strength_tags)] <- 1; newMat[1:2,1:10];dt[1:2,1:10]
colnames(newMat) <- colnames(dt); newMat[1:2,1:10];dt[1:2,1:10]
newMat <- as.data.frame(newMat)[, strength_tags]; newMat[1:1,];dt[1:2,1:10]

# calculate based on cosine distance, output top 100 closest questions 
distances <- c(dist(newMat, as.data.frame(dt)[, strength_tags], method = "cosine"))
choiceUniverse <- data.table(question_id = dt$question_id, dist = distances) %>% arrange(dist) %>% as.data.table %>% head(topNQs)
#question_master[question_master$question_id %in% c(2849,2851,14516),];choiceUniverse[choiceUniverse$question_id %in% c(2849,2851,14516),]

#Role + Strength Tags fileterd question data
if (!(LoggedInUser %in% question_data[question_data$masked_user_id == LoggedInUser,]$masked_user_id)) {
  question_data_sub_2 <- question_data_sub[question_data_sub$question_id %in% c(choiceUniverse$question_id),]
}else {
  question_data_sub_2 <- question_data[question_data$question_id %in% c(choiceUniverse$question_id),] #Already answered + to be answered
}

#question_data_sub_2[question_data_sub_2$masked_user_id == LoggedInUser,]

# Pick-up few questions that MIGHT NOT be from the same role that user belongs or from the same tags (Serendipity)
question_list = question_data %>% 
  dplyr::filter(question_id %in% choiceUniverse$question_id, 
                #!question_id %in% question_data_sub_2$question_id), #May not belong to same role
                !question_id %in% question_data_sub$question_id) %$% question_id %>% unique #May not belong to same strengths

# choiceUniverse[choiceUniverse$question_id == 15331,]
# question_data_sub_2[question_data_sub_2$question_id == 15331,]
# question_data_sub[question_data_sub$question_id == 15331,]
# question_data[question_data$question_id == 15331,]

if (length(question_list) > serendipityNum)
{
  serendipity_question_set <- question_data %>% dplyr::filter(question_id %in% sample(question_list, serendipityNum))
}else
{
  serendipity_question_set <- question_data %>% dplyr::filter(question_id %in% question_list)
}
unique(serendipity_question_set$question_id); length(unique(serendipity_question_set$question_id))
#Verify
choiceUniverse[choiceUniverse$question_id %in% serendipity_question_set$question_id,]

# Final question data set where tag strength is high in a given peer group [role_id] 

if (length(unique(question_data_sub_2$question_id)) >= 30) {
  question_data_sub_3 <- rbind(question_data_sub_2, serendipity_question_set)
  closestUser <- question_data_sub_2 %>% group_by(masked_user_id) %>% summarise(score = sum(points_earned)) %>% arrange(-score) %>% head(3) %$% masked_user_id
  closestUserCF <- closestUser[1] 
} else {
  question_data_sub_3 <- rbind(question_data_sub, serendipity_question_set)
  closestUser <- question_data_sub %>% group_by(masked_user_id) %>% summarise(score = sum(points_earned)) %>% arrange(-score) %>% head(3) %$% masked_user_id
  closestUserCF <- closestUser[1] 
}

#question_data_sub_3 = question_data_sub_2

# CF using reco
# Model evaluation
ratingsMatrixTrain <- question_data %>% select(user = masked_user_id, item = question_id, ratings = points_earned, 9:15)
ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)
trainInd <- sample(1:nrow(ratingsMatrixTrain), size = 0.9*nrow(ratingsMatrixTrain))
testInd <- (1:nrow(ratingsMatrixTrain))[!(1:nrow(ratingsMatrixTrain)) %in% trainInd]
trainset <- ratingsMatrixTrain[trainInd, ]
testset <- ratingsMatrixTrain[testInd, ]
train_data <- data_memory(user_index = trainset$user, item_index = trainset$item, rating = trainset$ratings, index1= TRUE)
test_data <- data_memory(user_index = testset$user, item_index = testset$item, rating = testset$ratings, index1= TRUE)
reco <- Reco()
reco$train(train_data, opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 50))
test_set = data_memory(testset$user, testset$item)
testset$pred <- reco$predict(test_set, out_memory())
RMSE.CF <- sqrt(mean(testset$pred - testset$ratings)^2);RMSE.CF
MAE.CF <- mean(abs(testset$pred - testset$ratings));MAE.CF


# CF recommendation model using recosystem library using ALS algorithm
ratingsMatrixTrain <- question_data_sub_3 %>% select(user = masked_user_id, item = question_id, ratings = points_earned, 9:15)
ratingsMatrixTrain$user <- as.factor(ratingsMatrixTrain$user)
ratingsMatrixTrain$item <- as.factor(ratingsMatrixTrain$item)
uniqUsers <- length(unique(ratingsMatrixTrain$user));uniqUsers
uniqItems <- length(unique(ratingsMatrixTrain$item));uniqItems
reco <- Reco()
reco$train(data_memory(user_index = ratingsMatrixTrain$user, item_index = ratingsMatrixTrain$item, rating = ratingsMatrixTrain$ratings),
           opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 50))

#predict for a highly active user
closestUser_Index <- rep(which(levels(ratingsMatrixTrain$user) == closestUserCF), each = uniqItems)
pred <- reco$predict(data_memory(closestUser_Index, 1:uniqItems), out_memory())
tempfinalReco <- data.table(question_id = unique(ratingsMatrixTrain$item), scores = pred)
question_data_sub_2$question_id = as.factor(question_data_sub_2$question_id)
CFrecommended = merge(tempfinalReco, question_data_sub_2, by.x = "question_id", by.y = "question_id", all.x = T) %>% select(question_id = question_id, PredScore = scores, 8,10:16) %>% arrange(-PredScore) %>% distinct()
head(CFrecommended,10)


#Predict for a given user 
closestUser_Index <- rep(which(levels(ratingsMatrixTrain$user) == LoggedInUser), each = uniqItems)
pred <- reco$predict(data_memory(closestUser_Index, 1:uniqItems), out_memory())
tempfinalReco <- data.table(question_id = unique(ratingsMatrixTrain$item), scores = pred)
question_data_sub_3$question_id = as.factor(question_data_sub_3$question_id)
CFrecommended = merge(tempfinalReco, question_data_sub_3, by.x = "question_id", by.y = "question_id", all.x = T) %>% select(question_id = question_id, PredScore = scores, 10:16) %>% arrange(-PredScore) %>% distinct()
head(CFrecommended,10)

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

#Verify if cosine similarity returns right set of questions based on feature matrix
S_sim_mat_cos <- crossprod_simple_triplet_matrix(t(Sdtm))/(sqrt(col_sums(t(Sdtm)^2) %*% t(col_sums(t(Sdtm)^2))))
as.matrix(S_sim_mat_cos)[1:5, 1:10]


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

#View Score
strength_stream_data$view_score = ifelse(strength_stream_data$action == "STREAM_COMPLETE", 1, 0)
strength_stream_data$serve_score = ifelse(strength_stream_data$action == "STREAM_RECIEVED", 1, 0)
stream_view_data = as.data.table(strength_stream_data %>% group_by(deck_id,masked_user_id) %>% summarise(vs = sum(view_score), ss = sum(serve_score)))
stream_view_data$view_status[stream_view_data$ss == 0] = 1
stream_view_data$view_status[stream_view_data$ss > 0 & stream_view_data$vs > 0] = 0
stream_view_data$view_status[stream_view_data$ss > 0 & stream_view_data$vs == 0] = -1
apply(stream_view_data, 2, function(x) length(unique(x)))
sapply(stream_view_data, function(col) sum(is.na(col))) 
table(stream_view_data$view_status)
stream_view_dist = merge(stream_view_data, FS_choiceUniverse, by.x = "deck_id", by.y = "deck_id", all.x = T ) %>% 
  select(deck_id = deck_id, view_status =view_status, distance = dist) %>% 
  arrange(-distance) %>% 
  distinct()
stream_view_dist$dist_rec = stream_view_dist$view_status + stream_view_dist$distance
stream_recommend = stream_view_dist %>% select(deck_id = deck_id, rec_sclae = dist_rec) %>% arrange(rec_sclae) %>% head(topNQs)



