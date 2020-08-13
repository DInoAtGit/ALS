

#Load packages
pacman::p_load(tm,slam,topicmodels,SnowballC,wordcloud,RColorBrewer,tidyverse, caret, corrplot, broom, ggpubr, 
               MASS,relaimpo, car, e1071,interplot,caTools,lubridate,date,stringi,ROCR,IRdisplay,knitr,
               data.table,dplyr,RColorBrewer,recosystem,softImpute,reshape2,recommenderlab,stringr,BiocManager)

#Update R
# pacman::p_load(installr)
# updateR()

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
#pacman::p_load(R.utils)
#gunzip("views_model.gz", remove=FALSE)
#activity_data = read.csv("views_model")
load("views_model_with_tags.RData")
dim(stream_data4)
activity_data = stream_data4; rm(stream_data4)

#Explore
head(activity_data, 4)
dim(activity_data)
str(activity_data)


#Factorize
cols = c('action', 'country','lang_code','role_id','client_type','tag1','tag2','tag3','tag4','tag5','tag6','tag7')
activity_data[,cols] = lapply(activity_data[,cols], factor)

#Convert to Date
activity_data$user_since_d = substr(activity_data$user_action_timestamp, start = 1, stop = 10)
activity_data$user_since_d = as.Date(activity_data$user_since_d, "%Y-%m-%d")


#Unique Roles
unique(activity_data$role_id)

#Roles and rows
barplot(table(activity_data$role_id, useNA = "ifany"))

#Lang and rows
barplot(table(activity_data$lang_code, useNA = "ifany"))

#Countr and rows
barplot(table(activity_data$country, useNA = "ifany"))

#Extract IR data & refactor
activity_data_t = activity_data[activity_data$country == 'IR',]
str(activity_data_t);dim(activity_data_t)
activity_data_t[,cols] = lapply(activity_data_t[,cols], factor)
barplot(table(activity_data_t$lang_code, useNA = "ifany"))
barplot(table(activity_data_t$role_id, useNA = "ifany"))
dim(activity_data_t); length(unique(activity_data_t$user_id)); length(unique(activity_data_t$deck_id))
head(activity_data_t,4)

#Stream View Count per user per deck

deck_view = activity_data_t %>%  
  group_by(user_id, deck_id) %>%
  summarise(vc=n())

deck_view = as.data.frame(deck_view)
head(deck_view,4)
table(deck_view$vc);nrow(deck_view[deck_view$vc == 1,])
barplot(table(deck_view$vc, useNA = "ifany"))
deck_view[deck_view$user_id == "0008a603",]; activity_data_t[activity_data_t$user_id == "0008a603",]
deck_view[deck_view$deck_id == "08b4f6d3",]
length(unique(deck_view$user_id)); length(unique(deck_view$deck_id))
deck_view[deck_view$vc == 8,]
deck_view$user_id = as.factor(deck_view$user_id)
deck_view$deck_id = as.factor(deck_view$deck_id)
str(deck_view);dim(deck_view)

#Avg views per user 
deck_view %>% group_by(user_id) %>% summarise(avg_vc = mean(vc)) %>% 
  ggplot(aes(avg_vc)) + geom_histogram()

#Avg views per deck 
deck_view %>% group_by(deck_id) %>% summarise(avg_vc = mean(vc)) %>% 
  ggplot(aes(avg_vc)) + geom_histogram()


#CF Matrix - Using Barry's lib

  source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day1\\CF-demolib-v3.R")
  
  #Check matrix size
  as.numeric(length(unique(deck_view$user_id)))*as.numeric(length(unique(deck_view$deck_id)))/1000000 # to show the size of the ratings matrix if explicitly created (in millions)
  memory.limit() # to see the current memory limit you have in MBytes
  
  users = acast(deck_view, user_id ~ deck_id, value.var = "vc")
  dim(users)
  users[1:10,1:15]
  #users[is.na(users)] = 0 #Replace NA with 0
  users[users[,"08b4f6d3"] == 2]
  
  #build similarity matrix on users - euclidean similarity for item-item 
  itemsimsE = getitemsimsmatrix(users, simfun=euclidsim); 
  itemsimsE[1:10,1:10]
  
  # get recommendations 
  LoginUser = "00f76716"
  
  
  #Cold Start
  if (length(activity_data_t[activity_data_t$user_id == LoginUser,]$user_id) >  0)
  {
    targetuser = LoginUser # Regular
  }
  else {
    
    if (length(activity_data[activity_data$user_id == LoginUser,]$user_id) > 0){
      #Get the role_id
    }
    else {
      
    }
    
  }
    
    
    
    if (length(activity_data[activity_data$user_id == LoginUser,]$user_id) >  0){
      #Get best user based on role_id from user master (user master is not avialble yet)
    }
    else {
      
      
    }
  }

  
  
  
  target = users[LoginUser,]
  
  #You may be interested in (Similar Items) - Covers longtail as it doesn't scope to role.
  getrecommendations_II(target, itemsimsE, topN=10)
  
  
  #People also viewed (Similar Users) 
  target_latest_active_d = activity_data_t %>% filter(user_id == LoginUser) %>% summarise(latest=max(user_since_d))
  target_latest_role = max(as.integer(activity_data_t[activity_data_t$user_id == LoginUser & activity_data_t$user_since_d == as.Date(target_latest_active_d$latest), ]$role_id))
  target_lang = as.String(unique(activity_data_t[activity_data_t$user_id == LoginUser, "lang_code"])) 
  r_users = users[rownames(users) %in% unique(activity_data_t[activity_data_t$role_id == target_latest_role, "user_id"]),]; dim(r_users)
  r_l_users = r_users[rownames(r_users) %in% unique(activity_data_t[activity_data_t$role_id == target_latest_role, "user_id"]),]; dim(r_users)
  
  getrecommendations_UU(target, r_users, simfun=euclidsim, topN =10) ## - - Limited to role..
  getrecommendations_UU(target, users, simfun=euclidsim, topN =10) ## - - Coveres whole activity
  
  activity_data_t[activity_data_t$deck_id == "e2b43fa5",]
  

  

  
  
  
  
  
  
#Testing the approach - Euclidean is Best for both UU and II
  fillrate(users)
  unlist_users = unlist(users)
  hist(unlist_users) #Histo of Views. Most viewed 1.
  likethreshold_m =1 #Viewed atleast once
  likerate_m = length(which(unlist_users>=likethreshold_m))/length(unlist_users) ; cat("% of decks that are viewed=",likerate_m*100,"%")
  
  #Get correlation matrix between users
  cor(t(users))
  #Correlation without NAs
  cor(t(users), use = "pairwise.complete.obs")
  
  # get recommendations for U2 (results with pearson shd be: 3.35 (night), 2.83 (lady), 2.53 (luck))
  users[1:2,1:10]
  target_u = users["00f76716",]
  getrecommendations_UU(target_u, users, simfun=pearsonsim)
  
  #Try various similarity functions
  itemsims_u = cor(users, use = "pairwise.complete.obs"); itemsims_u[1:10, 1:10]  #Similarity Correlation Matrix (Pairwaise is actually calculate mean only for the matching rows)
  itemsimsP_u = getitemsimsmatrix(users, simfun = pearsonsim); itemsimsP_u[1:10, 1:10]  #Pearson similarity matrix
  itemsimsC_u = getitemsimsmatrix(users, simfun = cosinesim); itemsimsC_u[1:10, 1:10]   #Cosin similarity matrix
  itemsimsE_u = getitemsimsmatrix(users, simfun = euclidsim); itemsimsE_u[1:10, 1:10]   #Euclid similarity matrix
  normalizedusers = sweep(users, 1, rowMeans(users, na.rm = TRUE))   #1 means row and 2 means column
  itemsimsC_uN = getitemsimsmatrix(normalizedusers, simfun = cosinesim); itemsimsC_uN[1:10, 1:10]  #Normalized Cosin
  itemsimsE_uN = getitemsimsmatrix(users, simfun = euclidsimF); itemsimsE_uN[1:10, 1:10]  #Euclid without square-root distance
  
  #Getrecommendations Item-Item using various similarity functions
  getrecommendations_II(target_u, itemsims_u)   # using vanilla similarity, based on correlation
  getrecommendations_II(target_u, itemsimsP_u)  # using Pearson cofficient similarity, based on correlation
  getrecommendations_II(target_u, itemsimsC_u)  # using Cosine similarity, based on correlation
  getrecommendations_II(target_u, itemsimsC_uN) # using Cosine Normalized similarity, based on correlation
  getrecommendations_II(target_u, itemsimsE_u)  # using Euclid similarity, based on correlation
  getrecommendations_II(target_u, itemsimsE_uN) # using Euclid Normalized similarity, based on correlation
  
  #System evalution
  
  numtestusers = 10
  test_users_names = sample(rownames(users), min(numtestusers, nrow(users))); test_users_names
  train_users_names = setdiff(rownames(users), test_users_names); head(train_users_names,10)
  train_users = users[train_users_names,]
  test_users = users[test_users_names,]
  nrow(users);nrow(train_users);nrow(test_users)
  
  #Prediction using UU
  preddeck = predictCF(test_users, train_users, numtestitems = 10, random = FALSE, simfun = pearsonsim);preddeck #Pearson
  preddeck_c = predictCF(test_users, train_users, numtestitems = 10, random = FALSE, simfun = cosinesim);preddeck_c #Cosine
  preddeck_e = predictCF(test_users, train_users, numtestitems = 10, random = FALSE, simfun = euclidsim);preddeck_e #Eucldin
  
  
  #Evaluation / Confusion Matrix
  cat("Avg UU-based MAEs { For Pearson: ", avgMAE(preddeck), " } , {For Cosine : ", avgMAE(preddeck_c), " } , {For Eucldin : ", avgMAE(preddeck_e), " }")
  showCM(preddeck, like = 2)
  
  #Prediction using II
  itemsimsE_m_p = getitemsimsmatrix(train_users, simfun = euclidsim)
  itemsimsC_m_p = getitemsimsmatrix(train_users, simfun = cosinesim)
  itemsimsP_m_p = getitemsimsmatrix(train_users, simfun = pearsonsim)
  preddeck_II_E = predictCF(test_users, itemsims = itemsimsE_m_p, numtestitems = 10, random = FALSE); preddeck_II_E
  preddeck_II_C = predictCF(test_users, itemsims = itemsimsC_m_p, numtestitems = 10, random = FALSE); preddeck_II_C
  preddeck_II_P = predictCF(test_users, itemsims = itemsimsP_m_p, numtestitems = 10, random = FALSE); preddeck_II_P
  
  #Evaluation / Confusion Matrix
  cat("Avg II-based MAEs { For Pearson: ", avgMAE(preddeck_II_P), " } , {For Cosine : ", avgMAE(preddeck_II_C), " } , {For Eucldin : ", avgMAE(preddeck_II_E), " }")
  
  RMSE.II <- sqrt(mean(preddeck_II_E$predictedrating - preddeck_II_E$truerating)^2)
  RMSE.UU <- sqrt(mean(preddeck_II_E$predictedrating - preddeck_II_E$truerating)^2)
  
str(test_users)
