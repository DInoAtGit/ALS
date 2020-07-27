
#Load packages
pacman::p_load(tm,slam,topicmodels,SnowballC,wordcloud,RColorBrewer,tidyverse, caret, corrplot, broom, 
               ggpubr, MASS,relaimpo, car, e1071,interplot,caTools,lubridate,date,stringi,ROCR,IRdisplay,
               knitr,data.table,dplyr,RColorBrewer,recosystem,softImpute,reshape2,BiocManager,MCRestimate,
               recommenderlab,stringr,data.table)

#Update R
# pacman::p_load(installr)
# updateR()

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
#pacman::p_load(R.utils)
#gunzip("assess_taged_new.gz", remove=FALSE)
ua_data = fread("assess_taged_new", sep = ",")
us_data = fread("views_taged_new.csv", sep = ",")

head(ua_data, 4)
dim(ua_data)
tail(ua_data, 4)
str(ua_data)

#Convert to Date
ua_data$user_submit_d = substr(ua_data$submission_utc_ts, start = 1, stop = 10)
ua_data$user_submit_d = as.Date(ua_data$user_submit_d, "%Y-%m-%d")

#Get rid of unwanted colunmns
ua_data = subset(ua_data, select = -c(V1))

#Are there any missing values..
sapply(ua_data, function(col) sum(is.na(col))) 
nrow(ua_data[is.na(ua_data$country),]);nrow(ua_data[is.na(ua_data$role_id),]) # 402 & 1.3 mill
ua_data = ua_data[!is.na(ua_data$country),] #eleminate 402 country NA records. No action on role_id

#Are there any duplicated data
ua_data[duplicated(ua_data) == TRUE, ] # 63 records
ua_data = ua_data[duplicated(ua_data) == FALSE, ]  # Remove dups

#Further more dups removal after unwanted columns elemination
ua_data = subset(ua_data, select = -c(submission_utc_ts))
ua_data[duplicated(ua_data) == TRUE, ] # lot of records
ua_data = ua_data[duplicated(ua_data) == FALSE, ]  # Remove dups




#Roles and rows
barplot(table(ua_data$role_id, useNA = "ifany"))

#Lang and rows
barplot(table(ua_data$org_id, useNA = "ifany"))

#Countr and rows
barplot(table(ua_data$country, useNA = "ifany"))

#Unique Users, Streams, Tags and rows
length(unique(ua_data$question_id)) #11031
length(unique(ua_data$masked_user_id))  #34429
length(unique(ua_data$question_tags)); length(unique(ua_data$V2)); length(unique(unlist(subset(ua_data, select = c(question_tags,V2))))) # 1135
table(unique(substr(ua_data$question_tags, start = 1, stop = 3)));table(unique(substr(ua_data$V2, start = 1, stop = 3))) #all tags






dim(us_data)
head(us_data, 4)
str(us_data)

#Factorize
cols = c('action', 'country','lang_code','role_id','client_type')
us_data[,cols] = lapply(us_data[,cols], factor)

#Convert to Date
us_data$user_action_d = substr(us_data$user_action_timestamp, start = 1, stop = 10)
us_data$user_action_d = as.Date(us_data$user_action_d, "%Y-%m-%d")

us_data$user_since_d = substr(us_data$user_since, start = 1, stop = 10)
us_data$user_since_d = as.Date(us_data$user_since_d, "%Y-%m-%d")


#Remove unwanted data
#stream_data = us_data %>% select(user_id = masked_user_id, login_type = login_handle_type,  role_id, lang_code, country, city, user_since = user_since_d, stream_id = deck_id, tag1 = stream_tags, tag2 = stream_tags2, user_activity_date = user_action_d, user_action_timestamp, activity_type, client_type, app_version_id)
us_data = subset(us_data, select = -c(X))

#Roles and rows
barplot(table(us_data$role_id, useNA = "ifany"))

#Lang and rows
barplot(table(us_data$lang_code, useNA = "ifany"))

#Countr and rows
barplot(table(us_data$country, useNA = "ifany"))

#Unique Users, Streams, Tags and rows
length(unique(us_data$deck_id)) #2754
length(unique(us_data$masked_user_id))  #75848
length(unique(us_data$stream_tags)); length(unique(us_data$stream_tags2)); length(unique(unlist(subset(us_data, select = c(stream_tags,stream_tags2))))) # 1134
table(unique(substr(us_data$stream_tags, start = 1, stop = 3)));table(unique(substr(us_data$stream_tags2, start = 1, stop = 3))) #all tags



