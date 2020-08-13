
#Load packages
# pacman::p_load(tm,slam,topicmodels,SnowballC,wordcloud,RColorBrewer,tidyverse, caret, corrplot, broom, 
#                ggpubr, MASS,relaimpo, car, e1071,interplot,caTools,lubridate,date,stringi,ROCR,IRdisplay,
#                knitr,data.table,dplyr,RColorBrewer,recosystem,softImpute,reshape2,BiocManager,
#                recommenderlab,stringr,data.table,recommenderlab,stringr)



#Load packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("views_model_with_tags.RData")
dim(stream_data4)
stream_data = stream_data4; rm(stream_data4)
head(stream_data)

#Remove unwanted data
stream_data1 = subset(stream_data, select = -c(card_id,action,app_version_id,client_type,city,login_handle_type))


#Convert to Date
stream_data1$user_action_d = substr(stream_data1$user_action_timestamp, start = 1, stop = 10)
stream_data1$user_action_d = as.Date(stream_data1$user_action_d, "%Y-%m-%d")

stream_data1$user_since_d = substr(stream_data1$user_since, start = 1, stop = 10)
stream_data1$user_since_d = as.Date(stream_data1$user_since_d, "%Y-%m-%d")

#drop datatimecolumns
stream_data1 = subset(stream_data1, select = -c(user_action_timestamp,user_since))

#Tidy-up names to match with presentation
stream_data1 = rename(stream_data1, stream_id = deck_id, activity_date = user_action_d, user_since_date = user_since_d)

#Are there any duplicated data
stream_data1 = stream_data1[duplicated(stream_data1) == FALSE, ]  # Remove dups

#Are datatypes correct?
cols = c('country','lang_code','role_id','tag1', 'tag2','tag3','tag4','tag5','tag6','tag7')
stream_data1[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
head(stream_data1, 4)

#Source Files
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day1\\CF-demolib-v3.R")

