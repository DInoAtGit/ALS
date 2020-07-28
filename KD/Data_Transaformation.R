
#Load packages
pacman::p_load(tm,slam,topicmodels,SnowballC,wordcloud,RColorBrewer,tidyverse, caret, corrplot, broom, 
               ggpubr, MASS,relaimpo, car, e1071,interplot,caTools,lubridate,date,stringi,ROCR,IRdisplay,
               knitr,data.table,dplyr,RColorBrewer,recosystem,softImpute,reshape2,BiocManager,
               recommenderlab,stringr,data.table,recommenderlab,stringr)

#Update R
# pacman::p_load(installr)
# updateR()

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
#pacman::p_load(R.utils)
#gunzip("assess_taged_new.gz", remove=FALSE)

#ua_data = fread("assess_taged_new", sep = ",")
stream_data = fread("views_taged_new.csv", sep = ",")


head(stream_data, 4)
dim(stream_data)
tail(stream_data, 4)
str(stream_data)


#Remove unwanted data
#stream_data = us_data %>% select(user_id = masked_user_id, login_type = login_handle_type,  role_id, lang_code, country, city, user_since = user_since_d, stream_id = deck_id, tag1 = stream_tags, tag2 = stream_tags2, user_activity_date = user_action_d, user_action_timestamp, activity_type, client_type, app_version_id)
stream_data1 = subset(stream_data, select = -c(V1,card_id,action,app_version_id,client_type,city,login_handle_type))


#Factorize
cols = c('action', 'country','lang_code','role_id','client_type')
stream_data[,cols] = lapply(stream_data[,cols], factor)

#Convert to Date
stream_data1$user_action_d = substr(stream_data1$user_action_timestamp, start = 1, stop = 10)
stream_data1$user_action_d = as.Date(stream_data1$user_action_d, "%Y-%m-%d")

stream_data1$user_since_d = substr(stream_data1$user_since, start = 1, stop = 10)
stream_data1$user_since_d = as.Date(stream_data1$user_since_d, "%Y-%m-%d")

#drop datatimecolumns
stream_data1 = subset(stream_data1, select = -c(user_action_timestamp,user_since))

#Tidy-up names to match with presentation
stream_data1 = rename(stream_data1, stream_id = deck_id, user_id = masked_user_id, tag1 = stream_tags, tag2 = stream_tags2, 
                      activity_date = user_action_d, user_since_date = user_since_d)

#Are there any duplicated data
stream_data1[duplicated(stream_data1) == TRUE, ] # 1 record
stream_data1 = stream_data1[duplicated(stream_data1) == FALSE, ]  # Remove dups

#Are there any missing values..
sapply(stream_data1, function(col) sum(is.na(col))) 

#Is role_id missing a concern?
role.na = stream_data1[is.na(role_id),]
role.non.na = stream_data1[!is.na(role_id),]
length(unique(stream_data1$user_id)); length(unique(role.na$user_id)); length(unique(role.non.na$user_id))  
length(unique(role.na$country)); length(unique(role.non.na$country))
setDT(role.na)
setDT(role.non.na)
usr_with_miss_role = role.non.na[, blankrole := FALSE][role.na, blankrole := TRUE, on = .(user_id)]
head(usr_with_miss_role,4)
head(usr_with_miss_role[usr_with_miss_role$blankrole == TRUE,],4)
rm(role.na, role.non.na, usr_with_miss_role)
#conclusion: There are 1.3 milllion valid users across the countries missing roles. They are not dups.


#Roles and rows
barplot(table(stream_data1$role_id, useNA = "ifany"))

#Lang and rows
barplot(table(stream_data1$lang_code, useNA = "ifany"))

#Countr and rows
barplot(table(stream_data1$country, useNA = "ifany"))

#Unique Users, Streams, Tags and rows
length(unique(stream_data1$stream_id)) #2754
length(unique(stream_data1$user_id))  #75848
length(unique(stream_data1$tag1)); length(unique(stream_data1$tag2)); length(unique(unlist(subset(stream_data1, select = c(tag1,tag2))))) # 1134
table(unique(substr(stream_data1$tag1, start = 1, stop = 3)));table(unique(substr(stream_data1$tag2, start = 1, stop = 3))) #all tags

#Users per tag 
boxplot(table(stream_data1$tag1)) # uniformly distributed

#Are datatypes correct?
str(stream_data1)
cols = c('country','lang_code','role_id','tag1', 'tag2')
stream_data1[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
head(stream_data1,4)


