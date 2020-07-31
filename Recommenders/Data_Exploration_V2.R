
#Load packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)
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
pacman::p_load(R.utils)
gunzip("Original\\user_assessments.gz", remove=FALSE)
gunzip("Original\\views_model.gz", remove=FALSE)
gunzip("Original\\user_master.gz", remove=FALSE)

stream_data = fread("Original\\views_model", sep = ",")
tag_data = read.csv("Categories.csv", sep = ",")
tag_data = subset(tag_data, select = -c(X))
head(tag_data,2)

#############Categroy data - No need to load. Data is ready in csv.
category_data = fread("2019-Oct.csv", select = c("category_code"))
nrow(category_data[duplicated(category_data) == TRUE, ]) # 1 record
category_data1 = category_data[duplicated(category_data) == FALSE, ]  # Remove dups
category_data1$category_code = gsub("\\.", ",",category_data1$category_code)
cat_list <-strsplit(category_data1$category_code, ",")
category_data2 = data.frame(tag1= sapply(cat_list, "[", 1), tag2 = sapply(cat_list, "[", 2), tag3 = sapply(cat_list, "[", 3))
rm(category_data, category_data1, cat_list)
category_data2 = as.data.table(category_data2)
category_data2 = category_data2[!is.na(category_data2$tag1),]
category_data2 = as.data.frame(category_data2)
write.csv(category_data2, file = "Categories.csv")
#############Categroy data - No need to load. Data is ready in csv.


head(stream_data, 4)
dim(stream_data)
tail(stream_data, 4)
str(stream_data)


#Remove unwanted data
#stream_data = us_data %>% select(user_id = masked_user_id, login_type = login_handle_type,  role_id, lang_code, country, city, user_since = user_since_d, stream_id = deck_id, tag1 = stream_tags, tag2 = stream_tags2, user_activity_date = user_action_d, user_action_timestamp, activity_type, client_type, app_version_id)
stream_data1 = subset(stream_data, select = -c(app_version_id,client_type,city,login_handle_type))

#Convert to Date
stream_data1$user_action_d = substr(stream_data1$user_action_timestamp, start = 1, stop = 10)
stream_data1$user_action_d = as.Date(stream_data1$user_action_d, "%Y-%m-%d")
stream_data1$user_since_d = substr(stream_data1$user_since, start = 1, stop = 10)
stream_data1$user_since_d = as.Date(stream_data1$user_since_d, "%Y-%m-%d")

#drop datatimecolumns
head(stream_data1,4)
stream_data1 = subset(stream_data1, select = -c(user_since))

#Tidy-up names to match with presentation
stream_data1 = rename(stream_data1, stream_id = deck_id, activity_date = user_action_d, user_since_date = user_since_d)

#Are there any duplicated data
nrow(stream_data1[duplicated(stream_data1) == TRUE, ]) # 165659 records
stream_data1 = stream_data1[duplicated(stream_data1) == FALSE, ]  # Remove dups

head(stream_data1)

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

#Users per tag 
hist(table(stream_data1$tag1)) # uniformly distributed

#Are datatypes correct?
str(stream_data1)
cols = c('country','lang_code','role_id','tag1', 'tag2')
stream_data1[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
head(stream_data1,4)

#Create Stream Master
stream_master = subset(stream_data1, select = c(stream_id))
stream_master = stream_master[duplicated(stream_master) == FALSE, ]
stream_master2 = cbind(stream_master, tag_data)
stream_data2 = merge(stream_data1, stream_master2, by.x = "stream_id", by.y = "stream_id", all.x = T)

head(stream_data1,4)
head(stream_data2,4)

write.csv(stream_data2, "StreamActivityData.csv")
write.csv(stream_master2, "Stream_Master.csv")


length(unique(category_data2$tag1)); length(unique(stream_master2$tag3)); length(unique(stream_data2$tag3))
length(unique(category_data2$tag2)); length(unique(stream_master2$tag4)); length(unique(stream_data2$tag4))
length(unique(category_data2$tag3)); length(unique(stream_master2$tag5)); length(unique(stream_data2$tag5))

plot(table(stream_data2$country, stream_data2$tag3))
plot(table(stream_data2$country, stream_data2$tag4))
plot(table(stream_data2$country, stream_data2$tag5))

save(stream_data2, file="Stream.RData")
save(stream_master2, file="Stream_Master.RData")
rm(stream_data, stream_data1, stream_master)

##Question Tagging
question_data = fread("user_assessments", sep = ",")

head(question_data,4)
question_data = subset(question_data, select = -c(V1))
question_data = rename(question_data, user_id = masked_user_id, tag1 = question_tags, tag2 = V2)
question_data$submission_date = substr(question_data$submission_utc_ts , start = 1, stop = 10)
question_data$submission_date = as.Date(question_data$submission_date, "%Y-%m-%d")
length(unique(question_data$question_id)); length(unique(question_data$question_tags))
table(question_data$question_tags)
question_data[question_data$question_tags == "tag-000b59d2",]
sapply(question_data, function(col) sum(is.na(col))) 

#Are there any duplicated data
question_data[duplicated(question_data) == TRUE, ] # 63 record
question_data = question_data[duplicated(question_data) == FALSE, ]  # Remove dups
question_data2 = subset(question_data, select = -c(tag1, tag2))
question_data2 = question_data2[duplicated(question_data2) == FALSE, ]  # Remove dups
head(question_data2,2)


#Create Question Master
question_master = subset(question_data2, select = c(question_id))
question_master = question_master[duplicated(question_master) == FALSE, ]
question_master2 = cbind(question_master, category_data2)
question_data3 = merge(question_data2, question_master2, by.x = "question_id", by.y = "question_id", all.x = T)

head(question_data2,4)
head(question_data3,4)


write.csv(question_data3, "QuestionActivityData.csv")
write.csv(question_master2, "Question_Master.csv")


length(unique(category_data2$tag1)); length(unique(question_master2$tag1)); length(unique(question_data3$tag1))
length(unique(category_data2$tag2)); length(unique(question_master2$tag2)); length(unique(question_data3$tag2))
length(unique(category_data2$tag3)); length(unique(question_master2$tag3)); length(unique(question_data3$tag3))

plot(table(question_data3$country, question_data3$tag1))
plot(table(question_data3$country, question_data3$tag2))
plot(table(question_data3$country, question_data3$tag3))

save(question_data3, file="Question.RData")
save(question_master2, file="Question_Master.RData")
rm(question_data,question_data2,question_master)

############################### Diff Approach###############

head(stream_data,4)
head(question_data,4)

stream_data4 = subset(stream_data, select = c(country, deck_id))
stream_data4 = stream_data4[duplicated(stream_data4) == FALSE, ]  # Remove dups

question_data4 = subset(question_data, select = c(country, question_tags))
question_data4 = question_data4[duplicated(question_data4) == FALSE, ]  # Remove dups

question_data5 = cbind(question_data4, tag_data)

head(stream_data4,4); head(question_data5,4)
table(stream_data4$country); table(question_data4$country); table(stream_data5$country)

cntry_lst = unique(stream_data4$country)
stream_data5 = cbind(stream_data4[stream_data4$country == "ABS"], question_data5[question_data5$country == "ABS", c("question_tags","tag1","tag2","tag3")])

for ( c in cntry_lst){
  stream_data5 = rbind(stream_data5, cbind(stream_data4[stream_data4$country == c,], question_data5[question_data5$country == c, c("question_tags","tag1","tag2","tag3")]))
}

colnames(stream_data5)[2] <-"stream_id"
colnames(stream_data5)[2] <-"deck_id"

head(stream_data5, 4)
head(stream_data1, 4)
head(stream_data, 4)
head(stream_data3, 4)
head(stream_data6, 4)

length(unique(stream_data5$stream_id));length(unique(stream_data1$stream_id))
table(stream_data4$country); table(stream_data5$country)
stream_data3 = merge(stream_data1, stream_data5[,c("stream_id","country","question_tags","tag1","tag2","tag3")], by = c("stream_id","country"))

stream_data6 = merge(stream_data, stream_data5[,c("deck_id","country","question_tags","tag1","tag2","tag3")], by = c("deck_id","country"))

question_data6 = merge(stream_data, stream_data5[,c("deck_id","country","question_tags","tag1","tag2","tag3")], by = c("deck_id","country"))

head(question_data,4)

#Verification

stream_data6
question_data

length(unique(stream_data6$question_tags)); length(unique(question_data$question_tags))
nrow(unique(stream_data6[stream_data6$country == "IN",c("question_tags")]))
nrow(unique(question_data[question_data$country == "IN",c("question_tags")]))

stream_data6[stream_data6$country == "IN",c("question_tags")]
question_data[question_data$country == "IN",c("question_tags")]

stream_data6[stream_data6$question_tags == "tag-4db5b3ba",]
question_data[question_data$question_tags == "tag-4db5b3ba",]

write.csv(stream_data6, "views_model_withtags.csv")

