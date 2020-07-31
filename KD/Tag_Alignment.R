
#Load Packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
stream_data = fread("Original\\views_model", sep = ",")
question_data = fread("Original\\user_assessments", sep = ",")
tag_data = read.csv("Categories.csv", sep = ",")
tag_data = subset(tag_data, select = -c(X))


head(stream_data,4)
head(question_data,4)
head(tag_data,4)

#Get unique streams at country level
stream_data4 = subset(stream_data, select = c(country, deck_id))
stream_data4 = stream_data4[duplicated(stream_data4) == FALSE, ]  # Remove dups

#Get unique question_tags at country level
question_data4 = subset(question_data, select = c(country, question_tags))
question_data4 = question_data4[duplicated(question_data4) == FALSE, ]  # Remove dups

#Add descriptive tags (additional) to the existing question_tags
question_data5 = cbind(question_data4, tag_data)

#verify the data
head(stream_data4,4); head(question_data5,4)
table(stream_data4$country); table(question_data4$country); table(stream_data5$country)

#Get country list from stream data & create a dummy data table 
#with structure equal to stream data + tags (question_tags + descriptive tags)
cntry_lst = unique(stream_data4$country)
stream_data5 = cbind(stream_data4[stream_data4$country == "ABS"], question_data5[question_data5$country == "ABS", c("question_tags","tag1","tag2","tag3")])

#Enrich unique stream at country level data with tags from question data - randomise at country level. 
for ( c in cntry_lst){
  stream_data5 = rbind(stream_data5, cbind(stream_data4[stream_data4$country == c,], question_data5[question_data5$country == c, c("question_tags","tag1","tag2","tag3")]))
}
head(stream_data5,4)


#Enrich main stream activity now using stream and country wise tag information
stream_data6 = merge(stream_data, stream_data5[,c("deck_id","country","question_tags","tag1","tag2","tag3")], by = c("deck_id","country"))
head(stream_data, 4)
head(stream_data6, 4)

#Enrich main quesiton activity now with discriptive tags
question_data6 = merge(question_data, question_data5[,c("question_tags","country","tag1","tag2","tag3")], by = c("question_tags","country"))
head(question_data,4)
head(question_data6,4)

#Verification
stream_data6
question_data6
length(unique(stream_data6$question_tags)); length(unique(question_data$question_tags))
nrow(unique(stream_data6[stream_data6$country == "IN",c("question_tags")])); nrow(unique(question_data[question_data$country == "IN",c("question_tags")]))
stream_data6[stream_data6$country == "IN",c("question_tags")]
question_data[question_data$country == "IN",c("question_tags")]
stream_data6[stream_data6$question_tags == "tag-4db5b3ba",]
question_data[question_data$question_tags == "tag-4db5b3ba",]

#Save enriched data
write.csv(stream_data6, "views_model_withtags.csv")
write.csv(question_data6, "user_assessments_withtags.csv")
save(stream_data6, file="views_model_withtags.RData")
save(question_data6, file="user_assessments_withtags.RData")
