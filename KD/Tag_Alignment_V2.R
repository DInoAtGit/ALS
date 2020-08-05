
#Load Packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
stream_data = fread("Original\\views_model", sep = ",")
question_data = fread("Original\\user_assessments", sep = ",")

head(stream_data,4)
head(question_data,4)

#Get unique question_tags at question_id & country level
question_data2 = subset(question_data, select = c(country, question_id, question_tags))
question_data2 = question_data2[duplicated(question_data2) == FALSE, ]  # Remove dups

#Each question has multiple tags but in long form. Convert it to wide.
question_tag_wide = unique(question_data2[,2:3])
question_tag_wide = reshape(transform(question_tag_wide, time=ave(question_tags, question_id, FUN=seq_along)), idvar="question_id", direction="wide")

#Verify number of questions in original question data Vs number of questions in wide form.
nrow(question_tag_wide); length(unique(question_data$question_id))

#Verify how many max tags for each question (93), but most are blank. Max 4 tags are consistent with 85% tags. Hence only take 4 tags.
sapply(question_tag_wide, function(col) sum(is.na(col))) 
question_tag_wide[question_tag_wide$question_id %in% c(3,9026),]

#Bind the wide form with country + question level data
question_data3= merge(question_data2, question_tag_wide,by = c("question_id"))
question_data3[question_data3$question_id %in% c(3,9026),]

  #question_tags at country level - unique. Need this to enrich stream data.
  tag_data_cntry_wise = unique(question_data3 %>% select(2,4,5,6,7))
  tag_data_cntry_wise = rename(tag_data_cntry_wise, tag1 = question_tags.1, tag2 = question_tags.2, tag3 = question_tags.3, tag4 = question_tags.4)

#Merge the question tag wide form with original question data
question_data4 = merge(question_data, question_data3[,c("question_id","country","question_tags","question_tags.1","question_tags.2","question_tags.3","question_tags.4")], 
                       by = c("question_id","country","question_tags"))
question_data4[question_data4$question_id %in% c(3,9026),]

#Remove the long form question tags and take unique rows (all columns)
question_data5 = subset(question_data4, select = -c(question_tags))
question_data5[question_data5$question_id %in% c(3,9026) & question_data5$country %in% c('AO','BH'),]
question_data6  = unique(question_data5)
question_data6[question_data6$question_id %in% c(3,9026) & question_data6$country  %in% c('AO','BH'),]

#Tidy-up the names
question_data6 = rename(question_data6, tag1 = question_tags.1, tag2 = question_tags.2, tag3 = question_tags.3, tag4 = question_tags.4)

#Save enriched data
write.csv(question_data6, "user_assessments_horizontal_tags.csv")
save(question_data6, file="user_assessments_horizontal_tags.RData")


#For applying tags to stream data, verify data consistency across question and stream data sets
length(unique(question_data$country)); length(unique((stream_data$country)))
length(unique(question_data$masked_user_id)); length(unique((stream_data$user_id)))
length(unique(question_data$question_id)); length(unique((stream_data$deck_id)))

#Get unique streams at country level
stream_data2 = subset(stream_data, select = c(country, deck_id))
stream_data2 = stream_data2[duplicated(stream_data2) == FALSE, ]  # Remove dups

#Get country list from stream data & create a dummy data table 
#with structure equal to stream data + tags (question_tags + descriptive tags)
cntry_lst = unique(stream_data2$country)
stream_data3 = cbind(stream_data2[stream_data2$country == "ABS"], tag_data_cntry_wise[tag_data_cntry_wise$country == "ABS", c("tag1","tag2","tag3","tag4")])

#Enrich unique stream at country level data with tags from question data - randomise at country level. 
for ( c in cntry_lst){
  stream_data3 = rbind(stream_data3, cbind(stream_data2[stream_data2$country == c,], tag_data_cntry_wise[tag_data_cntry_wise$country == c, c("tag1","tag2","tag3","tag4")]))
}

#Verify if tag enriched-stream country level data is valid?
head(stream_data3,4)
table(stream_data2[stream_data2$country %in% cntry_lst, c("country")])
table(stream_data3[stream_data3$country %in% cntry_lst, c("country")])
table(tag_data_cntry_wise[tag_data_cntry_wise$country %in% cntry_lst, c("country")])

x = as.data.table(table(stream_data2[stream_data2$country %in% cntry_lst, c("country")]))
y = as.data.table(table(tag_data_cntry_wise[tag_data_cntry_wise$country %in% cntry_lst, c("country")]))
z = merge(x, y, by = c("V1"))
z$diff = z$N.x - z$N.y

stream_data2[stream_data2$country == 'BH',]
stream_data3[stream_data3$country == 'BH',]

stream_data3 %>% filter(country == 'BH') %>% group_by(deck_id) %>% summarise(count = n())

stream_data3[stream_data3$deck_id == '1623f3b0' & stream_data3$country == 'BH',]
stream_data2[stream_data2$deck_id == '1623f3b0' & stream_data2$country == 'BH',]
tag_data_cntry_wise[tag_data_cntry_wise$country == 'BH',]

question_data6[question_data6$country =='BH' & question_data6$tag1 == 'tag-13b3f31a',]
unique(question_data[question_data$question_id == 9026 & question_data$country == 'BH', c("question_tags")])

question_tag_wide[question_tag_wide$question_id == 9026]

#Enrich main stream activity now using stream and country wise tag information
stream_data4 = merge(stream_data, stream_data3[,c("deck_id","country","tag1","tag2","tag3","tag4")], by = c("deck_id","country"))


#Verify
head(stream_data, 4)
head(stream_data4, 4)
sapply(stream_data4, function(col) sum(is.na(col)))


#Save enriched data
write.csv(stream_data4, "views_model_horizontal_tags.csv")
save(stream_data4, file="views_model_horizontal_tags.RData")


#For loading
load("user_assessments_horizontal_tags.RData")
load("views_model_horizontal_tags.RData")

