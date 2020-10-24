
#Load Packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
stream_data = fread("Verify\\views_model.gz", sep = ",")
question_data = fread("Verify\\user_assessments.gz", sep = ",")
tag_data = read.csv("Categories.csv", sep = ",")
tag_data = subset(tag_data, select = -c(X))
tag_data = rename(tag_data, tag5 = tag1, tag6 = tag2, tag7 = tag3)
nrow(tag_data);length(unique(tag_data$tag5));length(unique(tag_data$tag6));length(unique(tag_data$tag7));length(unique(question_data$question_tags))


# stream_data = stream_data[stream_data$country != 'BH',]
# question_data = question_data[question_data$country != 'BH',]

head(stream_data,4)
head(question_data,4)

#Get unique question_tags at question_id & country level
question_data2 = subset(question_data, select = c(country, question_id, question_tags))
question_data2 = question_data2[duplicated(question_data2) == FALSE, ]  # Remove dups

#Each question has multiple tags but in long form. Convert it to wide.
question_tag_wide = unique(question_data2[,2:3])
question_tag_wide = reshape(transform(question_tag_wide, time=ave(question_tags, question_id, FUN=seq_along)), idvar="question_id", direction="wide")

#check unique values & NA in each
apply(question_tag_wide, 2, function(x) length(unique(x)))
sapply(question_tag_wide, function(col) sum(is.na(col))) 


#Add descritive tags
question_tag_wide = cbind(question_tag_wide, tag_data)

#Verify number of questions in original question data Vs number of questions in wide form.
nrow(question_tag_wide); length(unique(question_data$question_id))

#Verify how many max tags for each question (93), but most are blank. Max 4 tags are consistent with 85% tags. Hence only take 4 tags.
apply(question_tag_wide, 2, function(x) length(unique(x)))
sapply(question_tag_wide, function(col) sum(is.na(col))) 
question_tag_wide[question_tag_wide$question_id %in% c(3,9026),]


#Bind the wide form with country + question level data
question_data3= merge(question_data2, question_tag_wide, by = c("question_id"))
question_data3[question_data3$question_id %in% c(3,9026),]

  #Keep the country based unique tags aside for stream data enrichement
  tags_for_streams = subset(question_data3, select = -c(question_id, question_tags))
  tags_for_streams = unique(tags_for_streams)

#Merge the question tag wide form with original question data (withou longform tags) & Verify
question_data1 = subset(question_data, select = -c(question_tags))
question_data1= unique(question_data1)
question_data3.1 = subset(question_data3, select = -c(question_tags))
question_data3.1= unique(question_data3.1)
question_data4 = merge(question_data1, question_data3.1[,c("question_id","country","question_tags.1","question_tags.2","question_tags.3","question_tags.4","tag5","tag6","tag7")], 
                       by = c("question_id","country"))
question_data4[question_data4$question_id %in% c(3,9026),]
question_data[question_data$question_id %in% c(3) & question_data$country  %in% c('AO') & question_data$masked_user_id == '8c866202',]
question_data4[question_data4$masked_user_id == '8c866202',]
question_data4[question_data4$question_id == 3,]

#improtant verification 
  # Each question can have multiple tags (in multiple columns, not in the same column)
  # Each tag can have multiple questions 
question_data3.1 %>% group_by(question_id, country, tag7) %>% summarise(count = n()) %>% filter(count >1)
question_data6 %>% group_by(question_id, country, tag1) %>% summarise(count = n()) %>% filter(count >1)
question_data6[question_data6$question_id %in% c(3) & question_data6$country  %in% c('AO') & question_data6$tag1 == 'tag-13b3f31a',]

question_data[question_data$question_id %in% c(3) & question_data$country  %in% c('AO') ,]
question_data6[question_data6$question_id %in% c(3) & question_data6$country  %in% c('AO'),]

#Tidy-up the names
question_data6 = rename(question_data4, tag1 = question_tags.1, tag2 = question_tags.2, tag3 = question_tags.3, tag4 = question_tags.4)


#Save enriched data
write.csv(question_data6, "assessment_with_tags.csv")
save(question_data6, file="assessments_with_tags.RData")


#For applying tags to stream data, verify data consistency across question and stream data sets
length(unique(question_data$country)); length(unique((stream_data$country)))
length(unique(question_data$masked_user_id)); length(unique((stream_data$masked_user_id)))
length(unique(question_data$question_id)); length(unique((stream_data$deck_id)))

#question_tags at country level - unique. Need this to enrich stream data.
tag_data_cntry_wise = unique(question_data6 %>% select(2,9:15))

#Get unique streams at country level
stream_data2 = subset(stream_data, select = c(country, deck_id))
stream_data2 = stream_data2[duplicated(stream_data2) == FALSE, ]  # Remove dups
stream_data2 = rename(stream_data2, country_s = country)

#Get country list from stream data & create a dummy data table 
#with structure equal to stream data + tags (question_tags + descriptive tags)
cntry_lst = unique(stream_data2$country_s)
tags_for_streams = tags_for_streams[tags_for_streams$country %in% c(cntry_lst),]
tags_for_streams = unique(tags_for_streams)
tags_for_streams = rename(tags_for_streams, tag1 = question_tags.1, tag2 = question_tags.2, tag3 = question_tags.3, tag4 = question_tags.4)
tags_for_streams %>% group_by(country, tag1, tag2, tag3, tag4, tag5, tag6, tag7) %>% summarise(count = n()) %>% filter(count >1)


#define uqique id column for matching
stream_data2 = unique(stream_data2)
length(unique(stream_data2$deck_id))
table(tags_for_streams$country); table(stream_data2$country_s)

#Declare empty structure
stream_data3 = cbind(stream_data2[stream_data2$country_s == "ABS"], tags_for_streams[tags_for_streams$country == "ABS", c("country","tag1","tag2","tag3","tag4","tag5","tag6","tag7")])


#Enrich unique stream at country level data with tags from question data - randomise at country level. 
for ( c in cntry_lst){
  stream_data3 = rbind(stream_data3, cbind(stream_data2[stream_data2$country_s == c,], tags_for_streams[tags_for_streams$country == c, c("country","tag1","tag2","tag3","tag4","tag5","tag6","tag7")]))
}

#Verify merge
stream_data3 %>% group_by(deck_id, country, tag1) %>% summarise(count = n()) %>% filter(count >1)
stream_data3[stream_data3$deck_id %in% c('stream-011f5e6b') & stream_data3$country  %in% c('SA') & stream_data3$tag1 == 'tag-705388a3',]


#Remove duplicated tags as tags are than decks and cbind would have resulted in duplicate tags for the same deck. 
head(stream_data3,4)
stream_data3 = unique(stream_data3)
stream_data3[stream_data3$tag4 == 'tag-ef6a3c07',]
stream_data3.1 = stream_data3[duplicated(paste0(stream_data3$deck_id, stream_data3$country)) == FALSE, ]  # Remove dups
stream_data3[stream_data3$deck_id == '2b146be4' & stream_data3$country == 'US',]
stream_data3.1[stream_data3.1$deck_id == '2b146be4' & stream_data3.1$country == 'US',]
stream_data3[stream_data3$deck_id %in% c('000158b1') & stream_data3$country  %in% c('SA') & stream_data3$tag1 == 'tag-04b97134',]
stream_data3.1[stream_data3.1$deck_id %in% c('stream-011f5e6b') & stream_data3.1$country  %in% c('SA') & stream_data3.1$tag1 == 'tag-705388a3',]
stream_data3.1 = subset(stream_data3.1, select = -c(country_s))

#Enrich main stream activity now using stream and country wise tag information
stream_data4 = merge(stream_data, stream_data3.1[,c("deck_id","country","tag1","tag2","tag3","tag4","tag5","tag6","tag7")], by = c("deck_id","country"))


stream_data4[stream_data4$deck_id %in% c('stream-de0d5c6a') & country == 'US', c(1:5,13:20)]

#Verify
head(stream_data, 4)
head(stream_data4, 4)
sapply(stream_data4, function(col) sum(is.na(col)))
stream_data4[stream_data4$deck_id == 'stream-ed299586',]
stream_data4[stream_data4$tag1 == 'tag-759aa959',]
stream_data4[stream_data4$deck_id %in% c('000158b1') & stream_data4$country  %in% c('SA') & stream_data4$tag1 == 'tag-04b97134',]
length(unique(stream_data4$deck_id));length(unique(stream_data$deck_id));length(unique(paste0(stream_data4$deck_id, stream_data4$country)))
length(unique(stream_data4$tag1));length(unique(question_data6$tag1))
apply(stream_data4, 2, function(x) length(unique(x)))

#Save enriched data
write.csv(stream_data4, "streams_with_tags.csv")
save(stream_data4, file="streams_with_tags.RData")


#For loading
load("assessments_with_tags.RData")
load("streams_with_tags.RData")

dim(question_data);dim(question_data6)
dim(stream_data);dim(stream_data4)
head(stream_data4)


