
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


load("user_assessments_withtags.RData")
load("views_model_withtags.RData")

head(question_data6, 4)

length(unique(question_data6$question_tags))
length(unique(question_data6$question_id))


question_data6 %>% group_by(question_id, question_tags, country) %>% summarise(count = n())

question_data7 = subset(question_data, select = c(question_id, country, question_tags))

question_data7 = question_data7[duplicated(question_data7) == FALSE, ]  # Remove dups

View(question_data7 %>% group_by(question_id, country) %>% summarise(count = n()))
                                                                      
question_data[question_data$country == 'PK' & question_data$question_id == 747,]

question_data6[question_data6$country == 'AO' & question_data6$question_id == 3 & question_data6$masked_user_id == '41e96c5b',]
question_data[question_data$country == 'AO' & question_data$question_id == 3 & question_data$masked_user_id == '41e96c5b',]


head(question_data7, 4)


#################ANother Approach

# Creating the Question, COuntry, Tag Master
C_Q_Tag_M = unique(question_data %>% select(1,4,9))
length(unique(C_Q_Tag_M$question_tags));length(unique(C_Q_Tag_M$question_id));length(unique(C_Q_Tag_M$country))


##Searealizing the values of the column = question_tags into 4 columns max
sereliz = C_Q_Tag_M[,2:3]
sereliz_test =  reshape(transform(sereliz, time=ave(question_tags, question_id, FUN=seq_along)), idvar="question_id", direction="wide")
sereliz_test
sapply(sereliz_test, function(col) sum(is.na(col))) 
Q_TAQ_SER = sereliz_test[,1:5]
sapply(Q_TAQ_SER, function(col) sum(is.na(col))) 


#Country_Question_Master
C_Q_M = unique(C_Q_Tag_M[,1:2])

#Binding the Searlized Data set of Views wih counties 
C_Q_TAQ_SER= merge(C_Q_M, Q_TAQ_SER,by = c("question_id") )
#,c("country","question_id","question_tags.1","question_tags.2","question_tags.3","question_tags.4")

#Converging the Data Sets across Assess_dt and views_dt for moving it out to the rest of the teams
#1) There are Questiosn ID Duplicates in the C_Q_TAQ_SER.data.table
C_TAQ_SER_4_VIEWDATA = unique(C_Q_TAQ_SER[,2:6])

#2)#Creating the Country Masters for the different data.tables
Ctry_View_M = data.table(unique(stream_data$country))    #22

#3)Lookingup with Views(Streams) ID & Country Master Data_Table 
#Enrich unique stream at country level data with tags from question data - randomise at country level. 


#Country_Deck Master from Veiws tables
#stream_data4 = subset(stream_data, select = c(country, deck_id))
#stream_data4 = stream_data4[duplicated(stream_data4) == FALSE, ]  # Remove dups
CountryDeck_M = stream_data %>% select(10, 1)
CountryDeck_M  = unique(CountryDeck_M)
#CountryDeck_M = data.table(CountryDeck_M)



cntry_lst = unique(CountryDeck_M$country)
stream_data5 = data.table()

for ( c in cntry_lst){
  stream_data5 = rbind(stream_data5,cbind(CountryDeck_M[CountryDeck_M$country == c,],C_TAQ_SER_4_VIEWDATA[C_TAQ_SER_4_VIEWDATA$country == c,c("question_tags.1","question_tags.2","question_tags.3","question_tags.4")])
  )}
#head(stream_data5,4)

#4) Merging with the views_dt
#rm(views_sear_tags_dt)

stream_data_t = merge(stream_data,stream_data5[,c("country","deck_id","question_tags.1","question_tags.2","question_tags.3","question_tags.4")
                                                 ],by = c("country","deck_id"))

#5) Merging with the assess_dt
question_data_t = merge(question_data,C_Q_TAQ_SER[,c("country","question_id","question_tags.1","question_tags.2","question_tags.3","question_tags.4")
                                                  ],by = c("country","question_id"))



