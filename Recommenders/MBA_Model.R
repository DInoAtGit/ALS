
#load apriori package
pacman::p_load(arules,arulesViz)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

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

question_data1 = tibble::rowid_to_column(question_data, "activity_id")
question_data1$all_tags = paste0(question_data1$tag1,",",
                                question_data1$tag2,",",
                                question_data1$tag3,",",
                                question_data1$tag4,",",
                                question_data1$tag5,",",
                                question_data1$tag6,",",
                                question_data1$tag7)

write.csv(question_data1, file = "GB_question_data.csv")

q_trans = read.transactions("GB_question_data.csv", format = "basket", sep = ",", cols = c("activity_id", "all_tags"), header = TRUE)

summary(q_trans)
head(Groceries,4)
class(Groceries)
