#Load packages
pacman::p_load(tidyverse,lubridate,date,stringi,data.table,dplyr,stringr,magrittr,tm,wordcloud,RColorBrewer,recosystem,
               slam,topicmodels,SnowballC)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("assessments_with_tags.RData")
question_data = question_data6; rm(question_data6)
dim(question_data)
head(question_data, 4)

#Load data
load("streams_with_tags.RData")
cntry_lst = unique(stream_data4$country)

#check unique values & NA in each
apply(question_data[,c(1,2,8:15)], 2, function(x) length(unique(x)))
sapply(question_data, function(col) sum(is.na(col))) 
sapply(stream_data4, function(col) sum(is.na(col)))

#Unique Tags
apply(question_data[,c(9:15)], 2, function(x) length(unique(x)))
apply(stream_data4[,c(14:20)], 2, function(x) length(unique(x)))

#Missing Tags
apply(question_data[,c(9:15)], 2, function(x) sum(is.na(x)))
apply(stream_data4[,c(14:20)], 2, function(x) sum(is.na(x)))


#Unique questions with tags
question_master = unique(question_data[,c(1,9:15)])
sapply(question_master, function(col) sum(is.na(col)))
apply(question_master, 2, function(x) length(unique(x)))
question_master %<>% select(-tag2, -tag3, -tag4, -tag7)
question_master$all_desc_tags = paste0(question_master$tag5," ",question_master$tag6)

# Tag Data Exploration --> we can outut some possible choices that user can choose from ?
categories <- gsub(" ", "-",question_master$all_desc_tags)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = question_master$question_id, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightTfIdf))
catsMat <- as.matrix(corpusCatdtm);catsMat[1:5,1:5]
cats <- colnames(catsMat)
sort(colSums(catsMat), decreasing = T) %>% head
wordcloud(cats, colSums(catsMat), colors=dark2 <- brewer.pal(6, "Dark2"), random.order = F, max.words = 100)

#Build feature matrix
question_master1 = unique(question_data[,c(1,9:15)])
sapply(question_master1, function(col) sum(is.na(col)))
apply(question_master1, 2, function(x) length(unique(x)))
question_master1[duplicated(question_master1) == TRUE, ]
question_master1$all_tags = paste0(question_master$tag1," ",
                                   question_master$tag2," ",
                                   question_master$tag3," ",
                                   question_master$tag4," ",
                                   question_master$tag5," ",
                                   question_master$tag6," ",
                                   question_master$tag7)
corpus <- Corpus(VectorSource(question_master1$all_tags))
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightBin))
inspect (dtm)
dtm_ti <- weightTfIdf(dtm)
inspect(dtm_ti)

dt <- as.data.table(as.matrix(dtm_ti))
dt[1:2,1:20]
dt <- cbind(question_id = question_master1$question_id, dt)
#save(dt, file = "dt.RData")

sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm_ti))/(sqrt(col_sums(t(dtm_ti)^2) %*% t(col_sums(t(dtm_ti)^2))))
as.matrix(sim_mat_cos)[1:5, 1:10]


recQst = function (simm, quest, k) {
  found <- sort(simm[, quest], decreasing = T)[2:(k+1)]
  print(found)
  cat(paste0("Selected Question: <ID>", question_master1[quest, 1], "\n<question>", question_master1[quest, 2]))
  # cat("\nRecommended Questions:\n")
  resindex <- as.integer(names(found))
  print(resindex)
  for (i in 1:k) {
    cat(paste0("\n",i,"-", resindex[i], " <qid>", question_master1[resindex[i], 1], "\n<question>", question_master1[resindex[i], 2]))
    #cat(paste0("\n",i,".", " <question_id>", question_master1[resindex[i], 1]))
    #print(question_master1[resindex[i], 1])
  }
}

names(sort(sim_mat_cos[, 6279], decreasing = T)[2:(2+1)])
question_master1[7030,1]
question_master1[7030,1]
question_master1[7030,1]

recQst(sim_mat_cos, 3, 2)


question_master1[3,]
question_master1[16627,]
question_master1[7756,]

question_master1[question_master1$question_id %in% c(7,11,24871),]
question_master1[question_master1$question_id == 9127,]
question_master1[question_master1$question_id %in% c(4418,2415,4518),]
question_master1[question_master1$tag1 == 'tag-13b3f31a' & question_master1$tag2 == 'tag-ce9cc2e6',]
                   
                   
# calculate based on cosine distance, output top 300 closest restaurants 
distances <- c(dist(newMat, as.data.frame(dt)[, keywords], method = "cosine"))
dist_mat_cos <- as.matrix(dist(mat_ti, method = "cosine"))
choiceUniverse <- data.table(bizname = dt$bizname ,dist = distances) %>% arrange(dist) %>% as.data.table %>% head(topNRes)


#Unique streams with tags
stream_master = unique(stream_data4[,c(1,14:20)])
sapply(stream_master, function(col) sum(is.na(col))) 
stream_master %<>% select(-tag2, -tag3, -tag4, -tag7)
stream_master$all_desc_tags = paste0(stream_master$tag5," ",stream_master$tag6)

# Tag Data Exploration --> we can outut some possible choices that user can choose from ?
categories <- gsub(" ", "-",stream_master$all_desc_tags)
categories <- gsub(";", " ",categories)
categories <- gsub("/", " ",categories)
categories <- gsub("-&-", "&",categories)
categoriesdt <- data.table(name = stream_master$deck_id, categories = categories)
corpusCat <- Corpus(VectorSource(categoriesdt$categories))
corpusCatdtm <- DocumentTermMatrix(corpusCat, control = list(weighting = weightTfIdf))
catsMat <- as.matrix(corpusCatdtm);catsMat[1:5,1:5]
cats <- colnames(catsMat)
sort(colSums(catsMat), decreasing = T) %>% head
wordcloud(cats, colSums(catsMat), colors=dark2 <- brewer.pal(6, "Dark2"), random.order = F, max.words = 100)


#Unique values 
length(unique(question_data6[question_data6$country %in% c(cntry_lst),]$masked_user_id)) # Unique Users - 27473
length(unique(question_data6[question_data6$country %in% c(cntry_lst),]$question_id)) # Unique Questions  - 8047
length(unique(stream_data4[stream_data4$country %in% c(cntry_lst),]$deck_id)) # Unique streams  - 2754


#Question take-up rate per country

question_data6[question_data6$country %in% c(cntry_lst),] %>%
  group_by(country) %>% 
  summarise(q_count = length(unique(question_id)),
                                                   u_count = length(unique(masked_user_id)),
                                                   q_rate = length(unique(question_id))/length(unique(masked_user_id)))

# Userbase per country
question_data6[question_data6$country %in% c(cntry_lst),] %>%
  group_by(country) %>% summarise(q_count = length(unique(question_id)),
                                  u_count = length(unique(masked_user_id))) %>%
  ggplot(aes(x = reorder(country, -u_count),
             y = u_count,
             fill = u_count)) +
  geom_bar(stat = 'identity',  position = 'dodge')

# Questions per country
question_data6[question_data6$country %in% c(cntry_lst),] %>%
  group_by(country) %>% summarise(q_count = length(unique(question_id)),
                                  u_count = length(unique(masked_user_id))) %>%
  ggplot(aes(x = reorder(country, -q_count),
             y = q_count,
             fill = q_count)) +
  geom_bar(stat = 'identity',  position = 'dodge')



#Mean number of questions tried/attempted in each country
question_data6[question_data6$country %in% c(cntry_lst),] %>%
  group_by(country) %>% summarise(mean_trials = mean(no_of_trials), mean_points = mean(points_earned)) %>%
  #top_n(10, wt = mean_trials) %>%
  ggplot(aes(x = reorder(country, -mean_trials),
             y = mean_trials,
             fill = mean_trials)) +
  geom_bar(stat = 'identity',  position = 'dodge')

#Mean number of questions answered in each country
question_data6[question_data6$country %in% c(cntry_lst),]  %>%
  group_by(country) %>% 
  summarise(mean_trials = mean(no_of_trials), mean_points = mean(points_earned)) %>%
  #top_n(10, wt = mean_points) %>%
  ggplot(aes(x = reorder(country, -mean_points),
             y = mean_points,
             fill = mean_points)) +
  geom_bar(stat = 'identity')


