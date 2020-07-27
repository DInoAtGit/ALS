
#Load packages
pacman::p_load(tm,slam,topicmodels,SnowballC,wordcloud,RColorBrewer)

setwd("C:\\Dino\\Git\\ILS\\ILS\\QR")

#read in data
questions <- read.csv("Question_Classification_Dataset.csv", stringsAsFactor=FALSE)
questions[1,]
dim(questions)

#rename columns
colnames(questions)[1] <-"qid"
colnames(questions)[2] <-"question"

#selet the text columns
qst <- questions[, c('qid', 'question')]

#remove non-ASCII characters --> To take away non-english words
qst$question <- iconv(qst$question, "UTF-8", "ASCII",sub='')
head(qst)

#Preprocessing the text...
corpus <- VCorpus(VectorSource(qst$question))


for( i in 1:5){
  print(corpus[[i]][1])
} 

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
<<<<<<< HEAD
corpus <- tm_map(corpus, removeWords, stopwords('english'))   # for questions, stopwords add value & context
=======
#corpus <- tm_map(corpus, removeWords, stopwords('english'))   # for questions, stopwords add value & context
>>>>>>> 166b02824f8f86a53790de9fb8b5fbfda2715c23
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

#creating the matrix
dtm <- DocumentTermMatrix(corpus)
dtm_ti <- weightTfIdf(dtm)  #empty documents which represent non-english related moview as there is no overview text.
dtm_ti
as.matrix(dtm_ti)[1:5, 1:5]


#word cloud to check roughly what's in the data
tf <-sort(colSums(as.matrix(dtm_ti)), decreasing=TRUE)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(tf), tf, max.words=100, scale=c(5, .29), colors=dark2)

#You may try other common distance functions, 
#such as correlation, euclidean, jaccard

sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm_ti))/(sqrt(col_sums(t(dtm_ti)^2) %*% t(col_sums(t(dtm_ti)^2))))
as.matrix(sim_mat_cos)[1:5, 1:5]

#m1=as.matrix(dtm_ti)
# pacman::p_load(pdist)
# dists <- pdist(t(dtm_ti))
# as.matrix(dists)
# sim_mat_pearson = cor(t(m1), use="pairwise.complete.obs")
# as.matrix(sim_mat_pearson)[1:5, 1:5]

#to make the testing easier...
#simm-similarity matrix, movi-index of the selected movie, k-top k movies in result
recQst = function (simm, quest, k) {
  found <- sort(simm[, quest], decreasing = T)[2:(k+1)]
  print(found)
  cat(paste0("Selected Question: <ID>", qst[quest, 1], "\n<question>", qst[quest, 2]))
  cat("\nRecommended Questions:\n")
  resindex <- as.integer(names(found))
  print(resindex)
  for (i in 1:k) {
    cat(paste0("\n",i,"-", resindex[i], " <qid>", qst[resindex[i], 1], "\n<question>", qst[resindex[i], 2]))
  }
}


<<<<<<< HEAD
recQst(sim_mat_cos, 151, 10)

=======
recQst(sim_mat_cos, 151, 3)
recQst(sim_mat_cos, 151, 3)
>>>>>>> 166b02824f8f86a53790de9fb8b5fbfda2715c23

