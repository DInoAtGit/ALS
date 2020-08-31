
#Load packages
pacman::p_load(tm,slam,topicmodels,SnowballC,wordcloud,RColorBrewer)

#Set the directory
setwd("C:\\Dino\\NUS\\CapStone\\DataSet")

#Load data
load("assessments_with_tags.RData")

#read in data
question_data2
dim(question_data2)

#selet the text columns
qst <- question_data2[question_data2$country == 'IR', c("tag1","tag2","tag3","tag4","tag5","tag6","tag7")]
qst$alltags = paste0(qst$tag1," ",qst$tag2," ",qst$tag3," ",qst$tag4," ",qst$tag5," ",qst$tag6," ",qst$tag7)

#remove non-ASCII characters --> To take away non-english words
qst$alltags <- iconv(qst$alltags, "UTF-8", "ASCII",sub='')


#Preprocessing the text...
corpus <- VCorpus(VectorSource(qst$alltags))


for( i in 1:5){
  print(corpus[[i]][1])
} 

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


recQst(sim_mat_cos, 151, 10)

recQst(sim_mat_cos, 151, 3)
recQst(sim_mat_cos, 151, 3)

