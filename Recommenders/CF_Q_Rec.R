
#Load packages
pacman::p_load(tm,slam,topicmodels,SnowballC,wordcloud,RColorBrewer,recosystem)
pacman::p_load(recosystem)

#source custom functions
source("C:\\Dino\\NUS\\Sem2\\RS\\Workshop Files\\day1\\CF-demolib-v3.R")

#set dir
setwd("C:\\Dino\\Git\\ILS\\ILS\\QR")

assessment <- read.csv("Assessment.csv", header=TRUE)
head(assessment,5)
dim(assessment)
assessment = assessment[,c('id_assessment','id_student','score')]
names(assessment) = c("tagid", "userid", "pscore") #Rename columns in DF

#fillrate & factorization
fillrate(assessment)
amatrix = assessment
amatrix$userid <- as.factor(amatrix$userid)
amatrix$tagid <- as.factor(amatrix$tagid)
str(amatrix);str(assessment)

uniqusers <- length(unique(amatrix$userid))
uniqtags <- length(unique(amatrix$tagid))
uniqusers;uniqtags;dim(amatrix)

#Train & Testset
smp_size <- floor(0.9 * nrow(amatrix))
train_indexes <- sample(1: nrow(amatrix), size = smp_size)
trainassess <- amatrix[train_indexes, ]; dim(trainassess)
testassess  <- amatrix[-train_indexes, ]; dim(testassess)

# load into recosystem format
trainset = data_memory(trainassess$userid, trainassess$tagid, trainassess$pscore, index1= TRUE)
testset  = data_memory(testassess$userid, testassess$tagid, testassess$pscore, index1= TRUE)


reco <- Reco()
reco$train(trainset,opts = c(dim = 10, costp_12 = 0.1, costq_12 = 0.1, lrate = 0.08, niter = 30))
show(reco)

pred <- reco$predict(data_memory(closestUser_Index, 1:uniqItems), out_memory())
tempfinalReco <- data.table(business_id = unique(ratingsMatrixTrain$item), scores = pred)
out = merge(tempfinalReco, resSubset3, by.x = "business_id", by.y = "business_id", all.x = T) %>% select(Restaurant = name, PredScore = scores, City = city, Address = address) %>% arrange(-PredScore) %>% distinct()
out$Address <- gsub(pattern = "\"", "", out$Address)

CFrecommended <- out
head(CFrecommended,10) %>% select(Restaurant, PredScore, Address)
head(CBrecommended,10) %>% select(Restaurant, PredScore, Address)

# get predictions:  this multiplies the user vectors in testset, with the item vectors in Q
testset$prediction <- reco$predict(testset, out_memory())   # out_memory means output to memory, can also use "out_file"
head(testset)



