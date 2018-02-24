library(tm)
library(RTextTools)
library(topicmodels)

getwd()
setwd("D:/Media/Documents/Data Science/Repositories/Text mining/topic modeling")

#Importing the data
uscong<-read.csv("USCongress.csv",header = T,na.strings = c("","NA","-","NaN"))
dim(uscong)

#To find the unique number of topics
length(unique(uscong$major))
unique(uscong$major)

#Creating a corpus and cleaning
uscong.corpus<-Corpus(VectorSource(uscong$text))
uscong.corpus<-tm_map(uscong.corpus,tolower)
uscong.corpus<-tm_map(uscong.corpus,stripWhitespace)
uscong.corpus<-tm_map(uscong.corpus,removePunctuation)
uscong.corpus<-tm_map(uscong.corpus,removeNumbers)
uscong.corpus<-tm_map(uscong.corpus,removeWords,c(stopwords("english"),"bill","amend","states","united","act"))

#Creating DTM
matrix<-DocumentTermMatrix(uscong.corpus)

#Sampling into train and test
train<-matrix[1:3114,]
test<-matrix[3115:4449,]

#Modeling
train.ldm<-LDA(train,20)

#Topics and terms
get_topics(train.ldm,5)[1:5,1:5]
get_terms(train.ldm,5)
topics(train.ldm)
terms(train.ldm)

#writing the most frequent terms in a document to csv file
write.csv(data.frame(get_terms(train.ldm,20)),"doc_terms.csv",row.names = F)

#testing the model
#Posterior probability
test.topics<-posterior(train.ldm,test)
test.topics$topics[1:10,1:5]
test.topics<-apply(test.topics$topics,1,which.max)

uscongtest<-uscong[3115:4449,]
final<-data.frame(Text=uscongtest$text,Pred_Topic=test.topics)

#Analysis
table(final$Pred_Topic)
View(final[final$Pred_Topic==1,])