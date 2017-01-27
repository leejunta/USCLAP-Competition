###read in data
#install.packages('Matrix',dependencies = T)
#install.packages('tcltk',dependencies = T)
#install.packages('gplots',dependencies = T)
install.packages('randomForest',dependencies = T)
#install.packages('ReprTree',dependencies = T)
#install.packages('ROCR',dependencies = T)
#install.packages('asbio',dependencies = T)
#install.packages('gamlr',dependencies = T)
install.packages('rpart',dependencies = T)
#library(gamlr)
#library(asbio)
#library(ROCR)
#library(reprtree)
#library(readr)
library(randomForest)
require(rpart)
install.packages('maptree',dependencies = T)
require(maptree)
require(cluster)

#set.seed(1212016)

survey <- read.csv("data.csv",header=T)
dim(survey)

#using 99 as NA
newsurvey<-survey
for (f in 1:dim(newsurvey)[2]) {
#  levels(newsurvey[,f])<-c(levels(newsurvey[,f]),99)
  newsurvey[,f][is.na(newsurvey[,f])]=99
}
#newsurvey$age.1<-NULL
colnam<-colnames(newsurvey)[c(-(match("age",colnames(newsurvey))))]
for (f in colnam) {
  newsurvey[,f]<-factor(newsurvey[,f])
}

#######################################
#tree algorithm

install.packages('tree',dependencies = T)
require(tree)

cart3<-tree(employed~sample+lang+sex+q1+q3+eminuse+intmob+bbhome3a+
               bbhome3b+bbhome3c+bbhome3d+bbhome3e+cable1+date3a+date3b+
               date4a+date4b+date4c+date4d+date4e+date4f+game1+game2a+
               game2b+game2c+game2d+game2e+game2f+game3a+game3b+age+
               marital+hh1+par+educ2+party+ideo+hisp+race,data=newsurvey[1:(dim(newsurvey)[1]/2),])
draw.tree(cart3)
cv3<-cv.tree(cart3)
#plot(cv3)
ksize3<-cv3$size[which(cv3$dev==min(cv3$dev))]
prune3<- prune.misclass(cart3, best=ksize3)

#PLOT TREE
draw.tree(prune3)
summary(prune3)

pred3<-predict(prune3,newsurvey[(dim(newsurvey)[1]/2+1):dim(newsurvey)[1],-1],
               type='class')
prob3<-sum(pred3==actual)/length(actual);prob3
#0.823 correct

############################
#RANDOM FOREST

require(randomForest)
randfor1<- randomForest(employed~sample+lang+sex+q1+q3+eminuse+intmob+bbhome3a+
                          bbhome3b+bbhome3c+bbhome3d+bbhome3e+cable1+date3a+date3b+
                          date4a+date4b+date4c+date4d+date4e+date4f+game1+game2a+
                          game2b+game2c+game2d+game2e+game2f+game3a+game3b+age+
                          marital+hh1+par+educ2+party+ideo+hisp+race,
                        data=newsurvey[1:(dim(newsurvey)[1]/2),],
                        importance=T,
                        proximity=T,
                        method='class',
                        ntree=500)

     
varImpPlot(randfor1)
plot(randfor1t)

pred<-predict(randfor1,newsurvey[(dim(newsurvey)[1]/2+1):dim(newsurvey)[1],-1])
actual<-as.factor(newsurvey[(dim(newsurvey)[1]/2+1):dim(newsurvey)[1],dim(newsurvey)[2]])
length(pred)
sum(actual==pred)
prob=sum(actual==pred)/length(pred); prob

#############################################
#USING CART GIVEN VARIABLES
#IGNORE
randfor2<- randomForest(employed~educ2+sex+date4d+marital,
                        data=newsurvey[1:(dim(newsurvey)[1]/2),],
                        importance=T,
                        proximity=T,
                        method='class',
                        ntree=500)

pred2<-predict(randfor2,newsurvey[(dim(newsurvey)[1]/2+1):dim(newsurvey)[1],-1])
actual<-as.factor(newsurvey[(dim(newsurvey)[1]/2+1):dim(newsurvey)[1],dim(newsurvey)[2]])
length(pred2)
sum(actual==pred2)
prob2=sum(actual==pred2)/length(pred2); prob2

c(prob,prob2,prob3)

################################################################################
