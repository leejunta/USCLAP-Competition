###read in data
install.packages('randomForest')
install.packages("reprtree")
install.packages('ROCR')
install.packages('asbio')
install.packages('gamlr')
library(rpart)
library(gamlr)
library(asbio)
library(ROCR)
library(reprtree)
library(readr)
library(randomForest)

set.seed(1212016)

setwd("C:/Users/noahr/Documents/POL397")
survey <- read.csv("C:/Users/noahr/Downloads/csv.csv")
dim(survey)

##generate binary outcome variable
#first, delete unneeded employment categories
# unemp: 4
# emp: 1 2 5
# rem: 7 98 99 6 8 3
droplist <- c(3,6,7,8,98,99)

for (i in droplist){
  survey <- survey[survey$emplnw!=i,]
}

#assign 1 for all values except 4
survey[, "employed"] <- 1
survey$employed[survey$emplnw==4] <- 0


###NA's
stdvars<-colnames(survey)[c(-1,-4,-5,-34,-35,-39,-41,-45,-46,-47)];stdvars
for (f in c(stdvars)) {
  for (g in (1:(dim(survey)[1]))) {
    if (survey[g,f]==8) {
      survey[g,f]<-NA
    } else if (survey[g,f]==9) {
      survey[g,f]<-NA
    }
  }
}
#survey$educ2[survey$educ2==98|survey$educ2==99] <- NA
survey$party[survey$party==4|survey$party==5|survey$party==8|survey$party==9] <- NA
#survey$inc[survey$inc==98|survey$inc==99] <- NA

### Removing variables
survey$emplnw <- NULL
survey$weight <- NULL
survey$standwt<- NULL
survey$psraid <- NULL
survey$disa <- NULL #disability is highly correlated 
survey$inc <- NULL #correlated with employment
survey$cregion <- NULL
survey$state <- NULL
survey$age[survey$age==99] <- NA

###
#converting variables into factors
ind <- match("age", colnames(survey)); ind #finding index of continuous variable; 
match("employed",colnames(survey))
lst <- c(1:length(colnames(survey))); lst
lst <- lst[-ind]; lst
lst <- lst[-(match("employed",colnames(survey))-1)]; lst

for (i in lst){
  survey[,i] <- factor(survey[[i]])
}


##SPLITTING INTO
##TRAINING + TESTING
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

cv_set <- splitdf(survey)
train <- cv_set$trainset
test <- cv_set$testset
#############
###RPART TREE
set.seed(1206)
tree <- rpart(employed~ ., data=train, method="class", cp = 0.0001)
rpart.plot::prp(tree, 
                #extra=106,
                shadow.col = "gray",
                branch.lty = 3,
                extra = 102) #plot the tree; important variables are educ2, sex, q1, age, eminuse, 
#bbhome3e, game2c, age, marital, race, game3a
summary(tree)
par(mfrow=c(1,1))
plotcp(tree) #plot the complexity parameter
printcp(tree)
#prune tree: look at CP
rpart.plot::prp(prune(tree,cp=0.001), , shadow.col = "gray",
                branch.lty = 3)
par(mfrow=c(1,1))

##TESTING
tree.te <- predict(tree1, test)
y_te <- test$employed
tree.te <- prediction(predictions=tree.te[,2], labels = y_te)

t_acc <- performance(tree.te, "acc")
plot(t_acc)
t_fpr <- performance(tree.te, "fpr") 
plot(t_fpr)
t_auc <- performance(tree.te, "auc")


tree_cd <- ConDis.matrix(tree.te[,2], test$employed)
options(max.print=5.5E5)
options(max.print=1000)

sum(tree_cd == 1, na.rm = TRUE) #number concordant
sum(tree_cd == -1, na.rm = TRUE) #number discordant
sum(tree_cd == 0, na.rm = TRUE) #number tied -> 166908




############################
#using only importantvariables from CART
#survey <- 
survey2 <- survey[c("employed","educ2","sex","q1","age","eminuse",
                    "bbhome3e","game2c","race","marital",
                    "race","game3a","cable1","q3","ideo","date4e",
                    "hisp", "bbhome3a", "bbhome3b", "date3a", "date3b",
                    "date4a", "date4b", "date4c")]

#only complete cases
survey2 <- survey2[complete.cases(survey2),]; dim(survey2)


###get in randomforest format
#need to be factors (32 levels or less), no NAs
  rftest <- randomForest(as.factor(employed) ~ .,
                         data = train,
                         importance = TRUE,
                         method = "class",
                         na.action = na.omit,
                         ntree = 2000)
varImpPlot(rftest) #see important variables
plot(rftest)



####################################################
####################################################


tree1 <- rpart(employed ~ ., data = train,
                 method = "class", 
                 control = rpart.control(minsplit = 20, xval = 81, cp = 0.01))
tree1$cptable
plotcp(tree1)
summary(tree1)
tree1$variable.importance


tree2 <- rpart(employed ~ ., data = train,
                 method = "class", 
                 cp = 0.0273,
                 control = rpart.control(minsplit = 20, xval = 81))

lasso_te <- predict(tree2, train)
sum(pred==actual)/length(actual)
sum()

tree2$variable.importance

rpart_1$cptable
rpart_1$variable.importance
plotcp(tree1)

rpart.plot::prp(tree1, 
                #extra=106,
                shadow.col = "gray",
                branch.lty = 3,
                extra = 106,
                main = "rpart: Pruned Tree")

rpart.plot::prp(tree2, 
                #extra=106,
                shadow.col = "gray",
                branch.lty = 3,
                extra = 106,
                main = "rpart: Pruned Tree")
summary(tree2)

preds <- predict(tree2, test)

tree_cd <- ConDis.matrix(preds[,2], test$employed)

options(max.print=5.5E5)
options(max.print=1000)

sum(tree_cd == 1, na.rm = TRUE) #number concordant
sum(tree_cd == -1, na.rm = TRUE) #number discordant
sum(tree_cd == 0, na.rm = TRUE) #number tied -> 166908

y_te <- test$employed
tree.te <- prediction(predictions=preds[,2], labels = y_te)
tree.perf <- performance(tree.te, "acc")
plot(t_acc)

rpart.perf <- performance(tree.te, "tpr", "fpr"); tree.perf
tree.perf@x.values[[1]][max(which(tree.perf@alpha.values[[1]] >= 0.5))] #tpr -> 0.7864
tree.perf@y.values[[1]][max(which(tree.perf@alpha.values[[1]] >= 0.5))] #fpr -> 0.9285

plot(rpart.perf, avg= "threshold", colorize=T, lwd= 3,main= "CART (rpart), AUC = 0.6083")


rpart.perf <- performance(tree.pred, "acc"); tree.perf
tree.perf@y.values[[1]][max(which(tree.perf@x.values[[1]] >= 0.5))] #0.8175 -> ACCURACY
#81.29713

rpart.perf <- performance(tree.pred, "auc"); rpart.perf #60830

