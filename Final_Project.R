# Load installed dependency packages
library('RColorBrewer')
library('lattice')
library('gridExtra')
library('fBasics')
library('ggplot2')
library('reshape2')
library('caret')
library('e1071')
library('rpart')

# Set working directory
setwd("/Users/fanglin/Documents/Stanford/Stats202/Final_Project")

# Load Training and Test data set
train <-read.csv("training.csv",header=TRUE)
training <- read.csv("training.csv", header=TRUE)
test <-read.csv("test.csv",header=TRUE)

# Show summary of the data
summary(train)
head(train)
nrow(train)
names(train)

# Check redundancy
length(unique(train$url_id))
length(unique(train$query_id))
length(unique(train$url_id))/nrow(train)
length(unique(train$query_id))/nrow(train)

# Check data duplication rate
sum(duplicated(train))
sum(duplicated(test))
sapply(train, function(x)all(is.na(x)))
sapply(test, function(x)all(is.na(x)))

# Change query_id, url_id and is_homepage to factors
lapply(train,class)
train[,c("query_id")] <- as.factor(train[,c("query_id")])
train[,c("url_id")] <- as.factor(train[,c("url_id")])
train[,c("is_homepage")] <- as.factor(train[,c("is_homepage")])
test[,c("query_id")] <- as.factor(test[,c("query_id")])
test[,c("url_id")] <- as.factor(test[,c("url_id")])
test[,c("is_homepage")] <- as.factor(test[,c("is_homepage")])

## probabibility that homepage and relevancy is just to chance
table(train$relevance, train$is_homepage)
pbinom(10471,size=(11057+10471),p=.4370862)
pbinom(24516,size=(24516+34002),p=.4370862)

binom.test(10471,21528,p=.4370862, alternative="two.sided")

## Classifier

# Logistic regression
logit = glm(relevance~is_homepage+sig1+sig2+sig3+sig4+sig5+sig6+sig7+sig8, training, family = binomial)
summary(logit)
require(boot)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
set.seed(20)
cv.glm(training, logit, cost, K = 10)$delta[1]

# cross validation
crossvalidation <- function(model,option,k) {
  folds = sample(1:10, nrow(training), replace = TRUE)
  model.table <- matrix(0,2,2)
  for(fld in unique(folds)) {
    train <- folds != fld
    test <- folds == fld
    if(option==1) {
      model.fit <- model(relevance ~ is_homepage+sig1+sig2+sig3+sig4+sig5+sig6+sig7+sig8, data = training, subset = train)
      model.predict <- predict(model.fit, newdata = training[test,])
      model.class = model.predict$class
      model.table <- model.table + table(model.predict$class, training[test,]$relevance)
    }
    if(option==2 | option==4) {
      model.fit <- model(relevance ~ is_homepage+sig1+sig2+sig3+sig4+sig5+sig6+sig7+sig8, data = training, subset = train)
      model.predict <- predict(model.fit, newdata = training[test,])
      model.table <- model.table + table(model.predict, training[test,]$relevance)
    }
    if(option==3) {
      model.predict = model(training[train,], training[test,], training[train,]$relevance, k)
      model.table <- model.table + table(model.predict, training[test,]$relevance)
    }
    if(option==5) {
      model.fit <- model(relevance ~ is_homepage+sig1+sig2+sig3+sig4+sig5+sig6+sig7+sig8, data = training, subset = train, kernel="linear")
      model.predict <- predict(model.fit, newdata = training[test,])
      model.pred = ifelse(model.predict > 0.5, 1, 0)
      model.table <- model.table + table(model.predict, training[test,]$relevance)
    }
  }
  return(model.table)
}

# Linear Discriminant Analysis
require(MASS)
set.seed(1)
crossvalidation(lda,1)
(18265+9773)/nrow(training)

# Quadratic Discriminant Analysis
set.seed(10)
crossvalidation(qda,1)
(30260+1489)/nrow(training)

# Naive Bayes 
library(e1071)
set.seed(100)
crossvalidation(naiveBayes,2)
(30072+1938)/nrow(training)

# K nearest Neighbor
library(class)
set.seed(1000)
crossvalidation(knn,3,1)
set.seed(3000)
crossvalidation(knn,3,3)
set.seed(5000)
crossvalidation(knn,3,5)
set.seed(10000)
crossvalidation(knn,3,10)

# SVM Radial
require(e1071)
crossvalidation(svm,4)
(20441+7208)/nrow(training)

# SVM Linear
crossvalidation(svm,5)
(19292+8637)/nrow(training)

# Decision Tree
require(tree)
decisiontree=tree(relevance~is_homepage+sig1+sig2+sig3+sig4+sig5+sig6+sig7+sig8,data=training)
summary(decisiontree)
plot(decisiontree)
text(decisiontree,all=T)

# Random Forest
library(randomForest)
y<-as.factor(training[,13])
x<-training[,4:12]
fit<-randomForest(x,y)
print(fit)
importance(fit)

# Make Predictions
test=read.csv("test.csv")
svm.fit <- svm(relevance ~ is_homepage+sig1+sig2+sig3+sig4+sig5+sig6+sig7+sig8, data = training)
test[,13]=predict(svm.fit,test[,4:12])
write.table(test[13],file = "tmp.txt")