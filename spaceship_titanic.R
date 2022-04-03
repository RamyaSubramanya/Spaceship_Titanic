setwd(dir = "D:\\DATA ANALYTICS\\CLASSIFICATION & REGRESSION\\Classification Practicals\\spaceship-titanic")

train<-read.csv("train.csv")
test<-read.csv("test.csv")

#--------------------------------------------------------------------------------

#Exploratory data analysis and Data Pre-processing

#check the structure of the dataset
str(train)

#Removing variables not required for analysis
train<-train[,-c(1,4,13)]
test<-test[,-c(1,4,13)]

#check the relationship between categorical data and target variable using chi-square test
#Ha: p-value<alpha, variables are significant
#H0: p-value>alpha, variables are insignificant

chisq.test(train$HomePlanet, train$Transported, simulate.p.value = TRUE)

chisq.test(train$CryoSleep, train$Transported, simulate.p.value = TRUE)

chisq.test(train$Destination, train$Transported, simulate.p.value = TRUE)

chisq.test(train$VIP, train$Transported, simulate.p.value = TRUE)

#from the analysis, we found that all the 4 categorical variables are significant. 

-------------------------------------------------------------------------------

#handling missing values for train and test data

colSums(is.na(data))  

hist(data$Age)

train$Age[which(is.na(train$Age))]<-median(train$Age, na.rm = TRUE)

train$RoomService[which(is.na(train$RoomService))]<-median(train$RoomService, 
                                                           na.rm = TRUE)

train$FoodCourt[which(is.na(train$FoodCourt))]<-median(train$FoodCourt, 
                                                       na.rm = TRUE)

train$ShoppingMall[which(is.na(train$ShoppingMall))]<-median(train$ShoppingMall, 
                                                             na.rm = TRUE)

train$Spa[which(is.na(train$Spa))]<-median(train$Spa, na.rm = TRUE)

train$VRDeck[which(is.na(train$VRDeck))]<-median(train$VRDeck, na.rm = TRUE)


---------------------------------------------------------------------------------


test$Age[which(is.na(test$Age))]<-median(test$Age, na.rm = TRUE)

test$RoomService[which(is.na(test$RoomService))]<-median(test$RoomService, 
                                                         na.rm = TRUE)

test$FoodCourt[which(is.na(test$FoodCourt))]<-median(test$FoodCourt, 
                                                     na.rm = TRUE)

test$ShoppingMall[which(is.na(test$ShoppingMall))]<-median(test$ShoppingMall, 
                                                           na.rm = TRUE)

test$Spa[which(is.na(test$Spa))]<-median(test$Spa, na.rm = TRUE)

test$VRDeck[which(is.na(test$VRDeck))]<-median(test$VRDeck, na.rm = TRUE)

--------------------------------------------------------------------------------

#converting categorical dataset into factor/dummies for train and test data

str(train)

install.packages("fastDummies")
library(fastDummies)

train<-dummy_cols(train, select_columns = "HomePlanet")
train<-dummy_cols(train, select_columns = "CryoSleep")
train<-dummy_cols(train, select_columns = "Destination")
train<-dummy_cols(train, select_columns = "VIP")


train<-train[,-c(1,2,3,5,12,16,19,23)]

train<-train[,-c(10,11,14,16)]

--------------------------------------------------------------------------------
  
#converting categorical data for test data

test<-dummy_cols(test, select_columns = "HomePlanet")
test<-dummy_cols(test, select_columns = "CryoSleep")
test<-dummy_cols(test, select_columns = "Destination")
test<-dummy_cols(test, select_columns = "VIP")


test<-test[,-c(1,2,3,5,11,15,18,22)]

test<-test[,-c(9,10,13,15)]

--------------------------------------------------------------------------------
  
#converting categorical target variable into factor

train$Transported<-factor(train$Transported, 
                          levels = c("True", "False"), 
                          labels = c(1,0)) 
                          

#if you get error saying indefinite columns or so.

colnames(train)<-make.names(colnames(train))


#formatting column names

install.packages("janitor")
library(janitor)

train<-clean_names(train)
test<-clean_names(test)

--------------------------------------------------------------------------------
#Logistic regression
  
install.packages("caret")
library(caret)

?glm
reg<-glm(transported~.,family = binomial, data = train)
summary(reg)
  
p1<-predict(reg, newdata = train, type = "response")
View(p1)

p1<-ifelse(p1>0.50, "1", "0")  
p1<-factor(p1)

confusionMatrix(p1, train$transported, positive = "1")

#Accuracy is 21%, Sensitivity is 20.60%, kappa is -0.57

---------------------------------------------------------------------------------
#Decision Tree

install.packages("rpart")
library(rpart)
  
library(rpart.plot)
library(rattle)
  
?rpart
tree<-rpart(transported~., data = train, method = "class")

tree
summary(tree)

fancyRpartPlot(tree, main = "Spaceship Titanic Decision Tree", cex = 0.6)

p2<-predict(tree, newdata = train, type = "class")
confusionMatrix(p2, train$transported, positive = "1")

#Accuracy is 76%, Sensitivity is 84.81%, Kappa is 0.53

--------------------------------------------------------------------------------
#10-fold cross validation
  
custom<-trainControl(method = "repeatedcv", number = 10, repeats = 5)
  
?train

set.seed(123)
tree1<-train(transported~., data = train, 
             trControl = custom, method = "rpart", tuneLength = 10)


tree1
summary(tree1)

p3<-predict(tree1, newdata = train)
confusionMatrix(p3, train$transported, positive = "1")

#Accuracy is 80%, Sensitivity is 82.59%, Kappa is 0.60

--------------------------------------------------------------------------------
#Random Forest
  
install.packages("randomForest")
library(randomForest)

set.seed(123)
forest<-randomForest(transported~., data = train)
plot(forest)
  
?tuneRF
  
  
tuneRF(train[,-c(7)], train[,c(7)], ntreeTry = 30, stepFactor=2, improve=0.05,
      trace=TRUE, plot=TRUE)


forest1<-randomForest(transported~., data = train, ntreetry = 30, mtry = 2)
  
p4<-predict(forest1, newdata = train, type = "class")
confusionMatrix(p4, train$transported, positive = "1")

#Accuracy is 84%, Sensitivity is 87%, Kappa is 0.68

--------------------------------------------------------------------------------

#Predict the random forest on Test data
  
p5<-predict(forest1, newdata = test)
p5<-ifelse(p5 == 1, "True", "False")
p5<-factor(p5)
View(p5)
write.csv(p6, "mypredictions.csv")
?write.csv

install.packages("tidyverse")
library(tidyverse)

?str_to_title
p6<-str_to_title(p5)

