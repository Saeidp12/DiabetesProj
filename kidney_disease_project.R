

# --- Task 1 ------

#1 Read/Load the data set into R

library(readr)
library(RWeka)

kidney_disease <- read.arff("C://Users//User//Downloads//Chronic_Kidney_Disease//Chronic_Kidney_Disease//chronic_kidney_disease_full.arff")
kidney_disease
View(kidney_disease) 


#2 remove Pus Cell clumps, Sodium

kidney_disease$pcc <- NULL
kidney_disease$sod <- NULL
View(kidney_disease) 

#3 Check the structure of the data identify the following: # of instances, attributes, datatypes, missing values, etc
str(kidney_disease)
sum(is.na(kidney_disease))

#4 Provide general statistical description
summary(kidney_disease)

#5 Visualize the data in three different ways. Describe the plots in detail (give information). 

# boxplot 

boxplot(age~class, data=kidney_disease, xlab="class", 
        ylab = "age", main = "class vs age")

# Histogram 

hist(kidney_disease$age, xlab="age", col= "purple")

# Scatterplot 

my_cols <- c("#00AFBB", "#E7B800")
plot(x=kidney_disease$rbcc, y= kidney_disease$hemo,
     xlab = "rbcc",
     ylab = "hemo",
     main = "rbcc vs hemo", col = my_cols )





#=====================================================================

# --- Task 2 ------


#Hypothesis

anova_test <- aov(bp ~ class, data = kidney_disease)
print(summary(anova_test))


# --- Task 3 ------



#=========== Logistic Regression ===================================


set.seed(123)
DS <- sample(2, nrow(kidney_disease), replace = T, prob = c(0.7, 0.3))
train <- kidney_disease[DS == 1,]
test <- kidney_disease[DS == 2,]

#logistic regrssion
logistic <- glm(train$classification ~ train$ba + train$htn + train$dm 
                + train$cad + train$appet + train$ane , data = train, family = "binomial")
print(summary(logistic))


#predication
prediction <- predict(logistic, test, type = "response")
print(prediction)

#predication as 1/0
predictionIF <- ifelse(prediction > 0.5, 1, 0)
print(predictionIF)


#ROC
install.packages("pROC")
library(pROC)
ROC <- roc(response = test$class, prediction, plot= TRUE)
plot(ROC, col="blue", main ="roc curve for the kidney disease")



#confusion matricx
conf_mt <- table(actual = test, predicted = prediction)
View(conf_mt)
accuracy = sum(diag(conf_mt)) / sum(conf_mt)
print(accuracy)
precision = conf_mt[1,1]/colSums(conf_mt)[1]
print(precision)
recall = conf_mt[1,1]/rowSums(conf_mt)[1]
print(recall)


#=========== Naive Bayes ===================================


set.seed(123)
index <- sample(2, nrow(kidney_disease), replace = T, prob = c(0.7, 0.3))
training <- kidney_disease[index == 1,]
testing <- kidney_disease[index == 2,]

#model

library(e1071)
naive<- naiveBayes(training$class ~  training$ba + training$htn + training$dm + training$cad + training$appet + training$ane, data = training) 
print(naive)  


#=========================

#Model Evaluation

#Predict testing set
NB_Prediction <- predict(naive, newdata = testing)
NB_Prediction

#Confusion Matrix 
library(caret)
NB_CM = confusionMatrix(NB_Prediction, testing$class )
NB_CM

# model evaluation 
#---------------------

conf <- table(actual = testing$class, predicted= NB_Prediction )
accuracy= sum(diag(conf))/ sum(conf)
print(accuracy)

#---------------------

percision= conf[1,1]/colSums(conf)[1]
print(percision)

#---------------------

recall_DT = conf[1,1]/rowSums(conf)[1]
print(recall_DT)

#--------------------

#======================== DT ==================================

#create data framme
DT = data.frame( BA = kidney_disease$ba,
                 HTN = kidney_disease$htn,
                 DM = kidney_disease$dm,
                 CAD = kidney_disease$cad,
                 APPET = kidney_disease$appet,
                 ANE = kidney_disease$ane,
                 CLASS = kidney_disease$class)


#split the data into training/testing set
split_DT = sample(2, nrow(DT), replace = TRUE, prob = c(0.7,0.3))
DT_training <- DT[split_DT==1, ] 
DT_testing <- DT[split_DT ==2, ]


#required libraries
library(rpart)
library(rpart.plot)

CKD_DT = rpart(DT_training$CLASS ~.,DT_training)
print(CKD_DT)


# tree visualization
rpart.plot(CKD_DT)

#-----------------------------------------

# predict 
head(DT_testing,1)
summary(DT_testing)

DT_predction = predict(CKD_DT,DT_testing, type="class")
print(DT_predction)

# ---------------------------------------
#evaluotin


#confusion Matrix
DT_CM = confusionMatrix(DT_predction,DT_testing$CLASS)
print(DT_CM)

#------------------------
#accuracy
conf <- table(actual = DT_testing$CLASS, predicted= DT_predction )
accuracy= sum(diag(conf))/ sum(conf)
print(accuracy)

#----------------------
#percision
percision= conf[1,1]/colSums(conf)[1]
print(percision)

#------------------------
#recall
DT_recall = conf[1,1]/rowSums(conf)[1]
print(DT_recall)

