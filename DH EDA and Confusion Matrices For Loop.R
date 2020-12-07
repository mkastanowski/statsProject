
library(multcomp)
library(ROCR) #Used for ROC/AUC
library(car) #used for GVIF (to address categorical variables) https://cran.r-project.org/web/packages/car/car.pdf
library(leaps) #For All Regression
library(caret)  #Used for cross validation
library(boot)  #Used for cross validation

library(jtools)


#EDA
setwd("C:/Users/Daniel/OneDrive/Desktop/Stats6021/Project2")
data <- read.csv('nassCDS.csv', header = TRUE)
attach(data)
#explore predictors we think will be significant
round(prop.table(table(data$dead, data$dvcat)),2) #proportion table of deaths versus estimated speed of crash
round(prop.table(table(data$dead, data$seatbelt)),3) #proportion table of deaths versus buckled
round(prop.table(table(data$dead, data$airbag)),3) #proportion table of deaths versus airbag availability
boxplot(ageOFocc ~dead, data = data) #boxplot of age of occupant versus death


#Running for loop of train test split

#set up data for running modeling and for loop of train test split
nass <- read.csv("nassCDS.csv", header = T)
lengths(nass)
nass=subset(nass, select = -c(X,caseid, weight, injSeverity))
lengths(nass)
which(is.na(nass), arr.ind=TRUE)
nass <- nass[complete.cases(nass), ]
lengths(nass)

nass$dead<-factor(nass$dead) 
nass$dvcat<-factor(nass$dvcat)
nass$airbag<-factor(nass$airbag)
nass$seatbelt<-factor(nass$seatbelt)
nass$sex<-factor(nass$sex)
nass$abcat<-factor(nass$abcat)
nass$occRole<-factor(nass$occRole)
nass$deploy<-factor(nass$deploy)
nass$abcat<-factor(nass$abcat)
nass$dead_num <- nass$dead
levels(nass$dead_num) <- c(0,1)
#remove column of dead
nass=subset(nass, select = -c(dead))
head(nass, 5)



#run the train test split 10 times and make 10 different confusion matrices 
#this is to ensure the train test split does not influence the confusion matrices that much
for(i in 1:10) {
#make train and test
sample<-sample.int(nrow(nass), floor(.50*nrow(nass)), replace = F)
train<-nass[sample, ]
test<-nass[-sample, ]

# Test Data Set - combine categories
test$New_dvcat=test$dvcat
levels(test$New_dvcat)<-c(levels(test$New_dvcat),"1-24")
test$New_dvcat[test$dvcat=="1-9km/h"]<-"1-24"
test$New_dvcat[test$dvcat=="10-24"]<-"1-24"
test$New_dvcat <- droplevels(test$New_dvcat)
levels(test$New_dvcat)
test$New_dvcat<- relevel(test$New_dvcat, ref = "1-24")
levels(test$New_dvcat)

# Train Data Set - combine categories
train$New_dvcat=train$dvcat
levels(train$New_dvcat)<-c(levels(train$New_dvcat),"1-24")
train$New_dvcat[train$dvcat=="1-9km/h"]<-"1-24"
train$New_dvcat[train$dvcat=="10-24"]<-"1-24"
train$New_dvcat <- droplevels(train$New_dvcat)
levels(train$New_dvcat)
train$New_dvcat<- relevel(train$New_dvcat, ref = "1-24")
levels(train$New_dvcat)

#selected final model
model_final.interaction1 <- glm(formula = dead_num ~ New_dvcat + seatbelt + frontal + sex + ageOFocc + 
                                  abcat + occRole + New_dvcat*seatbelt, family = "binomial", data = train, maxit = 100)


preds<-predict(model_final.interaction1,newdata=test, type="response")

print(table(test$dead_num, preds>0.05))}
#Number of false positives ranges from 104 to 128,
# the variability is not that great in confusion matrices based on random test splits