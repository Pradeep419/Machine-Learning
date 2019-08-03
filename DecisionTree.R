install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)

setwd(choose.dir())
getwd()

Titanic <- read_xls("titanic3.xls")
View(Titanic)
str(Titanic)

clean_titanic <- Titanic[,c(1,2,4,5,6,7,9,11)]
View(clean_titanic)

#Randomize the data

shuffle_index <- sample(1:nrow(clean_titanic))
clean_titanic <- clean_titanic[shuffle_index,]
head(clean_titanic)

clean_titanic$pclass <- factor(clean_titanic$pclass,levels=c(1,2,3),labels=c('Upper','Middle','Lower'))
clean_titanic$survived <- factor(clean_titanic$survived, levels = c(0,1), labels = c('No','Yes'))
clean_titanic <- na.omit(clean_titanic)
glimpse(clean_titanic)

install.packages("caTools")
library(caTools)

set.seed(123)

split = sample.split(clean_titanic,SplitRatio = 0.8)
training_set = subset(clean_titanic,split==TRUE)
testing_set = subset(clean_titanic,split==FALSE)
dim(testing_set)
dim(training_set)

install.packages("rpart.plot") #To build the decision treee model
library(rpart.plot)

fit <- rpart(training_set$survived~.,data=training_set, method = 'class')
rpart.plot(fit,extra = 106)
summary(fit)

predict_test<-predict(fit, testing_set, type='class')

table_mat<- confusionMatrix(testing_set$survived,predict_test)
table_mat
