install.packages("readxl")
library(readxl)

setwd(choose.dir())
getwd()

data <- read_xlsx("diabetes.xlsx")
View(data)
str(data)

#Change the output to Factors
data$class <- factor(data$class)
str(data)
head(data)


install.packages("tidyverse")
library(tidyverse)
?tidyverse
describe(data)

#Convert 0 values to NA

data[,2:7][data[,2:7]==0] <- NA
view(data)

install.packages("Amelia")
library(Amelia)

#Visualize missing data
missmap(data)

#USe Mice Pacakage to predict missing avlues
install.packages("mice")
library(mice)
mice_mod <- mice(data[,c("plas", "pres", "skin","insu","mass", "pedi")])
mice_complete <- complete(mice_mod)

#Transfer the predicted missing values

data$plas <- mice_complete$pres
data$pres <- mice_complete$pres
data$skin <- mice_complete$skin
data$insu <- mice_complete$insu
data$mass <- mice_complete$mass
data$pedi <- mice_complete$pedi

missmap(data)

#Visualization
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)

ggplot(data,aes(data$age, colour = data$class))+geom_freqpoly(binwidth = 1)+labs(title = "Age Distribution by Outcome")

install.packages("GGally")
library(GGally)
ggpairs(data)

#Split into Train and Test
indTrain <- createDataPartition(y = data$class, p=0.75, list = FALSE)
training <- data[indTrain,]
testint <- data[-indTrain,]

X = training[,-9]
Y = training$class
str(Y)
library(e1071)
model = train(X, Y, 'nb', trControl = trainControl(method = 'CV', number = 10))
model

predict <- predict(model,newdata = testint)
predict
confusionMatrix(predict,testint$class)

install.packages("caret")
library(caret)
X = varImp(model)
plot(X)

