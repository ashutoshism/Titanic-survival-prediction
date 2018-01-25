# accuracy =  0.79426

setwd("C:/Users/ashutosh/Desktop/Titanic survival prediction")
titanic.train = read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test = read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$istrain = TRUE
titanic.test$istrain = FALSE
titanic.test$Survived = NA

titanic.full = rbind(titanic.train,titanic.test)
titanic.full$nameinitial = sapply(titanic.full$Name, FUN = function(x) {strsplit(x,split = "[,.]")[[1]][2]})
titanic.full$nameinitial = sub(" ","",titanic.full$nameinitial)
titanic.full$nameinitial[titanic.full$nameinitial %in% c("Mlle","Mme")] = "Mlle"
titanic.full$nameinitial[titanic.full$nameinitial %in% c("Capt","Don","Jonkheer","Major","Sir")] = "Sir"
titanic.full$nameinitial[titanic.full$nameinitial %in% c("Dona","Lady","the Countess")] = "Lady"

titanic.full$nameinitial = as.factor(titanic.full$nameinitial)
titanic.full$Pclass = as.factor(titanic.full$Pclass)
titanic.full$Sex = as.factor(titanic.full$Sex)

titanic.full[titanic.full$Embarked=="","Embarked"] = "S"
titanic.full$Embarked = as.factor(titanic.full$Embarked)

library(rpart)
Agefit = rpart(Age ~ Sex + SibSp + Parch + nameinitial,
               data = titanic.full[!is.na(titanic.full$Age),], method = "anova")
titanic.full$Age[is.na(titanic.full$Age)] = predict(Agefit, titanic.full[is.na(titanic.full$Age),])

Farefit = rpart(Fare ~ Sex + SibSp + Parch + Pclass + Embarked + Age,
               data = titanic.full[!is.na(titanic.full$Fare),], method = "anova")
titanic.full$Fare[is.na(titanic.full$Fare)] = predict(Farefit, titanic.full[is.na(titanic.full$Fare),])



titanic.train = titanic.full[titanic.full$istrain==TRUE, ]
titanic.test = titanic.full[titanic.full$istrain==FALSE, ]

titanic.train$Survived = as.factor(titanic.train$Survived)

survivedeq = "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + nameinitial"
survformula = as.formula(survivedeq)
install.packages("randomForest")
library(randomForest)

set.seed(777)
titanic.model = randomForest(formula = survformula, data = titanic.train, ntree = 2000)
Survived = predict(titanic.model, newdata = titanic.test)

PassengerId = titanic.test$PassengerId
op.df = as.data.frame(PassengerId)
op.df$Survived = Survived
write.csv(op.df, file = "Kaggle_submission_2.csv", row.names = FALSE)
