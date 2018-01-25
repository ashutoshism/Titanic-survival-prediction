setwd("C:/Users/ashutosh/Desktop/Titanic survival prediction")
titanic.train = read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test = read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$istrain = TRUE
titanic.test$istrain = FALSE
titanic.test$Survived = NA

titanic.full = rbind(titanic.train,titanic.test)

agemed = median(titanic.full$Age,na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"] = agemed

faremed = median(titanic.full$Fare,na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare),"Fare"] = faremed

titanic.full[titanic.full$Embarked=="","Embarked"] = "S"

titanic.full$Pclass = as.factor(titanic.full$Pclass)
titanic.full$Sex = as.factor(titanic.full$Sex)
titanic.full$Embarked = as.factor(titanic.full$Embarked)

titanic.train = titanic.full[titanic.full$istrain==TRUE, ]
titanic.test = titanic.full[titanic.full$istrain==FALSE, ]

titanic.train$Survived = as.factor(titanic.train$Survived)

survivedeq = "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survformula = as.formula(survivedeq)
install.packages("randomForest")
library(randomForest)

titanic.model = randomForest(formula = survformula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01*nrow(titanic.train)) )
Survived = predict(titanic.model, newdata = titanic.test)

PassengerId = titanic.test$PassengerId
op.df = as.data.frame(PassengerId)
op.df$Survived = Survived
write.csv(op.df, file = "Kaggle_submission.csv", row.names = FALSE)