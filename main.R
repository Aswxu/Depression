#uncomment to install packages
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("pROC")
#install.packages("klaR")
#install.packages("e1071")
#install.packages("randomForest")
#install.packages("party")
#install.packages("mboost")
#install.packages("C50")


require(caret)
require(rpart)
require(rpart.plot)
require(pROC)
require(klaR)
require(e1071)
require(randomForest)
require(party)
require(mboost)
require(C50)

set.seed(2389)


#read the dataset#
depress<-read.csv("dummy3.csv")

inTrain<-createDataPartition(y=depress$gold,
                             p=.8,
                             list=FALSE)

data.train<-depress[inTrain,]
data.test<-depress[-inTrain,]
x.test <- data.test[,2:27]
y.test <- data.test[,1]

#specify the controling settings
ctrl <- trainControl(method="cv", number=100,summaryFunction =twoClassSummary,classProbs = TRUE)

#cart
cart.model<-train(gold~.,
                 data=data.train,
                 metric="ROC",
                 method="rpart",
                 trControl = ctrl)

#c5.0
c50.grid<-expand.grid(trials=c(1:10),
                      model="tree",
                      winnow=c(TRUE,FALSE))
c50.model<-train(gold~.,
                 data=data.train,
                 metric="Accuracy",
                 method="C5.0",
                 tuneGrid = c50.grid,
                 trControl = ctrl)

#cost-sensitive cart
cs.cart.model<-train(gold~.,
                     data=data.train,
                     metric="ROC",
                     method="rpartCost",
                     trControl = ctrl)

#ada cart
ada.model<-train(gold~.,
                 data=data.train,
                 metric="ROC",
                 method="ada",
                 trControl=ctrl)



#Predict Output 
predicted= predict(c50.model,x.test)
confusionMatrix(predicted, y.test)

predicted= predict(cart.model,x.test)
confusionMatrix(predicted, y.test)


c50.model$finalModel$tuneValue
cat(c50.model$finalModel$output)

prp(cartmodel$finalModel)
summary(c50.model$finalModel)
plot(c50.model$finalModel,trial=1)
