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


require(caret)
require(rpart)
require(rpart.plot)
require(pROC)
require(klaR)
require(e1071)
require(randomForest)
require(party)
require(mboost)

#read the dataset#
depress<-read.csv("dummy3.csv")


# define an 80%/20% train/test split of the dataset
set.seed(2389)
split=0.80
trainIndex <- createDataPartition(depress$gold, p=split, list=FALSE)
data_train <- depress[ trainIndex,]
data_test <- depress[-trainIndex,]
x_test <- data_test[,2:27]
y_test <- data_test[,1]


#specify the controling settings
ctrl <- trainControl(method="cv", number=10,summaryFunction =twoClassSummary,classProbs = TRUE)


#1. CART#
#for twoclasssummary#
#cart.model <- train(gold~., data=data_train,metric="ROC",method="rpart", trControl=ctrl)
#for UNBALAN#
cart.model <- train(gold~., data=data_train,metric="ROC",method="rpart", trControl=ctrl)
#summary prediction accuracy
predictions <- predict(cart.model, x_test)
confusionMatrix(predictions, y_test)
#what about removing those combined variables?
#They are c(13,14,25,26,27).
cart.model.cp <- train(gold~., data=data_train[,-c(13,14,25,26,27)],metric="ROC",method="rpart", trControl=ctrl)
#x_test has been swifted, so
predictions.cp <- predict(cart.model.cp, x_test[,-c(12,13,24,25,26)])
confusionMatrix(predictions.cp, y_test)


#2. SVM? Blackbox, negate#
ls.svmLinear.model <- train(gold~., data=data_train, method="svmPoly", trControl=ctrl)



#3.Random Forest?#
rf.model <- train(gold~., data=data_train,metric="ROC",method="rpartCost", trControl=ctrl)
# Fitting model
fit <- randomForest(gold ~ ., data_train,ntree=500)
summary(fit)

#4.Boosting
boost.m1.model <- train(gold~., data=data_train,metric="ROC",method="AdaBoost.M1", trControl=ctrl)
boost.tree.model <- train(gold~., data=data_train,metric="ROC",method="blackboost", trControl=ctrl)

#5.Ada bag
ada.bag.model <-train(gold~.,data=data_train,metric="ROC",method="AdaBag",trControl=ctrl)
ada.model <-train(gold~.,data=data_train,metric="ROC",method="ada",trControl=ctrl)
#best till now!! but really hard too explain for pratical use
c5.model<-train(gold~.,data=data_train,metric="ROC",method="C5.0",trControl=ctrl)
c5.cost.model<-train(gold~.,data=data_train,metric="Accuracy",method="C5.0Cost",trControl=ctrl)

#Predict Output 
predicted= predict(fit,x_test)
confusionMatrix(predicted, y_test)



prp(cartmodel$finalModel)
summary(cartmodel$finalModel)

#????#
rt2 <- rpart(gold~., data = data_train,method="class")


#testing of custom train function: no not working
unbalanSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out=0
  for(n in 1:nrow(data)) {
    if(data[n,]$obs=="YES"){
      if(data[n,]$obs==data[n,]$pred){out=out+(1-0.06394523)}
    }
    if(data[n,]$obs=="NO"){
      if(data[n,]$obs==data[n,]$pred){out=out+0.06394523}
    }
  }
  names(out) <- "UNBALAN"
  out
}


#testing of custom train function2: no not working
imbalanSummary <- function (data,
                            lev = NULL,
                            model = NULL) {
  out=0
  for(n in 1:nrow(data)) {
    if(data[n,]$obs=="YES"){
      if(data[n,]$obs==data[n,]$pred){out=out+(1-0.06394523)}
    }
  }
  names(out) <- "IMBALAN"
  out
}


#try the EPE function: no not working
EPE<-function(data,
              lev = NULL,
              model = NULL) {
  c<-matrix(confusionMatrix(data$pred, data$obs)$table,nrow=2,ncol=2)
  #prior p(YES)=0.06394523
  out=c[1,2]*(1-0.06394523)*c[1,2]/(c[1,2]+c[2,2])+c[2,1]*(0.06394523)*c[2,1]/(c[2,1]+c[1,1])
  names(out)<-"EPE"
  out
}

#testing of exponential loss
expon <- function (data,
                  lev = NULL,
                  model = NULL) {
  temp=exp((2*(data$obs=="YES")-1)*(data$pred=="YES"))
  out=mean(temp)
  names(out) <- "EXPON"
  out
}