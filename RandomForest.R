library(randomForest) 

indicators <- read.csv("zScore.csv",sep=";",na.strings="NaN",stringsAsFactors = FALSE)

classes <- read.csv("classes.csv",sep=";")
classes <- transform(classes,Class = as.factor(Class))

indicators$Stocks <- I(indicators$Stocks)

for (i in names(indicators)[-1]){
  indicators[[i]] <- as.numeric(indicators[[i]])
}


ind_rf <- randomForest(classes$Class~.,data=indicators[,-1],ntree=100,proximity=TRUE,na.action = na.omit)


table(predict(ind_rf),classes$Class)

print(ind_rf)
importance(ind_rf)
varImpPlot(ind_rf)


FeatureNumber<-tuneRF(indicators[,-1],indicators[,1],na.action = na.omit)



str(indicators)
names(indicators)
dim(indicators)
str(classes)
dim(classes$Class)
dim(indicators[-1,])

indicators[,1]