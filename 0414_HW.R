setwd("D://DM")
test = read.csv("#5-titanic-test.csv")
train = read.csv("#5-titanic-train.csv")
set.seed(123)

acc = vector("numeric",10)

for (i in c(2:10)){
  ctree = J48(Survived~.,data = train,control = Weka_control(M=i,R=FALSE))
  train$predict = predict(ctree,train,type = "class")
  cm = table(train$Survived,train$predict,dnn=c("act","pred"))
  acc[i] = sum(diag(cm))/sum(cm)
}
acc