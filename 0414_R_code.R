setwd("D://DM")
bank = read.csv("#5-bank-data.csv")

set.seed(123)
n= nrow(bank)
sindex = sample(n,round(n*0.7))
Train = bank[sindex,]
Test = bank[-sindex,]

library(RWeka)
ctree = J48(pep~.,data = Train,control = Weka_control(M=2,C=0.25))
print(ctree)

library(partykit)
rparty.tree = as.party(ctree)
rparty.tree
plot(rparty.tree)

Test$predict = predict(ctree,Test,type='class')

cm = table(Test$pep,Test$predict,dnn = c("實際","預測"))

acc = sum(diag(cm))/sum(cm)
acc


acc = vector("numeric",10)

for (i in c(2:10)){
  ctree = J48(pep~.,data = Train,control = Weka_control(M=i,R=FALSE))
  Test$predict = predict(ctree,Test,type = "class")
  cm = table(Test$pep,Test$predict,dnn = c("實際","預測"))
  
  acc[i] = sum(diag(cm))/sum(cm)
}
acc

