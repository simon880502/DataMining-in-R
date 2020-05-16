setwd("D://DM")
data = read.csv("#5-bank-data.csv")
str(data)

set.seed(123)
n = nrow(data)
sindex = sample(n,round(0.7*n))
Train = data[sindex,]
Test = data[-sindex,]

library(rpart)
bank_clf = rpart(pep~.,data = Train,method = 'class')

library(rattle)
fancyRpartPlot(bank_clf)

library(rpart.plot)
prp(bank_clf,faclen = 0,fallen.leaves = TRUE,shadow.col = "gray")

library(partykit)
rparty.tree = as.party(bank_clf)
rparty.tree
plot(rparty.tree)

test_pred = predict(bank_clf,Test,type='class')
test_pred

cm = table(real=Test$pep,predic=test_pred)
acc = sum(diag(cm))/sum(cm)
acc

printcp(bank_clf)
plotcp(bank_clf)

ptree = prune(bank_clf,
              cp = bank_clf$cptable
              [which.min(bank_clf$cptable[,"xerror"]),"CP"])
ptree_pred = predict(ptree,Test,type='class')
ptree_pred
cm = table(real=Test$pep,predic=ptree_pred)
acc = sum(diag(cm))/sum(cm)
acc

library(caret)
library(e1071)
train_control = trainControl(method = "CV",number = 10)
train_control.model = train(pep~.,Train,method ="rpart",trControl=train_control)
train_control.model


