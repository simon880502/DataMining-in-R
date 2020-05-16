setwd("C://DM")
Train = read.csv("#5-titanic-train.csv")
Test = read.csv("#5-titanic-test.csv")

Train$Pclass = as.factor(Train$Pclass)
# Train$Parch = as.factor(Train$Parch)
Train$SibSp = as.factor(Train$SibSp)
Test$Pclass = as.factor(Test$Pclass)
# Test$Parch = as.factor(Test$Parch)
Test$SibSp = as.factor(Test$SibSp)

library(rpart)
bank_clf = rpart(Survived~.,data = Train,method = 'class')

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

cm = table(real=Test$Survived,predic=test_pred)
acc = sum(diag(cm))/sum(cm)
acc

printcp(bank_clf)
plotcp(bank_clf)

ptree = prune(bank_clf,
              cp = bank_clf$cptable
              [which.min(bank_clf$cptable[,"xerror"]),"CP"])
ptree_pred = predict(ptree,Test,type='class')
ptree_pred
cm = table(real=Test$Survived,predic=ptree_pred)
acc = sum(diag(cm))/sum(cm)
acc
