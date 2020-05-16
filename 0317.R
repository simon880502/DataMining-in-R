setwd("D:\\DM")
getwd()

bank = read.csv("#2-bank-data.csv")
View(bank)
str(bank)

bank = bank[,-1]
bank2 = bank[2,]
bank3 = bank[-2,]

sum(is.na(bank))

mean.age = mean(bank$age,na.rm = T)
mean.age = mean(bank[,1],na.rm = T)


na.rows = is.na(bank$age)
bank[na.rows,1] = mean.age

table(bank$married)
na.rows = is.na(bank$married)
bank[na.rows,"married"] = "YES"
sum(is.na(bank$married))


library(infotheo)
age1 = discretize(bank$age,"equalwidth",5)
income1 = discretize(bank$income,"equalfreq",5)

bank$age = age1
bank$income = income1

