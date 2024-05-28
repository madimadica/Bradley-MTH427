library(readr)

credit_data = read.csv("O:/Arr Matey/Credit.csv", header=T, na.strings="?")
credit_data = na.omit(credit_data)

quantitative_credit_data = credit_data[, c(1:6, 11)]

# Question 1a
summary(quantitative_credit_data)

# Question 1b
pairs(quantitative_credit_data)

# Question 1c
par(mfrow=c(2,3))
hist(credit_data$Income)
hist(credit_data$Limit)
hist(credit_data$Rating)
hist(credit_data$Age)
hist(credit_data$Education)
hist(credit_data$Balance)

# Question 1d
par(mfrow=c(2,3))
boxplot(credit_data$Income, main="Income")
boxplot(credit_data$Limit, main="Limit")
boxplot(credit_data$Rating, main="Rating")
boxplot(credit_data$Age, main="Age")
boxplot(credit_data$Education, main="Education")
boxplot(credit_data$Balance, main="Balance")
