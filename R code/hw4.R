# mwilder
library(readr)

df = read.csv("O:/Arr Matey/Auto.csv", header=T, na.strings="?")
df = na.omit(df)

y = df$mpg
x = df$horsepower

model1 = lm(y~x)
summary(model1)
par(mfrow=c(2,2))
plot(model1)



par(mfrow=c(2,2))
sqrtX =sqrt(x)
logX =log(x)
squareX =x^2
plot(sqrtX,y)
plot(logX,y)
plot(squareX,y)

model2= lm(y~logX)
summary(model2)
plot(model2)

logY = log(y)
plot(logX, logY)

model3 = lm(logY~logX)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
