library(readr)
df = read.csv("O:/Arr Matey/Auto.csv", header=T, na.strings="?")
df = na.omit(df)

mpg = df$mpg;
hp = df$horsepower

quadratic_model = lm(mpg~hp+I(hp^2))
summary(quadratic_model)

vcov(quadratic_model)
