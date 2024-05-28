library(readr)
df = read.csv("O:/Arr Matey/Auto.csv", header=T, na.strings="?")
df = na.omit(df)

mpg = df$mpg;
hp = df$horsepower

# 2.2 (a)
linear_regression_model = lm(mpg~hp)
summary(linear_regression_model)

# 2.2 (d)
confint(linear_regression_model, level=0.98)

# 2.2 (e)
plot(hp, mpg)

# 2.2 (f)
abline(linear_regression_model)
