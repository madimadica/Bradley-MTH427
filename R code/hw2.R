
#11.5
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c(27.6, 32.5, 35.9, 39.3, 44.2, 48.8, 55.7, 62.9)
y = y * 1000 # fix cost scaling
linear_regression_model = lm(y~x)
summary(linear_regression_model)



#11.17b
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c(27.6, 32.5, 35.9, 39.3, 44.2, 48.8, 55.7, 62.9)
y = y * 1000 # fix cost scaling
x = (x - 4.5) / 0.5;
linear_regression_model = lm(y~x)
summary(linear_regression_model)
