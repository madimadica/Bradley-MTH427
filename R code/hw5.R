library(readr)
library(leaps)
library(MASS)
library(boot)

# Load Credit dataset
creditData = read.csv("O:/Arr Matey/Credit.csv", header=T)
attach(creditData)
MAX_PARAMS = 11

# (a) -----------------------------------------------
linearRegressionModel = regsubsets(Balance~., data=creditData, nvmax=MAX_PARAMS)
summary(linearRegressionModel) # Row 4 asterisks: Income, Limit, Cards, Student
coef(linearRegressionModel, 4) # Get model equation's coefficients



# (b) -----------------------------------------------
plot(linearRegressionModel, scale="Cp")
plot(linearRegressionModel, scale ="bic")
plot(linearRegressionModel, scale="adjr2")
coef(linearRegressionModel, 5) # Cp [5 predictors]
coef(linearRegressionModel, 4) # BIC [4 predictors]
coef(linearRegressionModel, 7) # adj rsquare [7 predictors]


# (c) -----------------------------------------------
forwardSelectionLR = regsubsets(Balance~., data=creditData, nvmax=MAX_PARAMS, method="forward")
summary(forwardSelectionLR)



# (e) -----------------------------------------------
plot(forwardSelectionLR, scale="Cp")
plot(forwardSelectionLR, scale ="bic")
plot(forwardSelectionLR, scale="adjr2")
coef(forwardSelectionLR, 6) # Cp [6 predictors]
coef(forwardSelectionLR, 5) # BIC [5 predictors]
coef(forwardSelectionLR, 7) # adj rsquare [7 predictors]

# (f) -----------------------------------------------
set.seed(1)
x_train = sample(400, 200)
x_test = -x_train
split_lm = lm(Balance~Income+Limit+Cards+Student, data=creditData, subset=x_train)
summary(split_lm)


# (g) -----------------------------------------------
y_predictions = predict(split_lm, newdata=creditData)
all_residuals = (Balance - y_predictions)
test_residuals = all_residuals[x_test]
split_lm_mse = mean(test_residuals^2)
split_lm_mse


# (h) -----------------------------------------------
loocv_model = glm(Balance~Income+Limit+Cards+Student, data=creditData)
loocvMSE <- cv.glm(creditData, loocv_model)$delta[2]
loocvMSE

kfold_model = glm(Balance~Income+Limit+Cards+Student, data=creditData)
kfoldMSE = cv.glm(creditData, kfold_model, K=10)$delta[2]
kfoldMSE

# (i) -----------------------------------------------
x_train = sample(c(TRUE, FALSE), nrow(creditData), rep=TRUE)
x_test = !x_train
bestSubsets = regsubsets(Balance~., data=creditData[x_train,], nvmax=MAX_PARAMS)
testMatrix = model.matrix(Balance~., data=creditData[x_test,])
test_mses = rep(NA, MAX_PARAMS)

for (modelSize in 1:MAX_PARAMS){
  coefficients = coef(bestSubsets, modelSize)
  y_predictions = testMatrix[, names(coefficients)]%*%coefficients
  y_test = Balance[x_test]
  test_mses[modelSize]= mean((y_test - y_predictions)^2)
}

test_mses                   # Best MSEs for each size
which.min(test_mses)        # Best MSE's Model Size


# (j) -----------------------------------------------
k=10
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

folds = sample(1:k, nrow(creditData), replace=TRUE)
test_mses = matrix(NA, k, MAX_PARAMS, dimnames = list(NULL, paste(1:MAX_PARAMS)))

for (currentFold in 1:k) {
  bestSubsets = regsubsets(Balance~., data=creditData[folds != currentFold, ], nvmax=MAX_PARAMS)
  for (modelSize in 1:MAX_PARAMS) {
    y_predictions = predict(bestSubsets, creditData[folds == currentFold, ], id=modelSize)
    y_test = Balance[folds == currentFold]
    test_mse = mean((y_test-y_predictions)^2)
    test_mses[currentFold, modelSize] = test_mse
  }
}
mean_errors = apply(test_mses, 2, mean)
mean_errors
which.min(mean_errors)
