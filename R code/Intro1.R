library(readr)
Auto=read.csv("O:/Arr Matey/Auto.csv", header=T, na.strings="?")
dim(Auto)
Auto=na.omit(Auto)
plot(mpg, cylinders)
plot(Auto$mpg, Auto$cylinders)
attach(Auto)
plot(mpg, cylinders)
pairs(Auto[, 3:6])
hist(mpg)

par(mfrow=c(2,2)) # If you want to graph 4 plots side by side "2 rows" and "2 columns"
hist(displacement, col=1, breaks=5)
hist(horsepower, col=2, breaks=5)
hist(weight, col=3, breaks=5)
hist(acceleration, col=4, breaks=5)
summary(Auto[, 3:6])# produces summary of the variables located at column 3-6#
summary(horsepower) # produces numerical summary of the variable in the 
boxplot(horsepower, col="blue", border="brown")# produces a box plot for 'horsepower'
savehistory()# saves a record of all commands that we typed in the most session before exiting R.
loadhistory() # to load the most recent history.
