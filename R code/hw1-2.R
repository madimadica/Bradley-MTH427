library(readr)

data = read.csv("O:/Arr Matey/Hwk-data1.csv", header=T, na.strings="?")
data = na.omit(data)

tax_per_gal = data$Tax_per_gallon;

qqnorm(tax_per_gal)
qqline(tax_per_gal, col='red')

shapiro.test(tax_per_gal)
# Since p-value = 0.6469 > \alpha, the data are normally distributed

# Question 2b
t.test(data, conf.level=0.90)$"conf.int"

# Question 2c
t.test(data, conf.level=0.90, mu=45.2)
