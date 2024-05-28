# 2.1 (a)
library(itsmr)
model = lm(airpass$\sim$time(airpass))
summary(model)

# 2.1 (b)
acf(resid(model), 100, main="detrended")
acf(airpass, 100, main="original")

# 2.1 (c)
acf(diff(airpass), 100, main="1st diff")
acf(resid(model), 100, main="detrended")
