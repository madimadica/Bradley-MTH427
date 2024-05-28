# 5
par(mfrow = c(3,2))

ARMA11=arima.sim(list(order=c(1,0,1), ar=.6, ma=0.9), n=100)
acf(ARMA11)
pacf(ARMA11)

AR1=arima.sim(list(order=c(1,0,0), ar=.6), n=100)
acf(AR1)
pacf(AR1)

MA1=arima.sim(list(order=c(0,0,1), ma=0.9), n=100)
acf(MA1)
pacf(MA1)

# 6a
library(itsmr)
par(mfrow = c(3,1))
plot(lake)
acf(lake)
pacf(lake)

# 6b
sarima(lake,2,0,0, no.constant=TRUE)

# 6c
sarima.for(lake, 12, 3, 0, 0)

# 7a
par(mfrow = c(3,1))
plot(airpass)
acf(airpass)
pacf(airpass)

# 7b
stableAirpass <- log(airpass)
fit=lm(stableAirpass~time(stableAirpass))
par(mfrow=c(2,1))
plot(as.ts(resid(fit)), main="detrended")
summary(fit)
acf(resid(fit),100, main="detrended")

# 7d
diffAirpass <- diff(stableAirpass)
acf(diffAirpass)

# 7f
acf2(diff(diffAirpass, 12), 120)

# 7g
sarima(stableAirpass, 1,1,1, 1,1,1, 12, no.constant=TRUE)
sarima(stableAirpass, 0,1,1, 0,1,1, 12,no.constant=TRUE)

# 7h
sarima.for(stableAirpass, n.ahead=12, 0, 1, 1, 0, 1, 1, 12)

