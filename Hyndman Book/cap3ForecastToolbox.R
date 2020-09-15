library(tidyverse)
library(fpp)
library(fpp2)

# 3.1 SIMPLE FORECASTING METHOD -------------------------------------------

# Use as benchmarking

# Average Method

meanf(y, h = 3)

# Naive Method

naive(y, h = 3)

rwf (y, h = 3)

# Seasonal naive method

snaive(y, h = 3)

# Drift Method

rwf(y, h=3, drift = TRUE)

# Examples

# Set training data from 1992 to 2007
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts

autoplot(beer2) +
        autolayer(meanf(beer2, h=11),
                  series="Mean", PI=FALSE) +
        autolayer(naive(beer2, h=11),
                  series="Naïve", PI=FALSE) +
        autolayer(snaive(beer2, h=11),
                  series="Seasonal naïve", PI=FALSE) +
        ggtitle("Forecasts for quarterly beer production") +
        xlab("Year") + ylab("Megalitres") +
        guides(colour=guide_legend(title="Forecast"))

autoplot(goog200) +
        autolayer(meanf(goog200, h=40),
                  series="Mean", PI=FALSE) +
        autolayer(rwf(goog200, h=40),
                  series="Naïve", PI=FALSE) +
        autolayer(rwf(goog200, drift=TRUE, h=40),
                  series="Drift", PI=FALSE) +
        ggtitle("Google stock (daily ending 6 Dec 2013)") +
        xlab("Day") + ylab("Closing Price (US$)") +
        guides(colour=guide_legend(title="Forecast"))


# 3.2 TRANSFORMATIONS AND ADJUSTMENTS -------------------------------------

# Calendar adjustments

# Transformed a montly series on a day average series

dframe <-
        cbind(Monthly = milk,
              DailyAverage = milk/monthdays(milk))

autoplot(dframe, facet=TRUE) +
        xlab("Years") + ylab("Pounds") +
        ggtitle("Milk production per cow")

(lambda <- BoxCox.lambda(elec))

autoplot(BoxCox(elec,lambda))

# Bias adjustments


fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)

fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)

autoplot(eggs) +
        autolayer(fc, series="Simple back transformation") +
        autolayer(fc2, series="Bias adjusted", PI=FALSE) +
        guides(colour=guide_legend(title="Forecast"))


# 3.3 RESIDUAL DIAGNOSTICS ------------------------------------------------


autoplot(goog200) +
        xlab("Day") + ylab("Closing Price (US$)") +
        ggtitle("Google Stock (daily ending 6 December 2013)")

# Residual using naive

res <- residuals(naive(goog200))

autoplot(res) + xlab("Day") + ylab("") +
        ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

# Portmanteau tests for autocorrelation

# Box-Pierce test

Box.test(res, lag = 10, fitdf = 0)

Box.test(res,lag=10, fitdf=0, type="Lj")

checkresiduals(naive(goog200))

# 3.4 EVALUATING FORECAST -------------------------------------------------

# subset time series

window(ausbeer, start = 1995)

subset(ausbeer, start = length(ausbeer)-4*5) # last five years

subset(ausbeer, quarter = 1)

tail(ausbeer, 4*5)

# Forecast error

# Use only a part of the data to fit

beer2 <- window(ausbeer,start=1992,end=c(2007,4))  

beerfit1 <- meanf(beer2,h=10)

beerfit2 <- rwf(beer2,h=10)

beerfit3 <- snaive(beer2,h=10)

autoplot(window(ausbeer, start=1992)) +
        autolayer(beerfit1, series="Mean", PI=FALSE) +
        autolayer(beerfit2, series="Naïve", PI=FALSE) +
        autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
        xlab("Year") + ylab("Megalitres") +
        ggtitle("Forecasts for quarterly beer production") +
        guides(colour=guide_legend(title="Forecast"))

# use the other part to test

beer3 <- window(ausbeer, start=2008)

accuracy(beerfit1, beer3)

accuracy(beerfit2, beer3)

accuracy(beerfit3, beer3)

# Google example

googfc1 <- meanf(goog200, h=40)

googfc2 <- rwf(goog200, h=40)

googfc3 <- rwf(goog200, drift=TRUE, h=40)

autoplot(subset(goog, end = 240)) +
        autolayer(googfc1, PI=FALSE, series="Mean") +
        autolayer(googfc2, PI=FALSE, series="Naïve") +
        autolayer(googfc3, PI=FALSE, series="Drift") +
        xlab("Day") + ylab("Closing Price (US$)") +
        ggtitle("Google stock price (daily ending 6 Dec 13)") +
        guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)

accuracy(googfc1, googtest)

accuracy(googfc2, googtest)

accuracy(googfc3, googtest)

# Cross validation

e <- tsCV(goog200, rwf, drift = TRUE, h = 1)

sqrt(mean(e^2, na.rm = TRUE))

sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))

# Pipe operation

goog200 %>% 
        tsCV(forecastfunction = rwf,
             drift = T,
             h = 1) -> e

e^2 %>% 
        mean(na.rm = T) %>% 
        sqrt()


# Other example

e <- tsCV(goog200, forecastfunction=naive, h=8)

# Compute the MSE values and remove missing values

mse <- colMeans(e^2, na.rm = T)

# Plot the MSE values against the forecast horizon

data.frame(h = 1:8, MSE = mse) %>%
        ggplot(aes(x = h, y = MSE)) + geom_point()

# 3. PREDICTION INTERVALS -------------------------------------------------

naive(goog200)

autoplot(naive(goog200))

# bootstrap - several simulations of future using the past

naive(goog200, bootstrap=TRUE)

autoplot(naive(goog200, bootstrap = T))


# 3.6 FORECAST PACKAGE ----------------------------------------------------


