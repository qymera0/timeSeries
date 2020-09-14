

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

