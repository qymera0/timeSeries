library(tidyverse)
library(fpp)
library(fpp2)
library(GGally)

# 2.1 TS Objects ----------------------------------------------------------

y <- ts(c(123,39,78,52,110), start=2012)

# 2.2 TIME PLOTS ----------------------------------------------------------

autoplot(melsyd[ ,"Economy.Class"]) +
        ggtitle("Economy class passangers: Melbourne-Sydney") +
        xlab("Year") + 
        ylab("Thousands")


autoplot(a10) +
        ggtitle("Antidiabetic drug sales") +
        ylab("$ million") +
        xlab("Year")


# 2.4 SEASONAL PLOTS ------------------------------------------------------

ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE) +
        ylab("$ million") + 
        ggtitle("Seasonal plot: andiabetic drug sales")


ggseasonplot(a10, polar=TRUE) +
        ylab("$ million") +
        ggtitle("Polar seasonal plot: antidiabetic drug sales")


# # 2.5 SEASONAL SUBSERIES PLOT -------------------------------------------

ggsubseriesplot(a10) + 
        ylab("$ millin") +
        ggtitle("Seasonal subseries plot: antidiabetic drug sales")


# # 2.6 SCATERPLOTS -----------------------------------------------------------

autoplot(elecdemand[ ,c("Demand", "Temperature")], facets = TRUE) + 
     xlab("Year: 2014") + 
        ylab("") + 
        ggtitle("Half-hourly electricity demand: Victoria, Australia")

qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
        ylab("Demand (GW)") + xlab("Temperature (Celsius)")

autoplot(visnights[,1:5], facets=TRUE) +
        ylab("Number of visitor nights each quarter (millions)")


# Scaterplot matrices

ggpairs(as.data.frame(visnights[,1:5]))


# 2.7 LAG PLOTS -----------------------------------------------------------

beer2 <- window(ausbeer, start = 1992)

gglagplot(beer2)

# 2.8 AUTOCORRELATION ---------------------------------------------------

ggAcf(beer2)

# Trend and seasonality on ACF Plots

aelec <- window(elec, start = 1980)

autoplot(aelec)

ggAcf(aelec, lag = 48)


# 2.9 WHITE NOISE ---------------------------------------------------------

set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
