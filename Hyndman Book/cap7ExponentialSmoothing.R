
library(tidyverse)
library(forecast)
library(fpp)
library(fpp2)

# 7.1 Simple Exponential Smoothing ----------------------------------------

oildata <- window(oil, start=1996)

autoplot(oildata) +
        ylab("Oil (millions of tonnes)") + xlab("Year")

# Estimate parameters

fc <- ses(oildata, h=5)

fc$model

# Accuracy of one-step-ahead training errors

round(accuracy(fc),2)

autoplot(fc) +
        autolayer(fitted(fc), series="Fitted") +
        ylab("Oil (millions of tonnes)") + xlab("Year")


# 7.2 TREND METHODS -------------------------------------------------------

# hOLT

air <- window(ausair, start=1990)

fc <- holt(air, h=5)

fc$model

# Damped trend


fc <- holt(air, h=15)

fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)

autoplot(air) +
        autolayer(fc, series="Holt's method", PI=FALSE) +
        autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
        ggtitle("Forecasts from Holt's method") + xlab("Year") +
        ylab("Air passengers in Australia (millions)") +
        guides(colour=guide_legend(title="Forecast"))

fc2$model

# 7.3 HOLT-WINTERS --------------------------------------------------------

aust <- window(austourists,start=2005)

fit1 <- hw(aust,seasonal="additive")

fit2 <- hw(aust,seasonal="multiplicative")

autoplot(aust) +
        autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
        autolayer(fit2, series="HW multiplicative forecasts",
                  PI=FALSE) +
        xlab("Year") +
        ylab("Visitor nights (millions)") +
        ggtitle("International visitors nights in Australia") +
        guides(colour=guide_legend(title="Forecast"))

# Damped

fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
         damped = TRUE, seasonal="multiplicative", h=35)

autoplot(hyndsight) +
        autolayer(fc, series="HW multi damped", PI=FALSE)+
        guides(colour=guide_legend(title="Daily forecasts"))


# 7.5 INNOVATIONS STATE SPACE ---------------------------------------------


# 7.6 ESTIMATION AND MODEL SELECTION --------------------------------------

# Uses likelyhood

aust <- window(austourists, start=2005)

fit <- ets(aust)

summary(fit)

autoplot(fit)

cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
        autoplot(facet=TRUE) + xlab("Year") + ylab("")


# 7.7 Forecasting with ETS models -----------------------------------------

fit %>% forecast(h=8) %>%
        autoplot() +
        ylab("International visitor night in Australia (millions)")
