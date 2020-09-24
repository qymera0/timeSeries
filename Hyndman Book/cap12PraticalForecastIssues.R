library(tidyverse)
library(fpp)
library(fpp2)
library(fpp3)


# 12.1 Weekly, daily and sub-daily data -----------------------------------

gasoline %>% stlf() %>% autoplot()

bestfit <- list(aicc=Inf)

for(K in seq(25)) {
        fit <- auto.arima(gasoline, xreg=fourier(gasoline, K=K),
                          seasonal=FALSE)
        if(fit[["aicc"]] < bestfit[["aicc"]]) {
                bestfit <- fit
                bestK <- K
        }
}

fc <- forecast(bestfit,
               xreg=fourier(gasoline, K=bestK, h=104))

autoplot(fc)


fc <- forecast(bestfit,
               xreg=fourier(gasoline, K=bestK, h=104))
autoplot(fc)

# Time series of counts

productC %>% croston() %>% autoplot()


# 12.3 ENSURE FORECAST WITHIN LIMITS --------------------------------------

# Positive forecast

eggs %>%
        ets(model="AAN", damped=FALSE, lambda=0) %>%
        forecast(h=50, biasadj=TRUE) %>%
        autoplot()

# Forecasts constrained to an interval

# Bounds
a <- 50
b <- 400
# Transform data and fit model
fit <- log((eggs-a)/(b-eggs)) %>%
        ets(model="AAN", damped=FALSE)
fc <- forecast(fit, h=50)
# Back-transform forecasts
fc[["mean"]] <- (b-a)*exp(fc[["mean"]]) /
        (1+exp(fc[["mean"]])) + a
fc[["lower"]] <- (b-a)*exp(fc[["lower"]]) /
        (1+exp(fc[["lower"]])) + a
fc[["upper"]] <- (b-a)*exp(fc[["upper"]]) /
        (1+exp(fc[["upper"]])) + a
fc[["x"]] <- eggs
# Plot result on original scale
autoplot(fc)

# 12.4 FORECAST COMBINATIONS ----------------------------------------------

train <- window(auscafe, end=c(2012,9))

h <- length(auscafe) - length(train)

ETS <- forecast(ets(train), h=h)

ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),
                  h=h)

STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)

NNAR <- forecast(nnetar(train), h=h)

TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)

Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(auscafe) +
        autolayer(ETS, series="ETS", PI=FALSE) +
        autolayer(ARIMA, series="ARIMA", PI=FALSE) +
        autolayer(STL, series="STL", PI=FALSE) +
        autolayer(NNAR, series="NNAR", PI=FALSE) +
        autolayer(TBATS, series="TBATS", PI=FALSE) +
        autolayer(Combination, series="Combination") +
        xlab("Year") + ylab("$ billion") +
        ggtitle("Australian monthly expenditure on eating out")

c(ETS = accuracy(ETS, auscafe)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, auscafe)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, auscafe)["Test set","RMSE"],
  NNAR = accuracy(NNAR, auscafe)["Test set","RMSE"],
  TBATS = accuracy(TBATS, auscafe)["Test set","RMSE"],
  Combination =
          accuracy(Combination, auscafe)["Test set","RMSE"])

# 12.5 PREDICTION INTERVALS FOR AGGREGATES --------------------------------

# First fit a model to the data
fit <- ets(gas/1000)

# Forecast six months ahead

fc <- forecast(fit, h=6)

# Simulate 10000 future sample paths

nsim <- 10000

h <- 6

sim <- numeric(nsim)

for(i in seq_len(nsim))
        sim[i] <- sum(simulate(fit, future=TRUE, nsim=h))

meanagg <- mean(sim)

sum(fc[["mean"]][1:6])

meanagg

quantile(sim, prob=c(0.1, 0.9))

quantile(sim, prob=c(0.025, 0.975))

# 12.6 BACKCASTING --------------------------------------------------------

# Function to reverse time

reverse_ts <- function(y)
{
        ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}

# Function to reverse a forecast

reverse_forecast <- function(object)
{
        h <- length(object[["mean"]])
        f <- frequency(object[["mean"]])
        object[["x"]] <- reverse_ts(object[["x"]])
        object[["mean"]] <- ts(rev(object[["mean"]]),
                               end=tsp(object[["x"]])[1L]-1/f, frequency=f)
        object[["lower"]] <- object[["lower"]][h:1L,]
        object[["upper"]] <- object[["upper"]][h:1L,]
        return(object)
}

# Backcast example

euretail %>%
        reverse_ts() %>%
        auto.arima() %>%
        forecast() %>%
        reverse_forecast() -> bc

autoplot(bc) +
        ggtitle(paste("Backcasts from",bc[["method"]]))


# 12.8 FORECASTING ON TRAINING AND TEST -----------------------------------

training <- subset(auscafe, end=length(auscafe)-61)

test <- subset(auscafe, start=length(auscafe)-60)

cafe.train <- Arima(training, order=c(2,1,1),
                    seasonal=c(0,1,2), lambda=0)

cafe.train %>%
        forecast(h=60) %>%
        autoplot() + autolayer(test)

autoplot(training, series="Training data") +
        autolayer(fitted(cafe.train, h=12),
                  series="12-step fitted values")

