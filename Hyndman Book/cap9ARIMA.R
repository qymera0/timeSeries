library(tidyverse)
library(fable)
library(feasts)
library(tsibble)
library(tsibbledata)
library(fpp)
library(fpp2)
library(fpp3)

# 9.1 STATIONARITY AND DIFFERECING ----------------------------------------

google_stock <-
        gafa_stock %>% 
        filter(Symbol == "GOOG") %>% 
        mutate(day = row_number()) %>% 
        update_tsibble(index = day, regular = TRUE)

google_2015 <- google_stock %>%  filter(year(Date) == 2015) 

google_2015 %>%
        mutate(diff_close = difference(Close)) %>%
        features(diff_close, ljung_box, lag = 10)

PBS %>%
        filter(ATC2 == "H02") %>%
        summarise(Cost = sum(Cost)/1e6) %>%
        transmute(
                `Sales ($million)` = Cost,
                `Log sales` = log(Cost),
                `Annual change in log sales` = difference(log(Cost), 12),
                `Doubly differenced log sales` = difference(difference(log(Cost), 12), 1)
        ) %>%
        gather("Type", "Sales", !!!syms(measured_vars(.)), factor_key = TRUE) %>%
        ggplot(aes(x = Month, y = Sales)) +
        geom_line() +
        facet_grid(vars(Type), scales = "free_y") +
        labs(title = "Corticosteroid drug sales", x = "Year", y = NULL)

# Unit root tests

google_2015 %>% 
        features(Close, unitroot_kpss)

google_2015 %>%
        mutate(diff_close = difference(Close)) %>%
        features(diff_close, unitroot_kpss)

google_2015 %>%
        features(Close, unitroot_ndiffs)

aus_total_retail <- aus_retail %>%
        summarise(Turnover = sum(Turnover))

aus_total_retail %>%
        mutate(log_turnover = log(Turnover)) %>%
        features(log_turnover, unitroot_nsdiffs)


# 9.5 NON SEASONAL ARIMA --------------------------------------------------

us_change %>% 
        autoplot(Consumption)

fit <-
        us_change %>% 
        model(ARIMA(Consumption ~ PDQ(0,0,0)))

report(fit)

fit %>% forecast(h=10) %>% autoplot(slice(us_change, (n()-80):n()))

fit2 <- us_change %>%
        model(ARIMA(Consumption ~ pdq(3,0,0) + PDQ(0,0,0)))

report(fit2)

fit3 <- us_change %>%
        model(ARIMA(Consumption ~ PDQ(0,0,0),
                    stepwise = FALSE, approximation = FALSE))

report(fit3)

fit3 %>% forecast(h=10) %>% autoplot(slice(us_change, (n()-80):n()))

# 9.7 ARIMA MODELING IN R -------------------------------------------------


# 9.10 ARIMA VS ETS -------------------------------------------------------

aus_economy <- 
        global_economy %>% filter(Code == "AUS") %>%
        mutate(Population = Population/1e6)

aus_economy %>%
        slice(-n()) %>%
        stretch_tsibble(.init = 10) %>%
        model(
                ETS(Population),
                ARIMA(Population)
        ) %>%
        forecast(h = 1) %>%
        accuracy(aus_economy)

aus_economy %>%
        model(ETS(Population)) %>%
        forecast(h = "5 years") %>%
        autoplot(aus_economy)

# Seasonal

# Consider the cement data beginning in 1988

cement <- aus_production %>%
        filter(year(Quarter) >= 1988)

# Use 20 years of the data as the training set

train <- cement %>%
        filter(year(Quarter) <= 2007)

fit_arima <- train %>% model(ARIMA(Cement))

report(fit_arima)

gg_tsresiduals(fit_arima, lag_max = 16)

augment(fit_arima) %>%
        features(.resid, ljung_box, lag = 16, dof = 6)

fit_ets <- train %>% model(ETS(Cement))

report(fit_ets)

fit_ets %>% gg_tsresiduals(lag_max = 16)

augment(fit_ets) %>%
        features(.resid, ljung_box, lag = 16, dof = 6)

bind_rows(
        fit_arima %>% accuracy(),
        fit_ets %>% accuracy(),
        fit_arima %>% forecast(h = "2 years 6 months") %>%
                accuracy(cement),
        fit_ets %>% forecast(h = "2 years 6 months") %>%
                accuracy(cement)
)

cement %>% model(ARIMA(Cement)) %>% forecast(h="3 years") %>% autoplot(cement)
