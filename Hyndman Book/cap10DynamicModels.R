library(tidyverse)
library(fable)
library(feasts)
library(tsibble)
library(tsibbledata)
library(fpp)
library(fpp2)
library(fpp3)


# 10. REGRESSION WITH ARMA ERRORS -----------------------------------------

us_change %>%
        gather("var", "value", Consumption, Income) %>%
        ggplot(aes(x = Quarter, y = value)) +
        geom_line() +
        facet_grid(vars(var), scales = "free_y") +
        xlab("Year") + ylab(NULL) +
        ggtitle("Quarterly changes in US consumption and personal income")


fit <- us_change %>%
        model(ARIMA(Consumption ~ Income))

report(fit)

bind_rows(
        `Regression Errors` = as_tibble(residuals(fit, type="regression")),
        `ARIMA Errors` = as_tibble(residuals(fit, type="innovation")),
        .id = "type"
) %>%
        ggplot(aes(x = Quarter, y = .resid)) +
        geom_line() +
        facet_grid(vars(type), scales = "free_y") +
        xlab("Year") + ylab(NULL)

fit %>% gg_tsresiduals()

augment(fit) %>%
        features(.resid, ljung_box, dof = 5, lag = 8)

# 10.3 FORECASTING --------------------------------------------------------

us_change_future <- 
        new_data(us_change, 8) %>% 
        mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) %>%
        autoplot(us_change) + 
        xlab("Year") +
        ylab("Percentage change")

vic_elec_daily <- vic_elec %>%
        filter(year(Time) == 2014) %>%
        index_by(Date = date(Time)) %>%
        summarise(
                Demand = sum(Demand)/1e3,
                Temperature = max(Temperature),
                Holiday = any(Holiday)
        ) %>%
        mutate(Day_Type = case_when(
                Holiday ~ "Holiday",
                wday(Date) %in% 2:6 ~ "Weekday",
                TRUE ~ "Weekend"
        ))

vic_elec_daily %>%
        ggplot(aes(x=Temperature, y=Demand, colour=Day_Type)) +
        geom_point() +
        ylab("Electricity demand (GW)") +
        xlab("Maximum daily temperature")

fit <- vic_elec_daily %>%
        model(ARIMA(Demand ~ Temperature + I(Temperature^2) + (Day_Type=="Weekday")))

fit %>% gg_tsresiduals()

augment(fit) %>%
        features(.resid, ljung_box, dof = 8, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
        mutate(
                Temperature = 26,
                Holiday = c(TRUE, rep(FALSE, 13)),
                Day_Type = case_when(
                        Holiday ~ "Holiday",
                        wday(Date) %in% 2:6 ~ "Weekday",
                        TRUE ~ "Weekend"
                )
        )
forecast(fit, vic_elec_future) %>%
        autoplot(vic_elec_daily) + ylab("Electricity demand (GW)")
        


# 10.4 Sthocastic and deterministic trends --------------------------------

aus_visitors <- as_tsibble(fpp2::austa)

aus_visitors %>%
        autoplot(value) +
        labs(x = "Year", y = "millions of people",
             title = "Total annual international visitors to Australia")

# deterministic trend

fit_deterministic <- aus_visitors %>%
        model(ARIMA(value ~ trend() + pdq(d = 0)))

report(fit_deterministic)

# Stochastic

fit_stochastic <- aus_visitors %>%
        model(ARIMA(value ~ pdq(d=1)))

report(fit_stochastic)

bind_cols(fit_deterministic, fit_stochastic) %>%
        rename(`Deterministic trend` = 1, `Stochastic trend` = 2) %>%
        forecast(h = 10) %>%
        autoplot(aus_visitors) +
        labs(x = "Year", y = "Visitors to Australia (millions)",
             title = "Forecasts from trend models")

# 10.5 Dynamic harmonic regression ----------------------------------------

aus_cafe <- aus_retail %>%
        filter(
                Industry == "Cafes, restaurants and takeaway food services",
                year(Month) %in% 2004:2018
        ) %>%
        summarise(Turnover = sum(Turnover))

fit <- aus_cafe %>%
        model(
                `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0,0,0)),
                `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0,0,0)),
                `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0,0,0)),
                `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0,0,0)),
                `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0,0,0)),
                `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0,0,0))
        )

fit %>%
        forecast(h = "2 years") %>%
        autoplot(aus_cafe) +
        facet_wrap(vars(.model), ncol = 2) +
        guides(colour = FALSE) +
        geom_label(
                aes(x = yearmonth("2007 Jan"), y = 4250, label = paste0("AICc = ", format(AICc))),
                data = glance(fit)
        )


# 10.6 Lagged predictors --------------------------------------------------

insurance <- as_tsibble(fpp2::insurance, pivot_longer = FALSE)

insurance %>%
        gather("key", "value", Quotes, TV.advert) %>%
        ggplot(aes(x = index, y = value)) +
        geom_line() +
        facet_grid(vars(key), scales = "free_y") +
        labs(x = "Year", y = NULL,
             title = "Insurance advertising and quotations")

fit <- insurance %>%
        # Restrict data so models use same fitting period
        mutate(Quotes = c(NA,NA,NA,Quotes[4:40])) %>%
        # Estimate models
        model(
                lag0 = ARIMA(Quotes ~ pdq(d = 0) + TV.advert),
                lag1= ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert)),
                lag2= ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert) + lag(TV.advert, 2)),
                lag3= ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert) + lag(TV.advert, 2) + lag(TV.advert, 3))
        )

glance(fit)

# Fit with all data

fit_best <- insurance %>%
        model(ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert)))

report(fit_best)

# Forecast

insurance_future <- new_data(insurance, 20) %>%
        mutate(TV.advert = 8)

fit_best %>%
        forecast(insurance_future) %>%
        autoplot(insurance) + ylab("Quotes") +
        ggtitle("Forecast quotes with future advertising set to 8")
