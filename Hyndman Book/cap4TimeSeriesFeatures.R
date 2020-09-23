library(tidyverse)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)

# 4.1 SIMPLE STATISTICS ---------------------------------------------------

tourism %>% features(Trips, mean)

tourism %>% features(Trips, list(mean = mean)) %>% arrange(mean)

tourism %>%  features(Trips, quantile, prob = seq(0, 1, by = 0.25))

# 4.2 ACF -----------------------------------------------------------------

tourism %>% features(Trips, feat_acf)

# STL FEATURES ------------------------------------------------------------

tourism %>% features(Trips, feat_stl)

t <- tourism

tourism %>%
        features(Trips, feat_stl) %>%
        ggplot(aes(x=trend_strength, y=seasonal_strength_year, col=Purpose)) +
        geom_point() + facet_wrap(vars(State))

tourism %>%
        features(Trips, feat_stl) %>%
        filter(seasonal_strength_year == max(seasonal_strength_year)) %>%
        left_join(tourism, by = c("State","Region","Purpose")) %>%
        ggplot(aes(x = Quarter, y = Trips)) + geom_line() +
        facet_grid(vars(State,Region,Purpose))


# Explore Australia tourism data ----------------------------------------------------------

tourism_features <- 
        tourism %>%
        features(Trips, feature_set(pkgs="feasts"))

# Seasonality explore

tourism_features %>%
        select_at(vars(contains("season"), Purpose)) %>%
        mutate(
                seasonal_peak_year = glue::glue("Q{seasonal_peak_year+1}"),
                seasonal_trough_year = glue::glue("Q{seasonal_trough_year+1}"),
        ) %>%
        GGally::ggpairs(mapping = aes(colour=Purpose))

library(broom)

pcs <-
        tourism_features %>% 
        select(-State, -Region, -Purpose) %>% 
        prcomp(scale = TRUE) %>% 
        augment(tourism_features)

pcs %>% 
        ggplot(aes(x=.fittedPC1, y=.fittedPC2, col=Purpose)) +
        geom_point() + theme(aspect.ratio=1) 

outliers <-
        pcs %>% 
        filter(.fittedPC1 > 10.5) %>% 
        select(Region, 
               State, 
               Purpose, 
               .fittedPC1, 
               .fittedPC2)

outliers

outliers %>%
        left_join(tourism, by = c("State", "Region", "Purpose")) %>%
        mutate(Series = glue::glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) %>%
        ggplot(aes(x = Quarter, y = Trips)) +
        geom_line() +
        facet_grid(Series ~ ., scales='free') +
        ggtitle("Outlying time series in PC space")
