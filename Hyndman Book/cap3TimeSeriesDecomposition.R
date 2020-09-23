library(tidyverse)
library(fpp)
library(fpp2)

# 6.2 MOVING AVERAGE ------------------------------------------------------

autoplot(elecsales) + xlab("Year") + ylab("GWh") +
        ggtitle("Annual electricity sales: South Australia")


ma(elecsales, 5)

autoplot(elecsales, series="Data") +
        autolayer(ma(elecsales,5), series="5-MA") +
        xlab("Year") + ylab("GWh") +
        ggtitle("Annual electricity sales: South Australia") +
        scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                            breaks=c("Data","5-MA"))

# Moving averages of moving averages

beer2 <- window(ausbeer,start=1992)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)

# Estimate the trend-cycle with seasonal data

autoplot(elecequip, series="Data") +
        autolayer(ma(elecequip, 12), series="12-MA") +
        xlab("Year") + ylab("New orders index") +
        ggtitle("Electrical equipment manufacturing (Euro area)") +
        scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                            breaks=c("Data","12-MA"))

# 6.3 CLASSICAL DECOMPOSITION ---------------------------------------------

elecequip %>% 
        decompose(type="multiplicative") %>%
        autoplot() + xlab("Year") +
        ggtitle("Classical multiplicative decomposition
    of electrical equipment index")


# 6.4 X11 DECOMPOSITION ---------------------------------------------------

library(seasonal)

elecequip %>% seas(x11="") -> fit

autoplot(fit) +
        ggtitle("X11 decomposition of electrical equipment index")

autoplot(elecequip, series="Data") +
    autolayer(trendcycle(fit), series="Trend") +
    autolayer(seasadj(fit), series="Seasonally Adjusted") +
    xlab("Year") + ylab("New orders index") +
    ggtitle("Electrical equipment manufacturing (Euro area)") +
    scale_colour_manual(values=c("gray","blue","red"),
                        breaks=c("Data","Seasonally Adjusted","Trend"))

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

# 6.5 SEATS ---------------------------------------------------------------

elecequip %>% 
    seas() %>%
    autoplot() +
    ggtitle("SEATS decomposition of electrical equipment index")


# 6.6 STL -----------------------------------------------------------------

elecequip %>%
    stl(t.window=13, s.window="periodic", robust=TRUE) %>%
    autoplot()


# 6.7 STRENGHT OF TREND AND SEASONALITY -----------------------------------

t <- elecequip %>%
    stl(t.window=13, s.window="periodic", robust=TRUE)


