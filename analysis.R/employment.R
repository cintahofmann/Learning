#libraries
library(tidyr)
library(lubridate)
library(Quandl)
library(xts)
library(ggplot2)

setwd("C:/Users/cintacolix/Documents/GitHub/Learning/data")

unemployment <- Quandl("FRED/SEAT653URN", api_key="DKj29j9kxjAiq-FmRrVE", 
                       start_date="2006-01-01", end_date="2015-12-31")
unemploymentx <- xts(unemployment$VALU, order.by = unemployment$DATE)
unemployment_yearly <- apply.yearly(unemploymentx, mean)

precipitation <- c(48.42, 38.95, 30.73, 38.44, 46.99, 36.39, 48.26,
                   32.56, 48.5, 44.83)
unemployment_yearly <- cbind(unemployment_yearly, precipitation)
colnames(unemployment_yearly) <- c("Unemployment", "Precipitation")
unemployment_yearly
bluh <- lm(data=unemployment_yearly, Unemployment ~ Precipitation)
summary(bluh)
plot(bluh)

bleh <- lm(data=unemployment_yearly, Precipitation ~ Unemployment)
summary(bleh)

temperature <- c(53.2, 52, 51.4, 52.2, 52.7, 51, 52.3, 53.8, 55, 55.6)
unemployment_yearly <- cbind(unemployment_yearly, temperature)
colnames(unemployment_yearly) <- c("Unemployment", "Precipitation",
                                   "Temperature")
a <- lm(data=unemployment_yearly, Unemployment ~ Precipitation +
          Temperature)
summary(a)
plot(a)
resid(a)
plot(resid(a))

p <- ggplot(unemployment_yearly, aes(x=Precipitation, y=Unemployment))
p + geom_point() + stat_density2d()
q <- p + stat_density2d(aes(colour=..level..))
q

a <- p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)
b <- p + geom_point() + stat_density2d(aes(alpha=..density..), geom="tile", contour=FALSE)


#saving the picture
ggsave("plot.png")