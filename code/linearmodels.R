library(lubridate)
library(tidyverse)

# ERCOT
# Power grid load every hour for 6 1/2 years
# throughout the 8 ERCOT regions of Texas
# units of grid load are megawatts.
# This represents peak instantaneous demand for power in that hour.
# source: scraped from the ERCOT website
load_ercot = read.csv("../data/load_ercot.csv")

# Now weather data at the KHOU weather station
# temps in F
load_temperature = read.csv("../data/load_temperature.csv")

# Merge the two data sets on their common field: Time
# Now we'll have access to the temperature data to predict power consumption
load_combined = merge(load_ercot, load_temperature, by = 'Time')
head(load_combined)
plot(load_combined$KHOU,load_combined$COAST,col=rgb(0,0,0,alpha=0.1),pch=19,cex=0.8)

# first things first: let's get the Time variable into a format R understands.
# Right now R thinks it's just a string of characters.
# We need to tell R it's actually a time stamp in a specific format: Y-M-D H:M:S
# We'll do this with the ymd_hms function in the lubridate package
load_combined = mutate(load_combined,
                       Time = ymd_hms(Time))
plot(load_combined$Time,load_combined$COAST,type='l',col=rgb(0,0,0,alpha=0.6))
plot(load_combined$KHOU,load_combined$COAST,col=rgb(0,0,0,alpha=0.1),pch=19,cex=0.8)


# Simple linear regression
lm1 = lm(COAST ~ KHOU, data=load_combined)
summary(lm1)

plot(load_combined$KHOU,load_combined$COAST,col=rgb(0,0,0,alpha=0.1),pch=19,cex=0.8)
abline(lm1,lty=1,lwd=4,col='blue')
autoplot(lm1)

# What about an additional variable?
K2 = load_combined$KHOU^2
load_combined$K2 = K2
lm2 = lm(COAST ~ KHOU + K2, data=load_combined)
summary(lm2)
autoplot(lm2)

plot(load_combined$KHOU,load_combined$COAST,col=rgb(0,0,0,alpha=0.1),pch=19,cex=0.8)

x = load_combined$KHOU
xmin = floor(min(x))
xmax = floor(max(x))
xnew = seq(xmin,xmax,length.out=500)

quadfit = lm2$coefficients[1] + lm2$coefficients[2]*xnew + lm2$coefficients[3]*xnew^2
lines(xnew,quadfit,col=2,lwd=3)

# What about a crazy model?
# What about an additional variable?
K3 = load_combined$KHOU^3
K4 = load_combined$KHOU^4
K5 = load_combined$KHOU^5
K6 = load_combined$KHOU^6
load_combined$K3 = K3
load_combined$K4 = K4
load_combined$K5 = K5
load_combined$K6 = K6

lm3 = lm(COAST ~ KHOU + K2 + K3 + K4 + K5 + K6, data=load_combined)

x = load_combined$KHOU
xmin = floor(min(x))
xmax = floor(max(x))
xnew = seq(xmin,xmax,length.out=500)

crazyfit = lm3$coefficients[1] + lm3$coefficients[2]*xnew + lm3$coefficients[3]*xnew^2 + lm3$coefficients[4]*xnew^3 + lm3$coefficients[5]*xnew^4 + lm3$coefficients[6]*xnew^5 + lm3$coefficients[7]*xnew^6

lines(xnew,crazyfit,col=4,lwd=3)
