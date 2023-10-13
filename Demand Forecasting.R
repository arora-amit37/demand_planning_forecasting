#
# DEMAND FORECASTING
# Coursera Guided Project
# Instructor - Moses Gummadi

# This is an R Script file
# You can open this in R Studio
# Use # symbol for comments in R Script file
# The script file contains code and comments
# You can run the code by typing it next to ">" symbol
#   in the Console window (bottom left)
# You can also run the code by selecting the code in
#   the script file and hit "Run" in the top-left pane

# R Studio is already downloaded in your Rhyme Desktop
# To download on your own desktop
# Use this link https://rstudio.com/products/rstudio/
# Choose the RStudio Desktop Free Option

#----------------------------------------------------------
# Tasks In This Guided Project
#    Import data, create time series objects.
#    Analyse trend, seasonality.
#    Analyse trend, seasonality - Practice Task
#    Auto ARIMA modelling & parameters
#    Create and evaluate ARIMA demand model
#    Forecast demand, plot, extact and evaluate.
#    Capstone Task

# ---------------------------------------------------------
# Task 1 - Import data, create time series objects
# ---------------------------------------------------------

# Code To Check & Install Libraries
check = c("forecast","data.table","ggplot2") %in% installed.packages()
if(!check[1]) install.packages("forecast")
if(!check[2]) install.packages("data.table")
if(!check[3]) install.packages("ggplot2")
library(forecast); library(data.table); library(ggplot2)
rm(check)

setwd("C:/Users/niteshg/Downloads/demand forecasting")

# Import Demand Data
dt = fread("Demand-Data.csv")

# Format Date, add additional columns
dt$Date = as.Date(dt$Date, "yyyy-mm-dd")
dt[, WeekDay := wday(Date) ]
dt[, WeekNum := week(Date) ]
dt[, Month   := month(Date)]
dt[, Year    := year(Date) ]

# Plot demand data for a chosen product
ggplot(data = dt, aes(x = Date, y = Paint)) +
  geom_line(color = "blue")+
  xlab("Year") +
  ylab("Daily Demand for Paint")

# Summarise demand by week and by month
cnames = names(dt)[2:7]

dtw = dt[, lapply(.SD, sum, na.rm=TRUE), 
         by = .(WeekNum,Year), .SDcols=cnames ]

dtm = dt[, lapply(.SD, sum, na.rm=TRUE), 
         by = .(Month,Year), .SDcols=cnames ]

# Filtering Data Tables
dtw[Year == 2020]
dtm[Year >= 2019]

# Extracting Columns as Vectors
dtm[, Paint]
dtm[Year >= 2020, Paint]

#------Time Series Objects (Examples) ---------------

x = sample(10:20,100, replace = TRUE)

ts(x, frequency = 12, start = c(2014,2))

ts(x, frequency = 4, start = c(2014,3))

ts(x, frequency = 4, 
   start = c(2014,3), end = c(2020,4))


#-----Create Time Series Objects From Demand Data ----

ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))

autoplot(ts_dtm) + 
  xlab("Year") +
  ylab("Monthly Demand for Paint")

ts_dtw = ts(dtw[,GardEquip], frequency = 52, start = c(2018,1))

autoplot(ts_dtw) + 
  xlab("Year") +
  ylab("Weekly Demand for Garden Equipment")

# Check If It's Time Series Object
is.ts(dt)  
is.ts(ts_dtw)


# ---------------------------------------------------------
# Task 2 - Analyse demand trend & seasonality 
# ---------------------------------------------------------

ts_dtw = ts(dtw[,Compost], frequency =52, start = c(2018,1))
autoplot(ts_dtw)

ts_dtm = ts(dtm[,DoorLock], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

# ------ACF and PACF --------------------------------------
#   
#   ACF  = Auto-correlation Function 
#   PACF = Partial Auto-correlation Function 

Acf(ts_dtm)
Pacf(ts_dtm)


# ------Decompose Data ------------------------------------

ts_dtm  %>%
  decompose() %>%
  autoplot()

ts_dtw  %>%
  decompose() %>%
  autoplot()


# ------Extract Decomposed Data ---------------------------

decompose(ts_dtm)$trend
decompose(ts_dtm)$seasonal
decompose(ts_dtm)$random

autoplot(ts_dtm - decompose(ts_dtm)$seasonal)
autoplot(ts_dtm - decompose(ts_dtm)$trend)

Pacf(ts_dtm - decompose(ts_dtm)$seasonal)


# ---------------------------------------------------------
# Practice - Analyse demand trend & seasonality 
# ---------------------------------------------------------

# Analyse QUARTERLY trend and seasonality for "GardEquip"
# Use demand from 2019 onwards, exclude 2018 data
# You will need a new table, "dtq" and new time series object
# DO NOT use "ts_dtw" and "ts_dtm" to define time series objects
# Use the function "quarter" to create additional column in dt





# ---------------------------------------------------------
# Task 3 - Auto ARIMA modelling & parameters
# ---------------------------------------------------------

# ------What is ARIMA ? -----------------------------------
# AR     = Auto Regression - Regression on past values
# MA     = Moving Average model - Regression on past "errors"
# ARIMA  = Auto Regressive Integrated Moving Average

# AR - regression model - current values are 
#      linearly dependent on past values

# MR - regression model - current values are 
#      linearly dependent on past "errors" 

# Resource - https://otexts.com/fpp2/seasonal-arima.html


auto.arima(ts_dtm)

# ARIMA (p,d,q) (P,D,Q) [freq]
# (p,d,q) - non-seasonal part
# (P,D,Q) - seasonal part

# p,P - order (number of time lags) of the autoregressive model
# d,P - degree of first differencing 
# q,P - order of the moving-average model

# AIC = Akaike Information Criterion
# BIC = Bayesian Information Criterion

# AIC and BIC estimate prediction error
#     and quantity the information loss
#     the lesser the better model we have

# List the models using 'trace = TRUE'
auto.arima(ts_dtm, ic = "bic", trace = TRUE)

# "stepwise = FALSE" searches more models. It's slow.
auto.arima(ts_dtm, stepwise = FALSE, trace = TRUE)

# The model can be stored in a variable
m = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(m)

# ---------------------------------------------------------
# Task 4 - Create and evaluate ARIMA demand model
# ---------------------------------------------------------

ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

m = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(m)

auto.arima(ts_dtm, stepwise = FALSE, trace = TRUE)

m = Arima(ts_dtm, order = c(0,0,0),
          seasonal=list(order=c(0,1,1), period=12),
          include.drift = TRUE)
print(m)

Pacf(ts_dtm)

summary(m)

checkresiduals(m) # residuals normal, ACF matches periodicity



# ---------------------------------------------------------
# Task 5 - Forecast demand, plot, extact and evaluate.
# ---------------------------------------------------------

f = forecast(m, h = 12) # h = number of periods forecasted 
autoplot(f)

autoplot(f, include = 24) # previous data points to include

#----- Forecast Diagnosis  --------------------------------

plot(f$residuals)

qqnorm(f$residuals)
qqline(f$residuals, col = 2)

Acf(f$residuals)
Pacf(f$residuals)

#----- Write Output  -------------------------------------

out = as.data.table(f)
out = round(out,0)       # round the values to integers
write.csv(out,"Forecast.csv")

# ---------------------------------------------------------
# Capstone Task - 52 Week Forecast for Garden Equipment
# ---------------------------------------------------------



# ---------------------------------------------------------
# End of Guided Project - Thank You
# ---------------------------------------------------------