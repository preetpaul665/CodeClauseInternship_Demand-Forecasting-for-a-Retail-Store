# Codeclause Internship - Task II
## Project Title - Demand Forecasting for a Retail Store

## Loading the dataset

sales <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\Codeclause\\sales data-set.csv")
dim(sales)   # dimension of the dataset
View(sales)
str(sales)
summary(sales)   # summary of the dataset

## Checking for missing values

sum(is.na(sales))
which(is.na(sales))

## Working with the data of a particular department of a particular store

sales1 <- sales[1:143,]
View(sales1)

## Creating the time-series dataset

data <- ts(sales1$Weekly_Sales,frequency = 52, start = c(2010,2))
data

## Loading Required Packages

library(TSstudio)
library(forecast)

# Plotting the time-series data

ts_plot(data, Ytitle = "Weekly Sales", 
        title = "Time-series Plot of Weekly Sales of a Retail Store")


## Seasonality Analysis

ts_seasonal(data, type = "normal")

## Heatmap Plot

ts_heatmap(data)


# Correlation Analysis

## Visualizing ACF and PACF Plots

ts_cor(data, type = "both")

## Lags Plot

ts_lags(data, lags = 1:12)


# Training Forecasting Models


## Forecasting Applications
## Setting Training and Testing Partitions

library(caret)
set.seed(19)

index <- createDataPartition(data, p = (2/3), list = FALSE)
Train <- data[index]
Test <- data[-index]
length(Train)   # length of the training dataset
length(Test)    # length of the testing dataset

## Forecasting with auto.arima

md <- auto.arima(Train,seasonal = TRUE)
md
fc <- forecast(md, h=47)
fc

## Plotting actual vs. fitted and forecasted

test_forecast(actual = data, forecast.obj = fc, test = Test)

## Plotting the Forecast

plot_forecast(fc)

## Forecasting with ets

fit1 <- ets(Train, model = "ZZZ")
fit1
fc1 <- forecast(fit1, h=47)
fc1

## Plotting actual vs. fitted and forecasted

test_forecast(actual = data, forecast.obj = fc1, test = Test)
plot(forecast(fit1))

## Forecasting with tslm

fit2 <- tslm(data ~ trend + season)
fit2

## Plotting actual vs. fitted and forecasted

plot(forecast(fit2))

## Forecasting with Holt-Winters method

fit3 <- HoltWinters(data, seasonal = "additive")
fit3

## Plotting actual vs. fitted and forecasted

plot(forecast(fit3))



