# MINI PROJECT: FORECASTING INDIA'S INFLATION GAP

library(readxl)

data <- read_excel("C:/Users/Tanishq/OneDrive/Desktop/time series/Inflation dataset OECD.xlsx")#Import the data set (combined from FRED and MOSPI)

head(data)

data <- data[complete.cases(data), ] #Cleaning the data set
sum(is.na(data))

data$inflationgap <- data$India - 4 #Constructing the inflation gap using India's target inflation rate of 4%(+/-2)

head(data$inflationgap)

plot(data$inflationgap, type = "l", col = "mediumpurple2", lwd = 2,
     main = "Inflation Gap", xlab = "Date", ylab = "Inflation Gap") #Plot the inflation gap to check for visual cues related to trends, seasonality etc.

inflationgap_ts <- ts(data$inflationgap, start = c(2005, 1), frequency = 12)

plot(inflationgap_ts,
     main = "Inflation Gap",
     xlab = "Year",
     ylab = "Inflation Gap",
     col = "mediumpurple2",
     lwd = 3)

acf(data$India, lag.max = 24) #To check for seasonal patterns

library(seastests)

inflationgap_ts <- ts(data$inflationgap,
                      frequency = 12)

isSeasonal(inflationgap_ts,
           test = "combined",
           freq = 12)

library (tseries)

adf.test (data$inflationgap) #Checking for stationarity
kpss.test(data$inflationgap)

par(mfrow = c(2, 2))
acf(data$inflationgap, lag.max = 24) #To identify possible MA order
pacf(data$inflationgap, lag.max = 24) #To identify possible AR order

library(TSA)
eacf (data$inflationgap) #Since the ARMA order could not be discerned using ACF/PACF we use EACF to identify potentical candidates for ARMA

fit_arma23 <- arima(data$inflationgap, order = c(2,0,3), method = "ML") #Lower AIC indicates better fit
fit_arma23
fit_arma32 <- arima(data$inflationgap, order = c(3,0,2), method = "ML")
fit_arma32
fit_arma13 <- arima(data$inflationgap, order = c(1,0,3), method = "ML")
fit_arma13

BIC(fit_arma23) #Lower BIC indicates better fit
BIC(fit_arma32)
BIC(fit_arma13)

tsdiag(fit_arma23) #We use this for residual diagnostics
Box.test(residuals(fit_arma23), lag = 10, type = "Ljung-Box") #Ljung-Box test for residual autocorrelation; H0: residuals are white noise
tsdiag(fit_arma32)
Box.test(residuals(fit_arma32), lag = 10, type = "Ljung-Box")
tsdiag(fit_arma13)
Box.test(residuals(fit_arma13), lag = 10, type = "Ljung-Box")

length(data$inflationgap) # Check the total number of observations
h <- 12 # Set the forecasting horizon equal to 12 months
n <- length(data$inflationgap)
n_train <- n - h 

train   <- window(data$inflationgap, end = n_train) #Create the training sample using the first 243 observations
holdout <- window(data$inflationgap, start = n_train + 1) #Create the holdout sample using the final 12 observations


length(train) #Cross checking number of obs.
length(holdout)

fit_train_13 <- arima(train, order = c(1,0,3), method = "ML") #Estimate ARMA(1,3) using only the training data
fc_13 <- predict(fit_train_13, n.ahead = h) #Forecasting next 12 obs.

fit_train_23 <- arima(train, order = c(2,0,3), method = "ML")
fc_23 <- predict(fit_train_23, n.ahead = h)

fit_train_32 <- arima(train, order = c(3,0,2), method = "ML")
fc_32 <- predict(fit_train_32, n.ahead = h)

fc_23_pred <- fc_23$pred #Extract the 12 predicted inflation-gap values
se_23 <- fc_23$se #Extract the standard errors of the 12 forecasts

lower_95_23 <- fc_23_pred - 1.96 * se_23
upper_95_23 <- fc_23_pred + 1.96 * se_23

ts.plot(holdout,
        fc_23_pred,
        lower_95_23,
        upper_95_23,
        col = c("black", "lightblue", "magenta4", "magenta4"),
        lty = c(1, 1, 2, 2),
        lwd = c(2.2, 2.2, 1.5, 1.5),
        main = "ARMA(2,3) 12-Month Out-of-Sample Forecast vs Actual",
        ylab = "Inflation Gap",
        xlab = "Holdout Sample")

legend("topleft",
       legend = c("Actual",
                  "ARMA(2,3) Forecast",
                  "95% Lower Bound",
                  "95% Upper Bound"),
       col = c("black", "lightblue", "magenta4", "magenta4"),
       lty = c(1, 1, 2, 2),
       lwd = c(2.2, 2.2, 1.5, 1.5),
       bty = "n",
       cex = 0.85)

mae  <- function(actual, forecast) mean(abs(actual - forecast)) #Defining MAE
rmse <- function(actual, forecast) sqrt(mean((actual - forecast)^2)) #Defining RMSE

mae_13 <- mae(holdout, fc_13$pred) #Calculate MAE and RMSE for ARMA(1,3)
rmse_13 <- rmse(holdout, fc_13$pred)

mae_23 <- mae(holdout, fc_23$pred)
rmse_23 <- rmse(holdout, fc_23$pred)

mae_32 <- mae(holdout, fc_32$pred)
rmse_32 <- rmse(holdout, fc_32$pred)

eval_table <- data.frame(
  candidate_model = c("ARMA(1,3)","ARMA(2,3)", "ARMA(3,2)"),
  AIC = c(AIC(fit_arma13),
          AIC(fit_arma23),
          AIC(fit_arma32)),
  MAE = c(mae_13, mae_23, mae_32),
  RMSE = c(rmse_13, rmse_23, rmse_32))

eval_table$best_RMSE <- eval_table$RMSE == min(eval_table$RMSE)

print(eval_table) #Comparing the three models across AIC MAE and RMSE

cat("\nBest in-sample fit (lowest AIC): ",
    eval_table$candidate_model[which.min(eval_table$AIC)], "\n")

cat("Best out-of-sample forecast (lowest RMSE): ",
    eval_table$candidate_model[which.min(eval_table$RMSE)], "\n")

final_model <- arima(data$inflationgap, order = c(2,0,3), method = "ML")

pred_live <- predict(final_model, n.ahead = 3) #Forecasting the next three months

fc_live <- pred_live$pred
se_live <- pred_live$se

lower_95 <- fc_live - 1.96 * se_live
upper_95 <- fc_live + 1.96 * se_live

final_forecast <- data.frame(
  Month = c("09/26", "10/26", "11/26"),
  Forecast = fc_live,
  Lower_95 = lower_95,
  Upper_95 = upper_95
)

print(final_forecast) #Inflation gap forecasts

final_forecast_inflation <- final_forecast

final_forecast_inflation$Forecast <- final_forecast$Forecast + 4 #Converting inflation gap forecast to inflation forecast
final_forecast_inflation$Lower_95 <- final_forecast$Lower_95 + 4
final_forecast_inflation$Upper_95 <- final_forecast$Upper_95 + 4

print(final_forecast_inflation)

long_run_mean <- final_model$coef["intercept"]

long_run_mean