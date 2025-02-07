# Time Series Forecasting Tutorial
# This script demonstrates various forecasting techniques
# from basic to advanced approaches

# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(smooth)
library(ModelMetrics)
library(zoo)

X11()

# ===========================================
# 1. Generate Sample Data for Demonstration
# ===========================================
# Creating synthetic data with trend, seasonality, and noise
set.seed(123)
time_points <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "month")
n <- length(time_points)

# Generate components
trend <- seq(100, 100 + n - 1)
seasonality <- 20 * sin(2 * pi * seq_along(time_points) / 12)
noise <- rnorm(n, 0, 5)

# Combine components
y <- trend + seasonality + noise

# Create time series object
ts_data <- ts(y, frequency = 12, start = c(2018, 1))

# ===========================================
# 2. Simple Moving Average
# ===========================================
# Calculate 3-month and 6-month moving averages
ma3 <- rollmean(y, k = 3, fill = NA)
ma6 <- rollmean(y, k = 6, fill = NA)

# Plot original data with moving averages
plot(time_points, y,
    type = "l", col = "black",
    main = "Moving Average Smoothing",
    xlab = "Time", ylab = "Value"
)
lines(time_points, ma3, col = "blue")
lines(time_points, ma6, col = "red")
legend("topleft", c("Original", "3-month MA", "6-month MA"),
    col = c("black", "blue", "red"), lty = 1
)

# ===========================================
# 3. Exponential Smoothing
# ===========================================
# Simple Exponential Smoothing
ses_model <- ses(ts_data, h = 12) # Forecast 12 periods ahead

# Holt's Method (Double Exponential Smoothing)
holt_model <- holt(ts_data, h = 12)

# Holt-Winters Method (Triple Exponential Smoothing)
hw_model <- hw(ts_data, seasonal = "multiplicative", h = 12)

# ===========================================
# 4. ARIMA Models
# ===========================================
# Check stationarity
adf_test <- adf.test(ts_data)
print(paste("ADF test p-value:", adf_test$p.value))

# Auto ARIMA
auto_arima <- auto.arima(ts_data)
print(summary(auto_arima))

# Forecast with ARIMA
arima_forecast <- forecast(auto_arima, h = 12)

# ===========================================
# 5. Cross-Validation for Time Series
# ===========================================
# Function for time series cross-validation
ts_cv <- function(data, model_func, h = 1, window = 36) {
    n <- length(data)
    errors <- numeric()

    for (i in seq(window, n - h)) {
        # Training data
        train <- subset(data, start = i - window + 1, end = i)

        # Test data
        test <- subset(data, start = i + 1, end = i + h)

        # Fit model and make prediction
        model <- model_func(train)
        pred <- forecast(model, h = h)

        # Calculate error
        error <- test - pred$mean[1:h]
        errors <- c(errors, error)
    }

    return(errors)
}

# Apply cross-validation to different models
# Simple Exponential Smoothing
ses_errors <- ts_cv(ts_data, ses)
print(paste("SES RMSE:", sqrt(mean(ses_errors^2))))

# Holt-Winters
hw_errors <- ts_cv(ts_data, hw)
print(paste("Holt-Winters RMSE:", sqrt(mean(hw_errors^2))))

# ===========================================
# 6. Advanced Smoothing: TBATS
# ===========================================
# Fit TBATS model
tbats_model <- tbats(ts_data)
tbats_forecast <- forecast(tbats_model, h = 12)

# ===========================================
# 7. Regression with ARIMA Errors
# ===========================================
# Create external regressors (example: time index and seasonal dummies)
time_index <- 1:length(ts_data)
month_dummies <- seasonaldummy(ts_data)

# Fit regression with ARIMA errors
reg_arima <- auto.arima(ts_data, xreg = cbind(time_index, month_dummies))
reg_forecast <- forecast(reg_arima,
    xreg = cbind(
        (length(ts_data) + 1):(length(ts_data) + 12),
        seasonaldummy(ts_data, h = 12)
    )
)

# ===========================================
# 8. Comparing Forecasts
# ===========================================
# Create comparison plot
plot(hw_model, main = "Forecast Comparison")
lines(tbats_forecast$mean, col = "blue")
lines(reg_forecast$mean, col = "red")
legend("topleft",
    c("Holt-Winters", "TBATS", "Regression-ARIMA"),
    col = c("black", "blue", "red"),
    lty = 1
)

# Calculate accuracy measures for all models
accuracy_measures <- rbind(
    SES = accuracy(ses_model),
    "Holt-Winters" = accuracy(hw_model),
    TBATS = accuracy(tbats_model),
    "Reg-ARIMA" = accuracy(reg_forecast)
)
print(accuracy_measures)