library(readxl)
library(tidyverse)
library(forecast)
library(knitr)

travel_plans <- read_excel(path="assignment4/travel_plans.xlsx",
  sheet = "Vacation"
)

write.csv(travel_plans, "assignment4/travel_plans.csv")
travel_plans

data <- travel_plans %>%
  mutate(
    time = 1:n(),
    Q1 = as.numeric(Quarter == 1),
    Q2 = as.numeric(Quarter == 2),
    Q3 = as.numeric(Quarter == 3),
    Q4 = as.numeric(Quarter == 4)
  )

X11()

# Model 1: Seasonal dummies without trend
model1 <- lm(Vacation ~ Q1 + Q2 + Q3, data = data)

# Model 2: Seasonal dummies with trend
model2 <- lm(Vacation ~ time + Q1 + Q2 + Q3, data = data)

# Print model summaries
cat("\nModel 1: Seasonal Dummies Only\n")
print(summary(model1))

cat("\nModel 2: Seasonal Dummies with Trend\n")
print(summary(model2))

# Create comparison table
models_comparison <- data.frame(
  Model = c("Seasonal Only", "Seasonal with Trend"),
  R_squared = c(summary(model1)$r.squared, summary(model2)$r.squared),
  Adj_R_squared = c(summary(model1)$adj.r.squared, summary(model2)$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2))
)

# Print comparison table
print(kable(models_comparison, digits = 4))

# Plot actual vs fitted values
data$fitted1 <- fitted(model1)
data$fitted2 <- fitted(model2)

# Create visualization
ggplot(data, aes(x = time)) +
  geom_point(aes(y = Vacation), color = "black") +
  geom_line(aes(y = fitted1, color = "Model 1")) +
  geom_line(aes(y = fitted2, color = "Model 2")) +
  scale_color_manual(values = c("Model 1" = "blue", "Model 2" = "red")) +
  labs(
    title = "Actual vs Fitted Values",
    x = "Time",
    y = "Vacation Packages",
    color = "Models"
  ) +
  theme_minimal()

# Generate forecasts for 2020 Q1 and Q2
forecast_data <- data.frame(
  time = c(49, 50), # Time indices for 2020 Q1 and Q2
  Q1 = c(1, 0),
  Q2 = c(0, 1),
  Q3 = c(0, 0),
  Q4 = c(0, 0)
)

# Generate predictions from both models
pred1 <- predict(model1, newdata = forecast_data, interval = "prediction")
pred2 <- predict(model2, newdata = forecast_data, interval = "prediction")

# Create forecasts table
forecasts <- data.frame(
  Quarter = c("2020 Q1", "2020 Q2"),
  Model1_Forecast = pred1[, "fit"],
  Model2_Forecast = pred2[, "fit"]
)


# Print forecasts
print(kable(forecasts, digits = 0))
dev.hold()
Sys.sleep(10)