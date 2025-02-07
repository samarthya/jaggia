# Time Series

Think of time series like a story that numbers tell us over time. Just as a story has different elements that make it interesting, time series data has four main components that help us understand what's happening.

## Trend: The long-term movement in the data

Imagine you're tracking the number of steps you walk each day. The trend is like the general direction your walking habit is heading over months or years. Are you gradually becoming more active (upward trend) or less active (downward trend)?

### Real-world Applications

- Housing prices in a city (often showing upward trend over years)
- Global temperature changes (showing warming trend over decades)
- A company's revenue growth over time

## Seasonality: Regular patterns that repeat at fixed intervals

It  is like nature's rhythm - patterns that repeat at regular intervals. Just as ice cream sales go up every summer and down every winter, many business activities follow predictable seasonal patterns.

### Business Example

A retail store's sales typically spike during holidays like Christmas and dip in January. Understanding this pattern helps them:

- Plan inventory
- Schedule staff
- Manage cash flow
- Set realistic monthly targets

### Real-world Applications

- Hotel bookings (higher in summer, lower in winter)
- Coffee shop sales (morning rush, afternoon lull)
- Electricity usage (peaks during dinner time)

## Cyclical: Longer-term patterns that aren't of fixed length

Think of this like longer waves that aren't as regular as seasons. It's similar to how the fashion industry sees certain styles come back every few years, but not at exact intervals.

### Business Example

The automotive industry experiences cycles that last several years. During good economic times, people buy more new cars. During recessions, they keep their old cars longer. These cycles aren't as predictable as seasonal patterns but are important for long-term planning.

### Real-world Applications

- Housing market cycles (boom and bust periods)
- Commodity prices (like oil or gold)
- Employment rates during economic cycles

## Random: Unexplained variation or noise

This is like the unexpected events that happen in life. Maybe you planned to walk 10,000 steps today, but it rained unexpectedly. These random events make predictions challenging but are a natural part of any time series.

### Business Example

A restaurant might see unexpected changes in daily sales due to:

- Sudden weather changes
- A nearby road closure
- A celebrity visit
- A viral social media post


## Why Time Series Analysis is Crucial


### Business Planning

Imagine you're running a coffee shop. Time series analysis helps you:

1. Know how many baristas to schedule (seasonal pattern - busier mornings)
2. Plan when to buy new equipment (trend - growing customer base)
3. Prepare for economic downturns (cyclical pattern)
4. Have backup plans for unexpected events (random component)

### Public Health

During the COVID-19 pandemic, time series analysis helped:

1. Track infection rates (trend)
2. Understand seasonal variations in virus spread
3. Plan hospital capacity
4. Predict future waves

### Personal Finance

Even individual investors use time series patterns to:

1. Understand stock market cycles
2. Plan retirement savings
3. Budget for seasonal expenses
4. Prepare for economic downturns

### Environmental Planning

Cities use time series analysis for:

1. Water usage patterns (seasonal)
2. Long-term climate changes (trend)
3. Pollution levels (daily and seasonal patterns)
4. Natural disaster preparedness

## Smoothening techniques

### Moving Averages

The simplest form of smoothing is the moving average, where we take the mean of a sliding window of observations. This helps reduce noise and reveal underlying patterns. The code shows both 3-month and 6-month moving averages. The wider the window, the smoother the result but also the more lag introduced.

### Exponential Smoothing

This family of methods gives more weight to recent observations:

### Simple Exponential Smoothing (SES): For data with no clear trend or seasonality

Holt's Method: Adds trend component
Holt-Winters: Adds both trend and seasonal components


## Linear Regression Models

Regression is a statistical method that helps us understand and quantify relationships between variables. Think of it as drawing a line (or curve) through data points that best represents their pattern. This line then allows us to make predictions about new data.

1. Simple linear regression model
2. Multiple linear regression


Time series regression can be approached in several ways:

1. `Basic trend models`: Using time as a predictor
2. `Seasonal dummy variables`: Adding indicators for seasons/months
3. `Regression with ARIMA errors`: Combining regression with time series error structure

## Cross-Validation in Time Series

Time series cross-validation is different from traditional cross-validation because we must respect the temporal order of observations.

1. `Rolling window approach`: Train on a fixed window of past data
2. `Walk-forward optimization`: Incrementally add new observations
3. `Performance evaluation` using `RMSE` and other metrics

Advanced Smoothing Methods

TBATS (Trigonometric, Box-Cox transform, ARMA errors, Trend, and Seasonal components):

Handles multiple seasonal patterns
Automatically selects optimal parameters
Particularly useful for complex seasonal patterns

Practical Considerations

When choosing a forecasting method, consider:

Data characteristics (trend, seasonality, noise level)
Forecast horizon (short-term vs. long-term)
Amount of historical data available
Computational resources
Need for interpretability

Model Selection Process

The recommended process is:
a. Start with simple methods (moving averages, simple exponential smoothing)
b. Check if they capture the main patterns in your data
c. If not, gradually increase complexity (add trend, seasonality)
d. Use cross-validation to compare model performance
e. Consider ensemble methods for important forecasts

Important R Functions

The code demonstrates several key R functions:

ts(): Creates time series objects
ses(), holt(), hw(): Different exponential smoothing methods
auto.arima(): Automatic ARIMA model selection
tbats(): Complex seasonal patterns
accuracy(): Compute forecast accuracy measures
