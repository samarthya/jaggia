# Load necessary libraries for data analysis and visualization
library(dplyr) # For data manipulation
library(ggplot2) # For creating visualizations
library(lubridate) # For handling dates
library(scales) # For formatting numbers in plots
library(broom)

file_paths <- list(
  transactions = "~/Downloads/Boston-MET/Assignment-2/NYC_RE/NYC_TRANSACTION_DATA.csv", # nolint
  neighborhoods = "~/Downloads/Boston-MET/Assignment-2/NYC_RE/NEIGHBORHOOD.csv",
  boroughs = "~/Downloads/Boston-MET/Assignment-2/NYC_RE/BOROUGH.csv",
  building_class = "~/Downloads/Boston-MET/Assignment-2/NYC_RE/BUILDING_CLASS.csv"
)

data <- lapply(file_paths, function(path) {
  tryCatch(read.csv(path),
    error = function(e) stop("Error reading file: ", path)
  )
})

transactions <- data$transactions
neighborhoods <- data$neighborhoods
boroughs <- data$boroughs
building_class <- data$building_class


# Time to do some data cleaning
transactions <- transactions %>%
  filter(
    !is.na(SALE_DATE) # Remove rows where SALE_DATE is NA
  ) %>%
  mutate(
    SALE_PRICE = as.numeric(SALE_PRICE), # Convert SALE_PRICE to numeric
    SALE_DATE = as.Date(SALE_DATE),
    GROSS_SQUARE_FEET = as.numeric(GROSS_SQUARE_FEET),
    YEAR = year(SALE_DATE)
  )


transactions <- transactions %>%
  mutate(
    SALE_PRICE = ifelse(
      is.na(SALE_PRICE),
      median(SALE_PRICE,
        na.rm = TRUE
      ), SALE_PRICE
    ),
    GROSS_SQUARE_FEET = ifelse(
      is.na(GROSS_SQUARE_FEET),
      median(GROSS_SQUARE_FEET,
        na.rm = TRUE
      ),
      GROSS_SQUARE_FEET
    )
  )

str(transactions)

# Columns read
cat(
  " Transactions columns read: ", ncol(transactions), "\n",
  " Transactions column names: ", names(transactions), "\n",
  " Neighborhoods columns read: ", ncol(neighborhoods), "\n",
  " Neighborhoods columns names: ", names(neighborhoods), "\n",
  " Boroughs columns read: ", ncol(boroughs), "\n",
  " Boroughs columns names: ", names(boroughs), "\n",
  " Building columns read: ", ncol(building_class), "\n",
  " Building columns names: ", names(building_class), "\n"
)


str(building_class)

# Q.1 - Compute the average price of 1 square foot of
# residential real estate in Ridgewood for each year.
# Join and filter for Ridgewood properties
ridgewood_data <- transactions %>%
  inner_join(neighborhoods, by = "NEIGHBORHOOD_ID") %>%
  inner_join(building_class,
    by =
      c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")
  ) %>%
  inner_join(boroughs, by = "BOROUGH_ID") %>%
  filter( # Combined the filter
    toupper(NEIGHBORHOOD_NAME) == "RIDGEWOOD",
    # Filter out unrealistic values
    SALE_PRICE > 100000,
    GROSS_SQUARE_FEET > 350
    # SALE_PRICE / GROSS_SQUARE_FEET < 10000 # Remove extreme outliers
  ) %>%
  mutate(
    price_per_sqft = SALE_PRICE / GROSS_SQUARE_FEET
  )

str(building_class)
summarise(ridgewood_data)

# # Convert sale date and calculate price per square foot
# ridgewood_data <- ridgewood_data %>%
#   mutate(
#     price_per_sqft = SALE_PRICE / GROSS_SQUARE_FEET
#   )

# ridgewood_data$SALE_DATE <- as.Date(ridgewood_data$SALE_DATE)
# ridgewood_data$YEAR <- format(ridgewood_data$SALE_DATE, "%Y")
# ridgewood_data$price_per_sqft <-
#   ridgewood_data$SALE_PRICE / ridgewood_data$GROSS_SQUARE_FEET

# Calculate yearly averages
yearly_trends <- ridgewood_data %>%
  group_by(YEAR) %>% # groups by year
  summarise(
    avg_price_per_sqft = mean(price_per_sqft, na.rm = TRUE),
    n_transactions = n(), # Number of transactions
    std_dev = sd(price_per_sqft, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YEAR)

summary(yearly_trends)

# Create visualization with trend line
ggplot(yearly_trends, aes(x = as.numeric(YEAR), y = avg_price_per_sqft)) +
  # Add actual data points
  geom_point(size = 4, color = "#2C3E50") +
  # Add error bars showing standard deviation
  geom_errorbar(
    aes(
      ymin = avg_price_per_sqft - std_dev,
      ymax = avg_price_per_sqft + std_dev
    ),
    width = 0.2, color = "#7F8C8D", alpha = 0.5
  ) +
  # Add trend line
  geom_smooth(method = "lm", color = "#E74C3C", se = TRUE) +
  # Customize appearance
  theme_minimal() +
  labs(
    title = "Ridgewood Real Estate Price",
    subtitle = "Average Price per Square Foot with Trend Line",
    x = "Year",
    y = "Price per Square Foot ($)",
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


# Calculate trend statistics
# performs a linear regression to analyze the trend of
# average price per square foot over time in Ridgewood.
trend_model <- lm(avg_price_per_sqft ~ as.numeric(YEAR), data = yearly_trends)
trend_stats <- tidy(trend_model)
print(trend_stats)
