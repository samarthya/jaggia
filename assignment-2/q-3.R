# Load necessary libraries for data analysis and visualization
library(dplyr) # For data manipulation
library(ggplot2) # For creating visualizations
library(scales) # For formatting numbers in plots
library(broom)

# Read all the necessary data files
# Each file contains different aspects of NYC real estate information
transactions <- read.csv(
    "~/Downloads/Boston-MET/Assignment-2/NYC_RE/NYC_TRANSACTION_DATA.csv"
) # NYC_TRANSACTION_DATA.csv


neighborhoods <- read.csv(
    "~/Downloads/Boston-MET/Assignment-2/NYC_RE/NEIGHBORHOOD.csv"
) # NEIGHBORHOOD.csv

boroughs <- read.csv(
    "~/Downloads/Boston-MET/Assignment-2/NYC_RE/BOROUGH.csv"
) # BOROUGH.csv

building_class <- read.csv(
    "~/Downloads/Boston-MET/Assignment-2/NYC_RE/BUILDING_CLASS.csv"
) # BUILDING_CLASS.csv

# Create our initial dataset by joining the necessary tables
ridgewood_data <- transactions %>%
    inner_join(neighborhoods, by = "NEIGHBORHOOD_ID") %>%
    inner_join(building_class, by = c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
    inner_join(boroughs, by = "BOROUGH_ID") %>%
    filter(toupper(NEIGHBORHOOD_NAME) == "RIDGEWOOD")

# Now let's apply careful filtering to ensure data quality
filtered_ridgewood <- ridgewood_data %>%
    filter(
        # Remove properties with unrealistic sale prices
        SALE_PRICE >= 100000, # Minimum reasonable price for NYC property
        SALE_PRICE <= 10000000, # Upper limit to remove likely commercial properties

        # Remove properties with unrealistic square footage
        GROSS_SQUARE_FEET >= 500, # Minimum reasonable size for residential
        GROSS_SQUARE_FEET <= 10000, # Upper limit for typical residential

        # Remove unrealistic price per square foot
        (SALE_PRICE / GROSS_SQUARE_FEET) >= 100, # Minimum reasonable $/sqft
        (SALE_PRICE / GROSS_SQUARE_FEET) <= 1000, # Maximum reasonable $/sqft

        # Focus on residential properties
        TYPE == "RESIDENTIAL"
    )

# Calculate yearly averages with confidence intervals
yearly_trends <- filtered_ridgewood %>%
    mutate(
        YEAR = format(as.Date(SALE_DATE), "%Y"),
        price_per_sqft = SALE_PRICE / GROSS_SQUARE_FEET
    ) %>%
    group_by(YEAR) %>%
    summarise(
        avg_price_per_sqft = mean(price_per_sqft, na.rm = TRUE),
        n_transactions = n(),
        std_error = sd(price_per_sqft, na.rm = TRUE) / sqrt(n()),
        conf_low = avg_price_per_sqft - 1.96 * std_error,
        conf_high = avg_price_per_sqft + 1.96 * std_error,
        .groups = "drop"
    )

# Create an enhanced visualization
ggplot(yearly_trends, aes(x = as.numeric(YEAR), y = avg_price_per_sqft)) +
    # Add confidence interval ribbon
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
        fill = "lightblue", alpha = 0.3
    ) +
    # Add actual data points
    geom_point(aes(size = n_transactions), color = "#2C3E50") +
    # Add trend line
    geom_smooth(method = "lm", color = "#E74C3C", se = FALSE) +
    # Customize appearance
    theme_minimal() +
    labs(
        title = "Ridgewood Residential real-estate Trends ",
        subtitle = "Average Price per Sq Ft",
        x = "Year",
        y = "Price per Sq Ft ($)",
        size = "Number of\nTransactions",
        caption = "Data filtered for realistic residential properties only"
    ) +
    scale_y_continuous(labels = dollar_format()) +
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
    )

# Calculate trend statistics
trend_model <- lm(avg_price_per_sqft ~ as.numeric(YEAR), data = yearly_trends)
summary(trend_model)