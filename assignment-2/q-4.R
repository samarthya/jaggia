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

# Create filtered dataset focusing on the last 5 years
filtered_data <- transactions %>%
    inner_join(neighborhoods, by = "NEIGHBORHOOD_ID") %>%
    inner_join(building_class, by = c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
    filter(
        # Apply residential filters
        SALE_PRICE >= 100000,
        SALE_PRICE <= 10000000,
        GROSS_SQUARE_FEET >= 500,
        GROSS_SQUARE_FEET <= 10000,
        (SALE_PRICE / GROSS_SQUARE_FEET) >= 100,
        (SALE_PRICE / GROSS_SQUARE_FEET) <= 1000,
        TYPE == "RESIDENTIAL"
    ) %>%
    mutate(
        SALE_DATE = as.Date(SALE_DATE),
        YEAR = as.numeric(format(SALE_DATE, "%Y"))
    ) %>%
    # Keep only last 5 years
    filter(YEAR >= max(YEAR) - 4)

# Get Ridgewood's borough
ridgewood_borough <- filtered_data %>%
  filter(toupper(NEIGHBORHOOD_NAME) == "RIDGEWOOD") %>%
  select(BOROUGH_ID) %>%
  distinct() %>%
  pull()

# Calculate metrics for the top neighborhoods
top_neighborhoods <- filtered_data %>%
  filter(BOROUGH_ID == ridgewood_borough) %>%
  group_by(NEIGHBORHOOD_NAME) %>%
  summarise(
    avg_price_per_sqft = mean(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    n_transactions = n(),
    .groups = "drop"
  ) %>%
  filter(n_transactions >= 20) %>%  # Ensure sufficient data points
  top_n(4, avg_price_per_sqft) %>%  # Get top 4 by price
  pull(NEIGHBORHOOD_NAME) %>%
  c("RIDGEWOOD", .)  # Add Ridgewood to the list

# Create time series for selected neighborhoods
selected_trends <- filtered_data %>%
  filter(NEIGHBORHOOD_NAME %in% top_neighborhoods) %>%
  group_by(NEIGHBORHOOD_NAME, YEAR) %>%
  summarise(
    avg_price_per_sqft = mean(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    n_transactions = n(),
    .groups = "drop"
  )

# Create enhanced visualization
ggplot(selected_trends,
       aes(x = as.numeric(YEAR),
           y = avg_price_per_sqft,
           color = NEIGHBORHOOD_NAME)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = n_transactions)) +
  scale_color_manual(values = c(
    "RIDGEWOOD" = "#E74C3C",  # Highlight Ridgewood in red
    setNames(scales::hue_pal()(length(top_neighborhoods) - 1),
            setdiff(top_neighborhoods, "RIDGEWOOD"))
  )) +
  theme_minimal() +
  labs(
    title = "Ridgewood vs Top Nearby Neighborhoods: Last 5 Years",
    subtitle = "Average Price per Square Foot Comparison",
    x = "Year",
    y = "Price per Square Foot ($)",
    color = "Neighborhood",
    size = "Number of\nTransactions"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Calculate summary statistics for clear comparison
neighborhood_summary <- filtered_data %>%
  filter(NEIGHBORHOOD_NAME %in% top_neighborhoods) %>%
  group_by(NEIGHBORHOOD_NAME) %>%
  summarise(
    avg_price_per_sqft = mean(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    median_price_per_sqft = median(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    total_transactions = n(),
    recent_trend = (last(SALE_PRICE / GROSS_SQUARE_FEET) -
                   first(SALE_PRICE / GROSS_SQUARE_FEET)) /
                   first(SALE_PRICE / GROSS_SQUARE_FEET) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(avg_price_per_sqft))

print(neighborhood_summary)