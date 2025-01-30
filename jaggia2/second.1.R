# Read the Excel file
library(readxl)
library(moments)
library(ggplot2)

setwd("~/Downloads/Boston-MET/Jaggia/Jaggia2")

# Create price breaks for the frequency distribution and histogram
price_breaks <- seq(1.70, 3.40, by = 0.30)

# Read the data
gas_data <- tryCatch(
  read_excel("jaggia_ba_1e_ch03_data.xlsx", sheet = "Gas_2019"),
  error = function(e) {
    stop("Error reading file: ", e$message)
  }
)

# Descriptive statistics
mean_price <- mean(gas_data$Price)
median_price <- median(gas_data$Price)
sd_price <- sd(gas_data$Price)
price_mode <- as.numeric(
  names(
    which.max(table(gas_data$Price)) # Mode
  )
)
#



# Create frequency distribution table using cut()
freq_dist <- cut(gas_data$Price,
  breaks = price_breaks,
  right = TRUE,
  include.lowest = TRUE
)

freq_table <- table(freq_dist)

summary(freq_table)

# Create a histogram using ggplot2
histogram_plot <- ggplot(gas_data, aes(x = Price)) +
  geom_histogram(binwidth = 0.3, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Gas Prices", x = "Price ($)", y = "Frequency") +
  theme_classic()

# Create a histogram with density curve, mean, median, and mode lines
histogram_plot_with_density <- ggplot(gas_data, aes(x = Price)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 0.3,
    fill = "lightblue",
    color = "black"
  ) + # For density curve

  geom_density(color = "blue", linewidth = 1) + # Add density curve

  geom_vline(aes(xintercept = mean_price),
    color = "red", linetype = "dashed", linewidth = 3
  ) + # Mean line

  geom_vline(
    aes(xintercept = median_price),
    color = "#ff9900", linetype = "dashed",
    linewidth = 2
  ) + # Median line

  geom_vline(
    aes(xintercept = price_mode),
    color = "purple",
    linetype = "dashed",
    linewidth = 1
  ) + # Mode line
  labs(
    title = "Histogram of Gas Prices with Density Curve",
    x = "Price ($)",
    y = "Density"
  ) +
  theme_classic() +
  annotate("text",
    x = mean_price + 0.05, y = 0.8,
    label = paste(
      "Mean =",
      round(mean_price, 2)
    ),
    color = "red"
  ) + # Annotate
  annotate("text",
    x = median_price + 0.05, y = 0.7,
    label = paste(
      "Median =",
      round(median_price, 2)
    ),
    color = "#ffbb00"
  ) + # Annotate
  annotate("text",
    x = price_mode + 0.05, y = 0.6,
    label = paste(
      "Mode =",
      round(price_mode, 2)
    ),
    color = "purple"
  ) # Annotate


# Calculate number of states with prices > $2.60 and add explanation
high_price_threshold <- 2.60 # Define a meaningful variable for clarity.
num_high_prices <- sum(gas_data$Price > high_price_threshold)

# Calculate skewness
price_skewness <- skewness(gas_data$Price)


# Print formatted results and explanations
cat("Summary Statistics:\n")
cat(sprintf("Mean Price: $%.2f\n", mean_price))
cat(sprintf("Median Price: $%.2f\n", median_price))
cat(sprintf("Standard Deviation: $%.2f\n", sd_price))

cat("\nFrequency Distribution:\n")
print(freq_table)

cat(sprintf(
  "\nNumber of states with prices > $%.2f: %d\n",
  high_price_threshold,
  num_high_prices
))

cat(sprintf("Skewness: %.3f\n", price_skewness))

if (price_skewness > 0) {
  cat("The distribution is positively skewed.\n")
} else {
  cat("The distribution is negatively skewed.\n")
}

X11()
# Display the ggplot
print(histogram_plot)
X11()
print(histogram_plot_with_density)

file_path <- sprintf("%s/gas_price_histogram_with_density.png", getwd())

# Save the plot (optional)
save_result <- tryCatch(
  {
    ggsave(
      file_path,
      histogram_plot,
      scale = 1, width = 8, height = 6
    ) # Save plot
    TRUE
  },
  error = function(e) {
    message("Error saving plot: ", e$message)
    FALSE # Return FALSE if an error occurred
  },
  warning = function(w) {
    message("Warning while saving plot: ", w$message)
    TRUE # Consider warnings as potentially successful. Adjust return as needed.
  }
)

if (save_result) {
  message(paste("Plot saved successfully: ", file_path))
} else {
  message("Plot not saved.")
}



dev.hold()
Sys.sleep(30)
