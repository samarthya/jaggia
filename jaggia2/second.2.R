# Read the Excel file
library(readxl)
library(moments)
library(ggplot2)

setwd("~/Downloads/Boston-MET/Jaggia/Jaggia2")

# X11()

# read the data in case the file is not there
# throw an error
gas_data <- tryCatch(
  read_excel("jaggia_ba_1e_ch03_data.xlsx", sheet = "Gas_2019"),
  error = function(e) {
    stop("Error reading file: ", e$message)
  }
)

str(gas_data)

# Create price breaks for the frequency distribution and histogram
price_breaks <- seq(1.70, 3.40, by = 0.30)

# Calculate descriptive statistics
mean_price <- mean(
  gas_data$Price # Mean
)

median_price <- median(
  gas_data$Price # Median
)

sd_price <- sd(
  gas_data$Price # Standard deviation
)

price_mode <- as.numeric(
  names(
    which.max(table(gas_data$Price)) # Mode
  )
)



# Create a histogram with density curve, mean, median, and mode lines
histogram_plot <- ggplot(gas_data, aes(x = Price)) +
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


print(histogram_plot)

file_path <- sprintf("%s/gas_price_histogram_with_density.png", getwd())
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


# Print summary statistics (optional)
cat("Summary Statistics:\n")
cat(sprintf("Mean Price: $%.2f\n", mean_price))
cat(sprintf("Median Price: $%.2f\n", median_price))
cat(sprintf("Mode Price: $%.2f\n", price_mode)) # Print the mode.
cat(sprintf("Standard Deviation: $%.2f\n", sd_price))

# dev.hold()
# Sys.sleep(30)
