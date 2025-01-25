# install.packages("readxl")

library("readxl")
myData <- read_excel("~/Downloads/Boston-MET/jaggia_ba_1e_ch02_data.xlsx", sheet = "Customers")


# stop(" I am stopping by force here!")

# Function to display columns
# @Param myData
display_columns <- function(allData, message = " Columns: ") {
    col_names <- names(allData)
    print(paste(message, paste(col_names, collapse = ", ")))
}

sprintf(" Total number of rows: %d", nrow(myData))
sprintf(" Total number of columns: %d", ncol(myData))

display_columns(myData)

print(" Head: ")
print(" ---------------------- ")
# To display all columns by default it will only display 6 columns
# options(width = 14)
head(myData, 14)
print(" ---------------------- ")


# Recency score
# convert by using as.numeric()
myData$DaysSinceLastReverse <- as.numeric(myData$DaysSinceLast * -1)

# col_names <- names(myData)
# sprintf(" New Columns: %s", paste(col_names, collapse = ", "))
display_columns(myData, " New Columns:")

# Create 5 bins for Recency Frequenct Monetary (RFM) for DaysSinceLastReverse, NumOfOrders, Spending2018
recencyBins <- quantile(myData$DaysSinceLastReverse, probs = seq(0, 1, by=0.20), names=T)

print(" Recency Bins: ")
names(recencyBins)
# recencyBins

freqencyBins <- quantile(myData$NumOfOrders, probs = seq(0, 1, by = 0.20), names = T)

print(" Frequency Bins: ")
names(freqencyBins)

monetaryBins <- quantile(myData$Spending2018, probs = seq(0, 1, by = 0.20), names = T)
print(" Monetary Bins: ")
names(monetaryBins)

myData$Recency <- cut(myData$DaysSinceLastReverse, breaks = recencyBins, labels = c("1", "2", "3", "4", "5"), include.lowest = TRUE, right = FALSE)

table(myData$Recency)

myData$Frequency <- cut(myData$NumOfOrders, breaks = freqencyBins, labels = c("1", "2", "3", "4", "5"), include.lowest = TRUE, right = FALSE)


table(myData$Frequency)

myData$Monetary <- cut(myData$Spending2018, breaks = monetaryBins, labels = c("1", "2", "3", "4", "5"), include.lowest = TRUE, right = FALSE)

table(myData$Monetary)
display_columns(myData)

myData$RFM <- paste(myData$Recency, myData$Frequency, myData$Monetary, sep = "")

display_columns(myData)
head(myData$RFM)

myData$BinnedIncome <- cut(myData$Income, breaks = 5, labels = c("1", "2", "3", "4", "5"), include.lowest = TRUE, right = FALSE)

display_columns(myData)
head(myData$BinnedIncome)

levels(cut(myData$Income, breaks = 5))
table(myData$BinnedIncome)
# What if 3 bins instead of 5

myData$BinnedIncome <- cut(myData$Income, breaks = 3, labels = c("1", "2", "3"), include.lowest = TRUE, right = FALSE)

head(myData$BinnedIncome)

myData$MembershipTier <- cut(myData$Spending2018, breaks = c(0, 250, 100, Inf), labels = c("Bronze", "Silver", "Gold"))

head(myData$MembershipTier)
display_columns(myData)

# View(myData$MembershipTier, "Table")