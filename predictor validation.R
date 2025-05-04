summary(O3_data)

#check for NA predictors
sum(is.na(O3_data$Value))
sum(is.na(O3_data$Year))
sum(is.na(O3_data$Month))
sum(is.na(O3_data$Air.Quality.Station.Type))
sum(is.na(O3_data$Air.Quality.Station.Area))
sum(is.na(O3_data$Inlet.Height))
sum(is.na(O3_data$Kerb.Distance))
sum(is.na(O3_data$Building.Distance))
sum(is.na(O3_data$Main.Emission.Sources))
sum(is.na(O3_data$Measurement.Method))

library(ggplot2)
library(dplyr)
if (!require(e1071)) install.packages("e1071")
library(e1071)

O3_data <- as.data.frame(O3_data)


cat("Summary of Building Distance:\n")
print(summary(O3_data$`Building.Distance`))

cat("\nSummary of Kerb Distance:\n")
print(summary(O3_data$`Kerb.Distance`))


ggplot(O3_data, aes(x = `Building.Distance`)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Building Distance", x = "Building Distance", y = "Count") +
  theme_minimal()

ggplot(O3_data, aes(x = `Kerb.Distance`)) +
  geom_histogram(bins = 50, fill = "tomato", color = "black") +
  labs(title = "Histogram of Kerb Distance", x = "Kerb Distance", y = "Count") +
  theme_minimal()


skew_building <- skewness(O3_data$`Building.Distance`, na.rm = TRUE)
skew_kerb <- skewness(O3_data$`Kerb.Distance`, na.rm = TRUE)
skew_inlet <- skewness(O3_data$`Inlet.Height`, na.rm = TRUE)
cat("\nSkewness of Building Distance:", skew_building, "\n")
cat("Skewness of Kerb Distance:", skew_kerb, "\n")
cat("Skewness of Inlet Heigh:", skew_inlet, "\n")


ggplot(O3_data, aes(y = `Building.Distance`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Building Distance", y = "Building Distance") +
  theme_minimal()

ggplot(O3_data, aes(y = `Kerb.Distance`)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Kerb Distance", y = "Kerb Distance") +
  theme_minimal()



O3_data <- O3_data %>%
  mutate(
    log_BuildingDistance = log1p(`Building Distance`),
    log_KerbDistance = log1p(`Kerb Distance`)
  )


ggplot(O3_data, aes(x = log_BuildingDistance)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Log-transformed Building Distance", 
       x = "log(Building Distance + 1)", y = "Count") +
  theme_minimal()

ggplot(O3_data, aes(x = log_KerbDistance)) +
  geom_histogram(bins = 50, fill = "tomato", color = "black") +
  labs(title = "Histogram of Log-transformed Kerb Distance", 
       x = "log(Kerb Distance + 1)", y = "Count") +
  theme_minimal()
