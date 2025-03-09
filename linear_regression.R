library(dplyr)  
library(lubridate)
library(car)      




merged_data_final <- merged_data_final %>%
  mutate(
    Start = as.Date(Start),
    Year = year(Start),
    Month = month(Start)
  )

merged_data_final <- merged_data_final %>%
  mutate(
    Air.Quality.Station.Type = as.factor(`Air Quality Station Type`),
    Air.Quality.Station.Area = as.factor(`Air Quality Station Area`),
    Inlet.Height = `Inlet Height`,
    Kerb.Distance = `Kerb Distance`,
    Building.Distance = `Building Distance`,
    Main.Emission.Sources = as.factor(`Main Emission Sources`),
    Measurement.Type = as.factor(`Measurement Type`),
    Measurement.Method = as.factor(`Measurement Method`)
  )


boxplot(Value ~ Air.Quality.Station.Type, data = merged_data_final)
boxplot(Value ~ Air.Quality.Station.Area, data = merged_data_final)
boxplot(Value ~ Main.Emission.Sources, data = merged_data_final)
boxplot(Value ~ Measurement.Type, data = merged_data_final)
boxplot(Value ~ Measurement.Method, data = merged_data_final)

sapply(merged_data_final[c(
  "Year",
  "Month",
  "Inlet.Height",
  "Kerb.Distance",
  "Building.Distance",
  "Air.Quality.Station.Type",
  "Air.Quality.Station.Area",
  "Main.Emission.Sources",
  "Measurement.Type",
  "Measurement.Method"
)], function(x) length(unique(x)))

factors <- c("Year", "Month", "Air.Quality.Station.Type", "Air.Quality.Station.Area", 
             "Inlet.Height", "Kerb.Distance", "Building.Distance", "Main.Emission.Sources", 
             "Measurement.Type", "Measurement.Method")

sapply(merged_data_final[, factors], function(x) {
  if (is.factor(x)) {
    levels(x)
  } else {
    unique(x)
  }
})

#merged_data_final <- droplevels(merged_data_final)

#merged_data_final$Year <- as.factor(merged_data_final$Year)
#merged_data_final$Month <- as.factor(merged_data_final$Month)
#merged_data_final$Air.Quality.Station.Type <- as.factor(merged_data_final$Air.Quality.Station.Type)
#merged_data_final$Air.Quality.Station.Area <- as.factor(merged_data_final$Air.Quality.Station.Area)
#merged_data_final$Main.Emission.Sources <- as.factor(merged_data_final$Main.Emission.Sources)
#merged_data_final$Measurement.Type <- as.factor(merged_data_final$Measurement.Type)
#merged_data_final$Measurement.Method <- as.factor(merged_data_final$Measurement.Method)

factor_cols <- sapply(merged_data_final, is.factor)
names(factor_cols)[factor_cols]

sapply(merged_data_final[, names(factor_cols)[factor_cols]], nlevels)

sapply(merged_data_final[, names(factor_cols)[factor_cols]], function(x) sum(is.na(x)))

merged_data_final$Measurement.Method <- as.character(merged_data_final$Measurement.Method) 
merged_data_final$Measurement.Method[is.na(merged_data_final$Measurement.Method)] <- "Unknown"
merged_data_final$Measurement.Method <- as.factor(merged_data_final$Measurement.Method)





model_formula <- Value ~ 
  Year + Month +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + Kerb.Distance + Building.Distance +
  Main.Emission.Sources +
  Measurement.Type + Measurement.Method


lm_model <- lm(model_formula, data = merged_data_final)

summary(lm_model)

cor_matrix <- cor(merged_data_final[, sapply(merged_data_final, is.numeric)], use = "complete.obs")
print(cor_matrix)

alias(lm_model)


vif(lm_model)  # Values >5-10 indicate multicollinearity

set.seed(123) 
sample_indices <- sample(nrow(merged_data_final), 50000)
lm_sample <- lm(model_formula, data = merged_data_final[sample_indices, ])
plot(lm_sample)

