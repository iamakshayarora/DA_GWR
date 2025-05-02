O3_data <- merged_data_final[merged_data_final$"Air Pollutant" == "O3", ]

cor_matrix <- cor(O3_data[, sapply(O3_data, is.numeric)], use = "complete.obs")
print(cor_matrix)
str(O3_data)
range(O3_data$Inlet.Height)
str(O3_data[, c("Year", "Month", "Inlet.Height", "Kerb.Distance", "Building.Distance", "Air.Quality.Station.Type", "Air.Quality.Station.Area", "Main.Emission.Sources", "Measurement.Type", "Measurement.Method")])

print(cor(O3_data[, c("Value","Year", "Month", "Inlet.Height", "Kerb.Distance", "Building.Distance")]))

unique_levels <- sapply(O3_data[, c("Air.Quality.Station.Type", "Air.Quality.Station.Area",
                                    "Main.Emission.Sources", 
                                    "Measurement.Method")], function(x) length(unique(x)))
print(unique_levels) #Measurement.Type only has 1 level, we will have to remove it

O3_data$Year <- factor(O3_data$Year)
O3_data$Month <- factor(O3_data$Month)

O3_data$Air.Quality.Station.Type <- factor(O3_data$Air.Quality.Station.Type)
unique(O3_data$Air.Quality.Station.Type)

O3_data$Air.Quality.Station.Area <- factor(O3_data$Air.Quality.Station.Area)
O3_data$Main.Emission.Sources <- factor(O3_data$Main.Emission.Sources)
O3_data$Measurement.Method <- factor(O3_data$Measurement.Method)

ggplot(O3_data, aes(x = `Month`, y=`Value`, group=`Month`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Month", x = "Month", y="Value") +
  theme_minimal()

ggplot(O3_data, aes(x = `Year`, y=`Value`, group=`Year`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Year", x = "Year", y="Value") +
  theme_minimal()

ggplot(O3_data, aes(x = `Hour`, y=`Value`, group=`Hour`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Hour", x = "Hour", y="Value") +
  theme_minimal()

O3_data$Inlet.Height2 <- factor(O3_data$Inlet.Height)

ggplot(O3_data, aes(x = `Inlet.Height2`, y=`Value`, group=`Inlet.Height2`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Inlet.Height2", x = "Inlet.Height2", y="Value") +
  theme_minimal()

ggplot(O3_data, aes(x = `Air.Quality.Station.Type`, y=`Value`, group=`Air.Quality.Station.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Type", x = "Air.Quality.Station.Type", y="Value") +
  theme_minimal()

ggplot(O3_data, aes(x = `Air.Quality.Station.Area`, y=`Value`, group=`Air.Quality.Station.Area`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Area", x = "Air.Quality.Station.Area", y="Value") +
  theme_minimal()

ggplot(O3_data, aes(x = `Main.Emission.Sources`, y=`Value`, group=`Main.Emission.Sources`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Main.Emission.Sources", x = "Main.Emission.Sources", y="Value") +
  theme_minimal()

ggplot(O3_data, aes(x = `Measurement.Method`, y=`Value`, group=`Measurement.Method`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Method", x = "Measurement.Method", y="Value") +
  theme_minimal()

ggplot(O3_data, aes(x = Inlet.Height, y = Value)) +
  geom_point() +
  labs(title = "Scatterplot of Value vs. Inlet.Height",
       x = "Inlet.Height",
       y = "Value")

remaining_categorical_cols <- c("Air.Quality.Station.Type", "Air.Quality.Station.Area",
                                "Main.Emission.Sources", "Measurement.Method")

for (i in 1:(length(remaining_categorical_cols) - 1)) {
  for (j in (i + 1):length(remaining_categorical_cols)) {
    col1 <- remaining_categorical_cols[i]
    col2 <- remaining_categorical_cols[j]
    
    contingency_table <- table(O3_data[[col1]], O3_data[[col2]])
    cat(paste("Contingency Table for:", col1, "and", col2, "\n"))
    print(contingency_table)
    
    chi2_result <- tryCatch(chisq.test(contingency_table),
                            error = function(e) paste("Error:", e$message))
    
    cat(paste("Chi-Square Test for:", col1, "and", col2, "\n"))
    print(chi2_result)
    cat("\n")
  }
}


sapply(O3_data[c(
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

#distinct Measurement Type with count
print(O3_data %>%
        group_by(`Measurement Type`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)



#install.packages("rsample")
library(rsample)

# Assume your data frame is named 'df'

# Set a seed for reproducibility
set.seed(123)

# Create a data split object
data_split_O3 <- initial_split(O3_data, prop = 0.9)

# Extract the training and testing (validation) datasets
train_O3_df <- training(data_split_O3)
validation_O3_df <- testing(data_split_O3)



model_formula <- Value ~ 
  Year + Month + Hour + Altitude +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + Kerb.Distance + Building.Distance +
  Main.Emission.Sources +
  Measurement.Method


lm_model_o3 <- lm(model_formula, data = train_O3_df)
summary(lm_model_o3)


validation_O3_df$predicted <- predict(lm_model_o3, newdata = validation_O3_df)

#install.packages("Metrics")
library(Metrics)

# Assume 'actual_values' and 'predicted_values' are your vectors
rmse_metrics <- rmse(validation_O3_df$Value, validation_O3_df$predicted)
print(paste("RMSE (Metrics package):", rmse_metrics))


print(O3_data %>%
        group_by(`predicted`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)))


anova(lm_model_o3)

alias(lm_model_o3)


vif(lm_model_o3)  # Values >5-10 indicate multicollinearity

set.seed(123) 
sample_indices <- sample(nrow(O3_data), 50000)
lm_sample_03 <- lm(model_formula, data = O3_data[sample_indices, ])
plot(lm_sample_03)


model_formula_O3_new <- Value ~ 
  Month +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + Kerb.Distance + Building.Distance +
  Measurement.Method


lm_model_o3_new <- lm(model_formula_O3_new, data = O3_data)

summary(lm_model_o3_new)

anova(lm_model_o3_new)
vif(lm_model_o3_new)


model_formula_O3_new2 <- Value ~ 
  Month + Altitude +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + Building.Distance +
  Measurement.Method


lm_model_o3_new2 <- lm(model_formula_O3_new2, data = O3_data)

summary(lm_model_o3_new2)

anova(lm_model_o3_new2)
vif(lm_model_o3_new2)


lm_sample_03_new <- lm(model_formula_O3_new2, data = O3_data[sample_indices, ])
plot(lm_sample_03_new)


str(O3_data)

print(O3_data %>%
        group_by(`Detection Limit`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


O3_data <- O3_data %>%
  mutate(
    Air.Quality.Station.Name = as.factor(`Air Quality Station Name`),
    Equivalence.Demonstrated = as.factor(`Equivalence Demonstrated`)
    
  )

model_formula_O3_new3 <- Value ~ 
  Year + Month + Altitude +
  Air.Quality.Station.Type + Air.Quality.Station.Area + Air.Quality.Station.Name +
  Inlet.Height + Building.Distance +
  Main.Emission.Sources +
  Measurement.Method + Equivalence.Demonstrated


lm_model_o3_new3 <- lm(model_formula_O3_new3, data = O3_data)

summary(lm_model_o3_new3)