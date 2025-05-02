Test_data <- merged_data_final[merged_data_final$"Air Pollutant" == "C6H6", ]

str(Test_data)

print(cor(Test_data[, c("Value", "Inlet.Height", "Kerb.Distance", "Building.Distance", "Altitude")]))


ggplot(Test_data, aes(x = `Month`, y=`Value`, group=`Month`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Month", x = "Month", y="Value") +
  theme_minimal()

ggplot(Test_data, aes(x = `Year`, y=`Value`, group=`Year`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Year", x = "Year", y="Value") +
  theme_minimal()


ggplot(Test_data, aes(x = `Air.Quality.Station.Type`, y=`Value`, group=`Air.Quality.Station.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Type", x = "Air.Quality.Station.Type", y="Value") +
  theme_minimal()

ggplot(Test_data, aes(x = `Air.Quality.Station.Area`, y=`Value`, group=`Air.Quality.Station.Area`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Area", x = "Air.Quality.Station.Area", y="Value") +
  theme_minimal()

ggplot(Test_data, aes(x = `Main.Emission.Sources`, y=`Value`, group=`Main.Emission.Sources`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Main.Emission.Sources", x = "Main.Emission.Sources", y="Value") +
  theme_minimal()

ggplot(Test_data, aes(x = `Measurement.Method`, y=`Value`, group=`Measurement.Method`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Method", x = "Measurement.Method", y="Value") +
  theme_minimal()


ggplot(Test_data, aes(x = `Measurement.Type`, y=`Value`, group=`Measurement.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Type", x = "Measurement.Type", y="Value") +
  theme_minimal()

print(Test_data %>%
        group_by(`Air.Quality.Station.Area`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)

print(unique(Test_data$Air.Quality.Station.Type))
print(length(unique(Test_data$Air.Quality.Station.Type)))
print(unique(Test_data$Air.Quality.Station.Area))
print(length(unique(Test_data$Air.Quality.Station.Area)))
print(unique(Test_data$Main.Emission.Sources))
print(length(unique(Test_data$Main.Emission.Sources)))
print(unique(Test_data$Measurement.Method))
print(length(unique(Test_data$Measurement.Method)))
print(unique(Test_data$Building.Distance))


set.seed(123)

# Create a data split object
data_split_C6H6 <- initial_split(Test_data, prop = 0.9)

# Extract the training and testing (validation) datasets
train_C6H6_df <- training(data_split_C6H6)
validation_C6H6_df <- testing(data_split_C6H6)



model_formula_test <- Value ~ 
  Year + Month  + Altitude +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + #Kerb.Distance + #Building.Distance +
  Main.Emission.Sources #+
  #Measurement.Method

#str(train_C6H6_df)


lm_model_test <- lm(model_formula_test, data = train_C6H6_df)

summary(lm_model_test)

plot(lm_model_test)


validation_C6H6_df$predicted <- predict(lm_model_test, newdata = validation_C6H6_df)
str(validation_C6H6_df)

#summary(test_df)



# Assume 'actual_values' and 'predicted_values' are your vectors
rmse_metrics_c6h6 <- rmse(validation_C6H6_df$Value, validation_C6H6_df$predicted)
print(paste("RMSE (Metrics package):", rmse_metrics_c6h6))

alias(lm_model_test)
colSums(is.na(train_C6H6_df))


Test_data <- Test_data %>%
  mutate(
    log_KerbDistance = log1p(`Kerb.Distance`),
    log_InletHeight = log1p(`Inlet.Height`)
  )


#Test_data$Air.Quality.Station.Type <- droplevels(Test_data$Air.Quality.Station.Type)
#Test_data$Air.Quality.Station.Area <- droplevels(Test_data$Air.Quality.Station.Area)
#Test_data$Main.Emission.Sources <- droplevels(Test_data$Main.Emission.Sources)


model_formula_test_gwr <- Value ~ 
  Year + Month  + Altitude +
  #Air.Quality.Station.Type + Air.Quality.Station.Area + 
  log_InletHeight + log_KerbDistance #+
  #Main.Emission.Sources

Test_data$Year <- factor(Test_data$Year)
Test_data$Month <- factor(Test_data$Month)

set.seed(123)

# Create a data split object
data_split_C6H6_gwr <- initial_split(Test_data, prop = 0.9)

# Extract the training and testing (validation) datasets
train_C6H6_gwr_df <- training(data_split_C6H6_gwr)
validation_C6H6_gwr_df <- testing(data_split_C6H6_gwr)


test_df <- as.data.frame(train_C6H6_gwr_df)

coordinates(test_df) <- ~Longitude + Latitude

proj4string(test_df) <- CRS("+init=epsg:4326")
test_proj <- spTransform(test_df, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))

any(is.na(test_df))
str(test_df)

#cor_matrix <- cor(Test_data[, numeric_cols], use = "complete.obs")
#print(cor_matrix)
#vif(lm(model_formula_test, data = test_proj))

#coordinates(test_proj)  # Check coordinates
#proj4string(test_proj)  # Check projection




bw_opt <- bw.gwr(model_formula_test_gwr, data = test_proj, approach = "CV", kernel = "bisquare", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_opt, "\n")

gwr_model <- gwr.basic(model_formula_test_gwr, data = test_proj, bw = bw_opt, kernel = "bisquare", adaptive = TRUE)

print(gwr_model)




validation_C6H6_gwrdf <- as.data.frame(validation_C6H6_gwr_df)

# Define spatial coordinates (Longitude and Latitude must be present in validation_C6H6_gwr_df)
coordinates(validation_C6H6_gwrdf) <- ~Longitude + Latitude

# Assign the same coordinate reference system (CRS) as the training data
proj4string(validation_C6H6_gwrdf) <- CRS("+init=epsg:4326")

# Transform to the same projection as the training data (UTM Zone 29)
validation_proj <- spTransform(validation_C6H6_gwrdf, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))



# Create the design matrix for the validation set
X_valid <- model.matrix(model_formula_test_gwr, data = as.data.frame(validation_proj))

# Extract local coefficients from the GWR model
gwr_results <- gwr_model$SDF

# Inspect column names for debugging
cat("X_valid column names:\n")
print(colnames(X_valid))
cat("gwr_results@data column names:\n")
print(names(gwr_results@data))

# Initialize predicted values
predicted_values <- numeric(nrow(validation_proj))

# Loop over validation points
for (i in 1:nrow(validation_proj)) {
  # Calculate distances to all training points
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  # Extract the local coefficients for the nearest training point
  coef_names <- colnames(X_valid)  # e.g., "(Intercept)", "Year2016", "Month2", etc.
  local_coef <- numeric(length(coef_names))
  
  # Map the coefficients from gwr_results to match X_valid
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  # Adjust to match gwr_results@data
    } else {
      coef_name <- coef_name  # No prefix needed; names already match (e.g., "Year2016", "Month2")
    }
    
    # Check if the coefficient exists in gwr_results@data
    if (coef_name %in% names(gwr_results@data)) {
      local_coef[j] <- gwr_results@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results@data"))
    }
  }
  
  # Compute the prediction
  predicted_values[i] <- X_valid[i, ] %*% local_coef
}

# Extract actual values
actual_values <- validation_proj$Value  # Replace "Value" with your response variable name if different

# Calculate RMSE
rmse <- sqrt(mean((actual_values - predicted_values)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse, "\n")





