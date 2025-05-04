Test_data <- merged_data_final[merged_data_final$"Air Pollutant" == "C6H6", ]
nrow(Test_data)
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

ggplot(Test_data, aes(x = `Hour`, y=`Value`, group=`Hour`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Hour", x = "Hour", y="Value") +
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

data_split_C6H6 <- initial_split(Test_data, prop = 0.9)

train_C6H6_df <- training(data_split_C6H6)
validation_C6H6_df <- testing(data_split_C6H6)



model_formula_test <- Value ~ 
  Year + Month  + Altitude +
  #Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height #+ Kerb.Distance + Building.Distance +
  #Main.Emission.Sources +
  #Measurement.Method

#str(train_C6H6_df)


lm_model_test <- lm(model_formula_test, data = train_C6H6_df)

summary(lm_model_test)

original_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(lm_model_test)
par(original_par)


validation_C6H6_df$predicted <- predict(lm_model_test, newdata = validation_C6H6_df)
str(validation_C6H6_df)

#summary(test_df)

library(Metrics)

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


model_formula_C6H6_gwr <- Value ~ 
  Year + Month  + Altitude +
  #Air.Quality.Station.Type + Air.Quality.Station.Area + 
  log_InletHeight #+ log_KerbDistance +
#Main.Emission.Sources



Test_data$Year <- factor(Test_data$Year)
Test_data$Month <- factor(Test_data$Month)

set.seed(123)

data_split_C6H6_gwr <- initial_split(Test_data, prop = 0.9)

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





bw_C6H6_cv_bisq_adpt <- bw.gwr(model_formula_C6H6_gwr, data = test_proj, approach = "CV", kernel = "bisquare", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H6_cv_bisq_adpt, "\n")

gwr_model_C6H6_cv_bisq_adpt <- gwr.basic(model_formula_C6H6_gwr, data = test_proj, bw = bw_C6H6_cv_bisq_adpt, kernel = "bisquare", adaptive = TRUE)

print(gwr_model_C6H6_cv_bisq_adpt)




validation_C6H6_gwrdf <- as.data.frame(validation_C6H6_gwr_df)

coordinates(validation_C6H6_gwrdf) <- ~Longitude + Latitude

proj4string(validation_C6H6_gwrdf) <- CRS("+init=epsg:4326")

validation_proj <- spTransform(validation_C6H6_gwrdf, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))



X_valid <- model.matrix(model_formula_C6H6_gwr, data = as.data.frame(validation_proj))

gwr_results <- gwr_model$SDF

cat("X_valid column names:\n")
print(colnames(X_valid))
cat("gwr_results@data column names:\n")
print(names(gwr_results@data))


predicted_values <- numeric(nrow(validation_proj))

for (i in 1:nrow(validation_proj)) {
  
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  coef_names <- colnames(X_valid)  
  local_coef <- numeric(length(coef_names))
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    

    if (coef_name %in% names(gwr_results@data)) {
      local_coef[j] <- gwr_results@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results@data"))
    }
  }
  
  predicted_values[i] <- X_valid[i, ] %*% local_coef
}

actual_values <- validation_proj$Value  

# Calculate RMSE
rmse <- sqrt(mean((actual_values - predicted_values)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse, "\n")


print(predicted_values)


############################################################################
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
ireland_map <- ne_countries(country = "Ireland", scale = "medium", returnclass = "sf")
gwr_results_sf <- st_as_sf(gwr_model_C6H6_cv_bisq_adpt$SDF)
names(gwr_results_sf)

ggplot() +
  geom_sf(data = ireland_map, fill = "lightgray", color = "white") +
  geom_sf(data = gwr_results_sf, aes(color = Local_R2), size = 2) +
  scale_color_viridis_c() +
  labs(title = "GWR Local R-squared", color = "Local R-squared") +
  theme_minimal()


gwr_results_sf_transformed <- st_transform(gwr_results_sf, st_crs(ireland_counties))

gwr_results_with_regions_C6H6 <- st_join(gwr_results_sf_transformed, ireland_counties, join = st_within)

aggregated_results_C6H6 <- gwr_results_with_regions_C6H6 %>%
  st_drop_geometry() %>% 
  group_by(name) %>% 
  summarise(
    mean_local_R2 = mean(Local_R2, na.rm = TRUE),
    mean_yhat = mean(yhat, na.rm = TRUE)
  )
ireland_counties_with_results_C6H6 <- ireland_counties %>%
  left_join(aggregated_results_C6H6, by = "name")

ggplot(data = ireland_counties_with_results_C6H6) +
  geom_sf(aes(fill = mean_yhat), color = "white") + 
  geom_sf(data = gwr_results_sf, aes(fill = yhat),shape = 21,color = "black",size = 3,show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Local Predictions") + 
  labs(title = "Mean GWR Localised Prediction by County in Ireland") +
  theme_minimal()

#######################################################################################################
bw_C6H6_AICc_bisq_fixed <- bw.gwr(model_formula_C6H6_gwr, data = test_proj, approach = "AICc", kernel = "bisquare", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H6_AICc_bisq_fixed, "\n")

gwr_model_C6H6_AICc_bisq_fixed <- gwr.basic(model_formula_C6H6_gwr, data = test_proj, bw = bw_C6H6_AICc_bisq_fixed, kernel = "bisquare", adaptive = TRUE)

print(gwr_model_C6H6_AICc_bisq_fixed)


#######################################################################################################
bw_C6H6_cv_gaus_adpt <- bw.gwr(model_formula_C6H6_gwr, data = test_proj, approach = "CV", kernel = "gaussian", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H6_cv_gaus_adpt, "\n")

gwr_model_C6H6_cv_gaus_adpt <- gwr.basic(model_formula_C6H6_gwr, data = test_proj, bw = bw_C6H6_cv_gaus_adpt, kernel = "gaussian", adaptive = TRUE)
print(gwr_model_C6H6_cv_gaus_adpt)


gwr_C6H6_cv_gaus_results <- gwr_model_C6H6_cv_gaus_adpt$SDF
predicted_values_C6H6_cv_gaus_adpt <- numeric(nrow(validation_proj))

for (i in 1:nrow(validation_proj)) {

  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  
  coef_names <- colnames(X_valid)  
  local_coef <- numeric(length(coef_names))
  

  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    
    if (coef_name %in% names(gwr_C6H6_cv_gaus_results@data)) {
      local_coef[j] <- gwr_C6H6_cv_gaus_results@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_C6H6_cv_gaus_results@data"))
    }
  }
  
  predicted_values_C6H6_cv_gaus_adpt[i] <- X_valid[i, ] %*% local_coef
}

# Calculate RMSE
rmse_C6H6_cv_gaus_adpt <- sqrt(mean((actual_values - predicted_values_C6H6_cv_gaus_adpt)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_C6H6_cv_gaus_adpt, "\n")


#######################################################################################################
bw_C6H6_AICc_gaus_adpt <- bw.gwr(model_formula_C6H6_gwr, data = test_proj, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H6_AICc_gaus_adpt, "\n")

gwr_model_C6H6_AICc_gaus_adpt <- gwr.basic(model_formula_C6H6_gwr, data = test_proj, bw = bw_C6H6_AICc_gaus_adpt, kernel = "gaussian", adaptive = TRUE)

print(gwr_model_C6H6_AICc_gaus_adpt)

#######################################################################################################
bw_C6H6_cv_exp_adpt <- bw.gwr(model_formula_C6H6_gwr, data = test_proj, approach = "CV", kernel = "exponential", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H6_cv_exp_adpt, "\n")

gwr_model_C6H6_cv_exp_adpt <- gwr.basic(model_formula_C6H6_gwr, data = test_proj, bw = bw_C6H6_cv_exp_adpt, kernel = "exponential", adaptive = TRUE)
print(gwr_model_C6H6_cv_exp_adpt)


gwr_C6H6_cv_exp_results <- gwr_model_C6H6_cv_exp_adpt$SDF
predicted_values_C6H6_cv_exp_adpt <- numeric(nrow(validation_proj))

for (i in 1:nrow(validation_proj)) {
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  
  coef_names <- colnames(X_valid)  
  local_coef <- numeric(length(coef_names))
  
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    
    if (coef_name %in% names(gwr_C6H6_cv_exp_results@data)) {
      local_coef[j] <- gwr_C6H6_cv_exp_results@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_C6H6_cv_exp_results@data"))
    }
  }
  
  predicted_values_C6H6_cv_exp_adpt[i] <- X_valid[i, ] %*% local_coef
}

# Calculate RMSE
rmse_C6H6_cv_exp_adpt <- sqrt(mean((actual_values - predicted_values_C6H6_cv_exp_adpt)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_C6H6_cv_exp_adpt, "\n")



#######################################################################################################
bw_C6H6_AICc_exp_adpt <- bw.gwr(model_formula_C6H6_gwr, data = test_proj, approach = "AICc", kernel = "exponential", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H6_AICc_exp_adpt, "\n")

gwr_model_C6H6_AICc_exp_adpt <- gwr.basic(model_formula_C6H6_gwr, data = test_proj, bw = bw_C6H6_AICc_exp_adpt, kernel = "exponential", adaptive = TRUE)

print(gwr_model_C6H6_AICc_exp_adpt)
