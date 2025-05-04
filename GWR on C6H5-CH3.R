C6H5_CH3_data <- merged_data_final[merged_data_final$"Air Pollutant" == "C6H5-CH3", ]
nrow(C6H5_CH3_data)
str(C6H5_CH3_data)

print(cor(C6H5_CH3_data[, c("Value", "Inlet.Height", "Kerb.Distance", "Building.Distance", "Altitude")]))
#Building Distance and Kerb Distance has high correlation ~0.7
#rest is fine, so we exclude Building.Distance for now

#refactor all non numerical columns
C6H5_CH3_data$Year <- factor(C6H5_CH3_data$Year)
C6H5_CH3_data$Month <- factor(C6H5_CH3_data$Month)
C6H5_CH3_data$Hour <- factor(C6H5_CH3_data$Hour)
C6H5_CH3_data$Air.Quality.Station.Type <- factor(C6H5_CH3_data$Air.Quality.Station.Type)
C6H5_CH3_data$Air.Quality.Station.Area <- factor(C6H5_CH3_data$Air.Quality.Station.Area)
C6H5_CH3_data$Main.Emission.Sources <- factor(C6H5_CH3_data$Main.Emission.Sources)
C6H5_CH3_data$Measurement.Method <- factor(C6H5_CH3_data$Measurement.Method)
C6H5_CH3_data$Measurement.Type <- factor(C6H5_CH3_data$Measurement.Type)


#now moving on the checking the factored variables via their box plots
ggplot(C6H5_CH3_data, aes(x = `Month`, y=`Value`, group=`Month`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Month", x = "Month", y="Value") +
  theme_minimal()

ggplot(C6H5_CH3_data, aes(x = `Year`, y=`Value`, group=`Year`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Year", x = "Year", y="Value") +
  theme_minimal()

ggplot(C6H5_CH3_data, aes(x = `Hour`, y=`Value`, group=`Hour`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Hour", x = "Hour", y="Value") +
  theme_minimal()#no hourly information, exclude this

ggplot(C6H5_CH3_data, aes(x = `Air.Quality.Station.Type`, y=`Value`, group=`Air.Quality.Station.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Type", x = "Air.Quality.Station.Type", y="Value") +
  theme_minimal()

ggplot(C6H5_CH3_data, aes(x = `Air.Quality.Station.Area`, y=`Value`, group=`Air.Quality.Station.Area`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Area", x = "Air.Quality.Station.Area", y="Value") +
  theme_minimal()

ggplot(C6H5_CH3_data, aes(x = `Main.Emission.Sources`, y=`Value`, group=`Main.Emission.Sources`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Main.Emission.Sources", x = "Main.Emission.Sources", y="Value") +
  theme_minimal()

ggplot(C6H5_CH3_data, aes(x = `Measurement.Method`, y=`Value`, group=`Measurement.Method`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Method", x = "Measurement.Method", y="Value") +
  theme_minimal()


ggplot(C6H5_CH3_data, aes(x = `Measurement.Type`, y=`Value`, group=`Measurement.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Type", x = "Measurement.Type", y="Value") +
  theme_minimal()#only 1 factor remaining after removing Measurement.Method NAs, so we ignore this too


library(rsample)
set.seed(123)

data_split_C6H5_CH3 <- initial_split(C6H5_CH3_data, prop = 0.9)

train_C6H5_CH3_df <- training(data_split_C6H5_CH3)
validation_C6H5_CH3_df <- testing(data_split_C6H5_CH3)

#levels(train_C6H5_CH3_df$Air.Quality.Station.Type)
#levels(train_C6H5_CH3_df$Air.Quality.Station.Area)
#levels(train_C6H5_CH3_df$Main.Emission.Sources)
#levels(train_C6H5_CH3_df$Measurement.Method)
#levels(train_C6H5_CH3_df$Year)
#levels(train_C6H5_CH3_df$Month)
print(unique(train_C6H5_CH3_df$Air.Quality.Station.Type))
print(unique(train_C6H5_CH3_df$Air.Quality.Station.Area))
print(unique(train_C6H5_CH3_df$Main.Emission.Sources))
print(unique(train_C6H5_CH3_df$Measurement.Method))
print(unique(train_C6H5_CH3_df$Year))
print(unique(train_C6H5_CH3_df$Month))


model_formula_C6H5_CH3 <- Value ~ 
  Year + Month + Altitude +
  #Air.Quality.Station.Type + Air.Quality.Station.Area + 
  #Inlet.Height + Kerb.Distance +
  Building.Distance
  #Main.Emission.Sources +
  #Measurement.Method


lm_model_C6H5_CH3 <- lm(model_formula_C6H5_CH3, data = train_C6H5_CH3_df)

summary(lm_model_C6H5_CH3)

original_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(lm_model_C6H5_CH3)
par(original_par)




validation_C6H5_CH3_df$predicted <- predict(lm_model_C6H5_CH3, newdata = validation_C6H5_CH3_df)

rmse_metrics_C6H5_CH3 <- rmse(validation_C6H5_CH3_df$Value, validation_C6H5_CH3_df$predicted)
print(paste("RMSE (Metrics package):", rmse_metrics_C6H5_CH3))

alias(lm_model_C6H5_CH3)
colSums(is.na(C6H5_CH3_data))


#########################################################################
#GWR

C6H5_CH3_data <- C6H5_CH3_data %>%
  mutate(
    log_KerbDistance = log1p(`Kerb.Distance`),
    log_InletHeight = log1p(`Inlet.Height`),
    log_BuildingDistance = log1p(`Building.Distance`)
  )



model_formula_C6H5_CH3_gwr <- Value ~ 
  Year + Month + Altitude +
  #Air.Quality.Station.Type + Air.Quality.Station.Area + 
  #log_InletHeight + log_KerbDistance + 
  log_BuildingDistance 
  #Main.Emission.Sources


set.seed(123)

data_split_C6H5_CH3_gwr <- initial_split(C6H5_CH3_data, prop = 0.9)

train_C6H5_CH3_gwr_df <- training(data_split_C6H5_CH3_gwr)
validation_C6H5_CH3_gwr_df <- testing(data_split_C6H5_CH3_gwr)

train_C6H5_CH3_gwr_df <- as.data.frame(train_C6H5_CH3_gwr_df)

coordinates(train_C6H5_CH3_gwr_df) <- ~Longitude + Latitude

proj4string(train_C6H5_CH3_gwr_df) <- CRS("+init=epsg:4326")
train_C6H5_CH3_gwr_df_proj <- spTransform(train_C6H5_CH3_gwr_df, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))


bw_C6H5_CH3_cv_bisq <- bw.gwr(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, approach = "CV", kernel = "bisquare", adaptive = TRUE)
cat("Optimal bandwidth (Adaptive):", bw_C6H5_CH3_cv_bisq, "\n")

gwr_model_C6H5_CH3_cv_bisq <- gwr.basic(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, bw = bw_C6H5_CH3_cv_bisq, kernel = "bisquare", adaptive = TRUE)
print(gwr_model_C6H5_CH3_cv_bisq)





validation_C6H5_CH3_gwrdf <- as.data.frame(validation_C6H5_CH3_gwr_df)

coordinates(validation_C6H5_CH3_gwrdf) <- ~Longitude + Latitude

proj4string(validation_C6H5_CH3_gwrdf) <- CRS("+init=epsg:4326")

validation_proj_C6H5_CH3 <- spTransform(validation_C6H5_CH3_gwrdf, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))



X_valid_C6H5_CH3 <- model.matrix(model_formula_C6H5_CH3_gwr, data = as.data.frame(validation_proj_C6H5_CH3))

gwr_results_C6H5_CH3_cv_bisq <- gwr_model_C6H5_CH3_cv_bisq$SDF

cat("X_valid_C6H5_CH3 column names:\n")
print(colnames(X_valid_C6H5_CH3))
cat("gwr_results_C6H5_CH3_cv_bisq@data column names:\n")
print(names(gwr_results_C6H5_CH3_cv_bisq@data))

predicted_values_C6H5_CH3 <- numeric(nrow(validation_proj_C6H5_CH3))

for (i in 1:nrow(validation_proj_C6H5_CH3)) {
  
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj_C6H5_CH3)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  coef_names <- colnames(X_valid_C6H5_CH3)  
  local_coef <- numeric(length(coef_names))
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    
    if (coef_name %in% names(gwr_results_C6H5_CH3_cv_bisq@data)) {
      local_coef[j] <- gwr_results_C6H5_CH3_cv_bisq@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results_C6H5_CH3_cv_bisq@data"))
    }
  }
  
  predicted_values_C6H5_CH3[i] <- X_valid_C6H5_CH3[i, ] %*% local_coef
}

actual_values_C6H5_CH3 <- validation_proj_C6H5_CH3$Value

# Calculate RMSE
rmse_C6H5_CH3_cv_bisq <- sqrt(mean((actual_values_C6H5_CH3 - predicted_values_C6H5_CH3)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_C6H5_CH3_cv_bisq, "\n")





##############################################################################

gwr_results_sf_C6H5_CH3 <- st_as_sf(gwr_model_C6H5_CH3_cv_bisq$SDF)
names(gwr_results_sf_C6H5_CH3)
gwr_results_sf_transformed_C6H5_CH3 <- st_transform(gwr_results_sf_C6H5_CH3, st_crs(ireland_counties))

gwr_results_with_regions_C6H5_CH3 <- st_join(gwr_results_sf_transformed_C6H5_CH3, ireland_counties, join = st_within)

aggregated_results_C6H5_CH3 <- gwr_results_with_regions_C6H5_CH3 %>%
  st_drop_geometry() %>% 
  group_by(name) %>% 
  summarise(
    mean_local_R2 = mean(Local_R2, na.rm = TRUE),
    mean_yhat = mean(yhat, na.rm = TRUE)
  )
ireland_counties_with_results_C6H5_CH3 <- ireland_counties %>%
  left_join(aggregated_results_C6H5_CH3, by = "name")

ggplot(data = ireland_counties_with_results_C6H5_CH3) +
  geom_sf(aes(fill = mean_yhat), color = "white") + 
  geom_sf(data = gwr_results_sf_C6H5_CH3, aes(fill = yhat),shape = 21,color = "black",size = 3,show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Local Predictions") + 
  labs(title = "Mean GWR Localised Prediction by County in Ireland") +
  theme_minimal()


ggplot(data = ireland_counties_with_results_C6H5_CH3) +
  geom_sf(aes(fill = mean_local_R2), color = "white") + 
  geom_sf(data = gwr_results_sf_C6H5_CH3, aes(fill = Local_R2),shape = 21,color = "black",size = 3,show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Local R2") + 
  labs(title = "Mean GWR Localised R2 Value by County in Ireland") +
  theme_minimal()


#######################################################################################################
bw_C6H5_CH3_AICc_bisq <- bw.gwr(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, approach = "AICc", kernel = "bisquare", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H5_CH3_AICc_bisq, "\n")

gwr_model_C6H5_CH3_AICc_bisq <- gwr.basic(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, bw = bw_C6H5_CH3_AICc_bisq, kernel = "bisquare", adaptive = TRUE)

print(gwr_model_C6H5_CH3_AICc_bisq)

#######################################################################################################

bw_C6H5_CH3_CV_gaus <- bw.gwr(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, approach = "CV", kernel = "gaussian", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H5_CH3_CV_gaus, "\n")

gwr_model_C6H5_CH3_CV_gaus <- gwr.basic(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, bw = bw_C6H5_CH3_CV_gaus, kernel = "gaussian", adaptive = TRUE)

print(gwr_model_C6H5_CH3_CV_gaus)



gwr_results_C6H5_CH3_cv_gaus <- gwr_model_C6H5_CH3_CV_gaus$SDF
predicted_values_C6H5_CH3_gaus <- numeric(nrow(validation_proj_C6H5_CH3))
for (i in 1:nrow(validation_proj_C6H5_CH3)) {
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj_C6H5_CH3)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  coef_names <- colnames(X_valid_C6H5_CH3)  
  local_coef <- numeric(length(coef_names))
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    
    if (coef_name %in% names(gwr_results_C6H5_CH3_cv_gaus@data)) {
      local_coef[j] <- gwr_results_C6H5_CH3_cv_gaus@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results_C6H5_CH3_cv_gaus@data"))
    }
  }
  
  predicted_values_C6H5_CH3_gaus[i] <- X_valid_C6H5_CH3[i, ] %*% local_coef
}

# Calculate RMSE
rmse_C6H5_CH3_cv_gaus <- sqrt(mean((actual_values_C6H5_CH3 - predicted_values_C6H5_CH3_gaus)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_C6H5_CH3_cv_gaus, "\n")

#######################################################################################################
bw_C6H5_CH3_AICc_gaus <- bw.gwr(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H5_CH3_AICc_gaus, "\n")

gwr_model_C6H5_CH3_AICc_gaus <- gwr.basic(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, bw = bw_C6H5_CH3_AICc_gaus, kernel = "gaussian", adaptive = TRUE)

print(gwr_model_C6H5_CH3_AICc_gaus)

#######################################################################################################

bw_C6H5_CH3_CV_exp <- bw.gwr(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, approach = "CV", kernel = "exponential", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H5_CH3_CV_exp, "\n")

gwr_model_C6H5_CH3_CV_exp <- gwr.basic(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, bw = bw_C6H5_CH3_CV_exp, kernel = "exponential", adaptive = TRUE)

print(gwr_model_C6H5_CH3_CV_exp)



gwr_results_C6H5_CH3_cv_exp <- gwr_model_C6H5_CH3_CV_exp$SDF
predicted_values_C6H5_CH3_exp <- numeric(nrow(validation_proj_C6H5_CH3))
for (i in 1:nrow(validation_proj_C6H5_CH3)) {
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj_C6H5_CH3)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  
  coef_names <- colnames(X_valid_C6H5_CH3)  
  local_coef <- numeric(length(coef_names))
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    
    if (coef_name %in% names(gwr_results_C6H5_CH3_cv_exp@data)) {
      local_coef[j] <- gwr_results_C6H5_CH3_cv_exp@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results_C6H5_CH3_cv_exp@data"))
    }
  }
  
  predicted_values_C6H5_CH3_exp[i] <- X_valid_C6H5_CH3[i, ] %*% local_coef
}

# Calculate RMSE
rmse_C6H5_CH3_cv_exp <- sqrt(mean((actual_values_C6H5_CH3 - predicted_values_C6H5_CH3_exp)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_C6H5_CH3_cv_exp, "\n")

#######################################################################################################
bw_C6H5_CH3_AICc_exp <- bw.gwr(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, approach = "AICc", kernel = "exponential", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_C6H5_CH3_AICc_exp, "\n")

gwr_model_C6H5_CH3_AICc_exp <- gwr.basic(model_formula_C6H5_CH3_gwr, data = train_C6H5_CH3_gwr_df_proj, bw = bw_C6H5_CH3_AICc_exp, kernel = "exponential", adaptive = TRUE)

print(gwr_model_C6H5_CH3_AICc_exp)