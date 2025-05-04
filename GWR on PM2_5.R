PM2_5_data <- merged_data_final[merged_data_final$"Air Pollutant" == "PM2.5", ]
nrow(PM2_5_data)
str(PM2_5_data)

#distinct Years with count
print(PM2_5_data %>%
        group_by(`Year`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)

PM2_5_data_2020 <- PM2_5_data[PM2_5_data$"Year" == "2020", ]
nrow(PM2_5_data_2020)

#starting with check correlation among the numerical attributes
print(cor(PM2_5_data_2020[, c("Value", "Inlet.Height", "Kerb.Distance", "Building.Distance", "Altitude")]))
#Building Distance and Kerb Distance has high correlation ~0.7
#rest is fine, so we exclude Building.Distance for now

#refactor all non numerical columns
PM2_5_data_2020$Year <- factor(PM2_5_data_2020$Year)
PM2_5_data_2020$Month <- factor(PM2_5_data_2020$Month)
PM2_5_data_2020$Hour <- factor(PM2_5_data_2020$Hour)
PM2_5_data_2020$Air.Quality.Station.Type <- factor(PM2_5_data_2020$Air.Quality.Station.Type)
PM2_5_data_2020$Air.Quality.Station.Area <- factor(PM2_5_data_2020$Air.Quality.Station.Area)
PM2_5_data_2020$Main.Emission.Sources <- factor(PM2_5_data_2020$Main.Emission.Sources)
PM2_5_data_2020$Measurement.Method <- factor(PM2_5_data_2020$Measurement.Method)
PM2_5_data_2020$Measurement.Type <- factor(PM2_5_data_2020$Measurement.Type)


#now moving on the checking the factored variables via their box plots
ggplot(PM2_5_data_2020, aes(x = `Month`, y=`Value`, group=`Month`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Month", x = "Month", y="Value") +
  theme_minimal()

ggplot(PM2_5_data, aes(x = `Year`, y=`Value`, group=`Year`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Year", x = "Year", y="Value") +
  theme_minimal()#not any clear pattern or deviation but we keep it for now

ggplot(PM2_5_data_2020, aes(x = `Hour`, y=`Value`, group=`Hour`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Hour", x = "Hour", y="Value") +
  theme_minimal()#no hourly information, exclude this

ggplot(PM2_5_data_2020, aes(x = `Air.Quality.Station.Type`, y=`Value`, group=`Air.Quality.Station.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Type", x = "Air.Quality.Station.Type", y="Value") +
  theme_minimal()

ggplot(PM2_5_data_2020, aes(x = `Air.Quality.Station.Area`, y=`Value`, group=`Air.Quality.Station.Area`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Area", x = "Air.Quality.Station.Area", y="Value") +
  theme_minimal()

ggplot(PM2_5_data_2020, aes(x = `Main.Emission.Sources`, y=`Value`, group=`Main.Emission.Sources`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Main.Emission.Sources", x = "Main.Emission.Sources", y="Value") +
  theme_minimal()

ggplot(PM2_5_data_2020, aes(x = `Measurement.Method`, y=`Value`, group=`Measurement.Method`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Method", x = "Measurement.Method", y="Value") +
  theme_minimal()


ggplot(PM2_5_data_2020, aes(x = `Measurement.Type`, y=`Value`, group=`Measurement.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Type", x = "Measurement.Type", y="Value") +
  theme_minimal()


#All non numerical columns have more than 1 factor, so we will keep them all

#PM2_5_data <- PM2_5_data[!is.na(PM2_5_data$Measurement.Method), ] #removing NAs
#factor and check boxplot again

library(rsample)
set.seed(123)

data_split_PM2_5 <- initial_split(PM2_5_data_2020, prop = 0.9)

train_PM2_5_df <- training(data_split_PM2_5)
validation_PM2_5_df <- testing(data_split_PM2_5)

#levels(train_PM2_5_df$Air.Quality.Station.Type)
#levels(train_PM2_5_df$Air.Quality.Station.Area)
#levels(train_PM2_5_df$Main.Emission.Sources)
#levels(train_PM2_5_df$Measurement.Method)
#levels(train_PM2_5_df$Year)
#levels(train_PM2_5_df$Month)
print(unique(train_PM2_5_df$Air.Quality.Station.Type))
print(unique(train_PM2_5_df$Air.Quality.Station.Area))
print(unique(train_PM2_5_df$Main.Emission.Sources))
print(unique(train_PM2_5_df$Measurement.Method))
print(unique(train_PM2_5_df$Year))
print(unique(train_PM2_5_df$Month))


model_formula_PM2_5 <- Value ~ 
  Month + Altitude +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + Kerb.Distance + Building.Distance
  #Main.Emission.Sources +
 # Measurement.Method


lm_model_PM2_5 <- lm(model_formula_PM2_5, data = train_PM2_5_df)

summary(lm_model_PM2_5)

original_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(lm_model_PM2_5)
par(original_par)



validation_PM2_5_df$predicted <- predict(lm_model_PM2_5, newdata = validation_PM2_5_df)
str(validation_PM2_5_df)

library(Metrics)

rmse_metrics_PM2_5 <- rmse(validation_PM2_5_df$Value, validation_PM2_5_df$predicted)
print(paste("RMSE (Metrics package):", rmse_metrics_PM2_5))

alias(lm_model_PM2_5)
colSums(is.na(PM2_5_data))


#########################################################################
#GWR

PM2_5_data_2020 <- PM2_5_data_2020 %>%
  mutate(
    log_KerbDistance = log1p(`Kerb.Distance`),
    log_InletHeight = log1p(`Inlet.Height`),
    log_BuildingDistance = log1p(`Building.Distance`)
  )



model_formula_PM2_5_gwr <- Value ~ 
  Month + Altitude +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  log_InletHeight + log_KerbDistance + log_BuildingDistance
  #Main.Emission.Sources +
  #Measurement.Method


set.seed(123)

data_split_PM2_5_gwr <- initial_split(PM2_5_data_2020, prop = 0.9)

train_PM2_5_gwr_df <- training(data_split_PM2_5_gwr)
validation_PM2_5_gwr_df <- testing(data_split_PM2_5_gwr)

train_PM2_5_gwr_df <- as.data.frame(train_PM2_5_gwr_df)

coordinates(train_PM2_5_gwr_df) <- ~Longitude + Latitude

proj4string(train_PM2_5_gwr_df) <- CRS("+init=epsg:4326")
train_PM2_5_gwr_df_proj <- spTransform(train_PM2_5_gwr_df, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))


library(doParallel)
num_cores <- detectCores() - 1
print(num_cores)
cl <- makeCluster(num_cores)
registerDoParallel(cl)


cat("Using", getDoParWorkers(), "cores for parallel processing.\n")

tryCatch({
  bw_opt_PM2_5_cv_bisq <- bw.gwr(model_formula_PM2_5_gwr,
                         data = train_PM2_5_gwr_df_proj,
                         approach = "CV",
                         kernel = "bisquare",
                         adaptive = TRUE,
                         parallel.method = "cluster" 
  )
  print(bw_opt_PM2_5)
}, error = function(e) {
  cat("An error occurred during bandwidth selection:", e$message, "\n")
})


stopCluster(cl)
registerDoSEQ()

#bw_opt_PM2_5 <- bw.gwr(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, approach = "CV", kernel = "bisquare", adaptive = FALSE)
#cat("Optimal bandwidth (fixed):", bw_opt_PM2_5, "\n")

gwr_model_PM2_5_cv_bisq <- gwr.basic(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, bw = bw_opt_PM2_5_cv_bisq, kernel = "bisquare", adaptive = TRUE, parallel.method = "cluster")

print(gwr_model_PM2_5_cv_bisq)




validation_PM2_5_gwrdf <- as.data.frame(validation_PM2_5_gwr_df)

coordinates(validation_PM2_5_gwrdf) <- ~Longitude + Latitude

proj4string(validation_PM2_5_gwrdf) <- CRS("+init=epsg:4326")


validation_proj_PM2_5 <- spTransform(validation_PM2_5_gwrdf, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))




X_valid_PM2_5 <- model.matrix(model_formula_PM2_5_gwr, data = as.data.frame(validation_proj_PM2_5))

gwr_results_PM2_5_cv_bisq <- gwr_model_PM2_5_cv_bisq$SDF

cat("X_valid_PM2_5 column names:\n")
print(colnames(X_valid_PM2_5))
cat("gwr_results_PM2_5_cv_bisq@data column names:\n")
print(names(gwr_results_PM2_5_cv_bisq@data))

predicted_values_PM2_5_cv_bisq <- numeric(nrow(validation_proj_PM2_5))


for (i in 1:nrow(validation_proj_PM2_5)) {
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj_PM2_5)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  coef_names <- colnames(X_valid_PM2_5)  
  local_coef <- numeric(length(coef_names))
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    
    if (coef_name %in% names(gwr_results_PM2_5_cv_bisq@data)) {
      local_coef[j] <- gwr_results_PM2_5_cv_bisq@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results_PM2_5_cv_bisq@data"))
    }
  }
  
  predicted_values_PM2_5_cv_bisq[i] <- X_valid_PM2_5[i, ] %*% local_coef
}

actual_values_PM2_5 <- validation_proj_PM2_5$Value

# Calculate RMSE
rmse_PM2_5_cv_bisq <- sqrt(mean((actual_values_PM2_5 - predicted_values_PM2_5_cv_bisq)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_PM2_5_cv_bisq, "\n")



#######################################################################################################

bw_PM2_5_CV_gaus <- bw.gwr(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, approach = "CV", kernel = "gaussian", adaptive = TRUE, parallel.method = "cluster")
cat("Optimal bandwidth (fixed):", bw_PM2_5_CV_gaus, "\n")

gwr_model_PM2_5_CV_gaus <- gwr.basic(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, bw = bw_PM2_5_CV_gaus, kernel = "gaussian", adaptive = TRUE, parallel.method = "cluster")

print(gwr_model_PM2_5_CV_gaus)



gwr_results_PM2_5_cv_gaus <- gwr_model_PM2_5_CV_gaus$SDF
predicted_values_PM2_5_gaus <- numeric(nrow(validation_proj_PM2_5))
for (i in 1:nrow(validation_proj_PM2_5)) {
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj_PM2_5)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  coef_names <- colnames(X_valid_PM2_5) 
  local_coef <- numeric(length(coef_names))
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name  
    }
    
    if (coef_name %in% names(gwr_results_PM2_5_cv_gaus@data)) {
      local_coef[j] <- gwr_results_PM2_5_cv_gaus@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results_PM2_5_cv_gaus@data"))
    }
  }
  
  predicted_values_PM2_5_gaus[i] <- X_valid_PM2_5[i, ] %*% local_coef
}

# Calculate RMSE
rmse_PM2_5_cv_gaus <- sqrt(mean((actual_values_PM2_5 - predicted_values_PM2_5_gaus)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_PM2_5_cv_gaus, "\n")


#######################################################################################################

bw_PM2_5_CV_exp <- bw.gwr(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, approach = "CV", kernel = "exponential", adaptive = TRUE, parallel.method = "cluster")
cat("Optimal bandwidth (fixed):", bw_PM2_5_CV_exp, "\n")

gwr_model_PM2_5_CV_exp <- gwr.basic(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, bw = bw_PM2_5_CV_exp, kernel = "exponential", adaptive = TRUE, parallel.method = "cluster")

print(gwr_model_PM2_5_CV_exp)



gwr_results_PM2_5_cv_exp <- gwr_model_PM2_5_CV_exp$SDF
predicted_values_PM2_5_exp <- numeric(nrow(validation_proj_PM2_5))
for (i in 1:nrow(validation_proj_PM2_5)) {
  distances <- spDistsN1(coordinates(test_proj), coordinates(validation_proj_PM2_5)[i, ], longlat = FALSE)
  nearest_idx <- which.min(distances)
  
  coef_names <- colnames(X_valid_PM2_5)  
  local_coef <- numeric(length(coef_names))
  
  for (j in seq_along(coef_names)) {
    coef_name <- coef_names[j]
    if (coef_name == "(Intercept)") {
      coef_name <- "Intercept"  
    } else {
      coef_name <- coef_name
    }
    
    if (coef_name %in% names(gwr_results_PM2_5_cv_exp@data)) {
      local_coef[j] <- gwr_results_PM2_5_cv_exp@data[nearest_idx, coef_name]
    } else {
      stop(paste("Coefficient", coef_name, "not found in gwr_results_PM2_5_cv_exp@data"))
    }
  }
  
  predicted_values_PM2_5_exp[i] <- X_valid_PM2_5[i, ] %*% local_coef
}

# Calculate RMSE
rmse_PM2_5_cv_exp <- sqrt(mean((actual_values_PM2_5 - predicted_values_PM2_5_exp)^2, na.rm = TRUE))
cat("RMSE for the validation set:", rmse_PM2_5_cv_exp, "\n")





###################################

gwr_results_PM2_5_CV_exp_sf <- st_as_sf(gwr_model_PM2_5_CV_exp$SDF)
names(gwr_results_PM2_5_CV_exp_sf)
ggplot() +
  geom_sf(data = ireland_map, fill = "lightgray", color = "white") +
  geom_sf(data = gwr_results_PM2_5_CV_exp_sf, aes(color = Local_R2), size = 2) +
  scale_color_viridis_c() +
  labs(title = "GWR Local R-squared", color = "Local R-squared") +
  theme_minimal()

#install.packages("rnaturalearthhires")
#install.packages("devtools")
#devtools::install_github("ropensci/rnaturalearthhires")
#library(rnaturalearthhires)
#st_crs(gwr_results_PM2_5_CV_exp_sf)
#st_crs(ireland_counties)
gwr_results_PM2_5_CV_exp_sf_transformed <- st_transform(gwr_results_PM2_5_CV_exp_sf, st_crs(ireland_counties))

ireland_counties <- ne_states(country = "Ireland", returnclass = "sf")
ireland_boundary <- ne_countries(country = "Ireland", scale = "medium", returnclass = "sf")
gwr_results_with_regions <- st_join(gwr_results_PM2_5_CV_exp_sf_transformed, ireland_counties, join = st_within)

aggregated_results <- gwr_results_with_regions %>%
  st_drop_geometry() %>% 
  group_by(name) %>% 
  summarise(
    mean_local_R2 = mean(Local_R2, na.rm = TRUE),
    mean_yhat = mean(yhat, na.rm = TRUE)
  )
ireland_counties_with_results <- ireland_counties %>%
  left_join(aggregated_results, by = "name")

ggplot(data = ireland_counties_with_results) +
  geom_sf(aes(fill = mean_yhat), color = "white") + 
  geom_sf(data = gwr_results_PM2_5_CV_exp_sf, aes(fill = yhat),shape = 21,color = "black",size = 3,show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Local Predictions") + 
  labs(title = "Mean GWR Localised Prediction by County in Ireland") +
  theme_minimal()

#######################################################################################################
bw_PM2_5_AICc_exp <- bw.gwr(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, approach = "AICc", kernel = "exponential", adaptive = TRUE, parallel.method = "cluster")
cat("Optimal bandwidth (fixed):", bw_PM2_5_AICc_exp, "\n")

gwr_model_PM2_5_AICc_exp <- gwr.basic(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, bw = bw_PM2_5_AICc_exp, kernel = "exponential", adaptive = TRUE, parallel.method = "cluster")

print(gwr_model_PM2_5_AICc_exp)