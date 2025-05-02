install.packages("GWmodel")
install.packages("sp")

library(GWmodel)
library(sp)
library(dplyr)

# Assuming O3_data is already created as the subset of merged_data_final for O3
# Check that geographic columns are available:
head(O3_data[, c("Longitude", "Latitude", "Altitude")])

# Convert O3_data to a SpatialPointsDataFrame using longitude and latitude
# (Altitude can be used later as a predictor if desired)
O3_df <- as.data.frame(O3_data)
summary(O3_df)

# Create log-transformed distance variables to reduce skewness
O3_df <- O3_df %>%
  mutate(
    log_BuildingDistance = log1p(`Building.Distance`),
    log_KerbDistance = log1p(`Kerb.Distance`),
    log_InletHeight = log1p(`Inlet.Height`)
  )

coordinates(O3_df) <- ~Longitude + Latitude
# Set the coordinate reference system (here we assume WGS84; change if necessary)
proj4string(O3_df) <- CRS("+init=epsg:4326")

O3_proj <- spTransform(O3_df, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))


# (Optional) Transform to a projected coordinate system for distance-based calculations:
# O3_df <- spTransform(O3_df, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))

# Define the model formula.
# Note: altitude is not used for spatial weighting but you can include it as a predictor if you believe it influences 'Value'
model_formula_gwr <- Value ~ Year + Month +
  #Air.Quality.Station.Type + Air.Quality.Station.Area +
  log_InletHeight + log_BuildingDistance + log_KerbDistance
  #Main.Emission.Sources + Measurement.Method
# If you want to include altitude as a predictor, add it (e.g., "+ Altitude")

# --------------------------
# BANDWIDTH SELECTION
# --------------------------
# Use cross-validation to choose the optimal bandwidth.
# Here we use a bisquare kernel. You could try other kernels (e.g., "gaussian")
bw_opt <- bw.gwr(model_formula_gwr, data = O3_proj, approach = "CV", kernel = "bisquare", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_opt, "\n")

# If you prefer adaptive bandwidth (which adjusts based on local point density), use adaptive = TRUE:
# bw_opt <- bw.gwr(model_formula, data = O3_df, approach = "CV", kernel = "bisquare", adaptive = TRUE)

# --------------------------
# FIT THE GWR MODEL
# --------------------------
# Fit the GWR model using the optimal bandwidth.
gwr_model <- gwr.basic(model_formula_gwr, data = O3_proj, bw = bw_opt, kernel = "bisquare", adaptive = TRUE)

# View a summary of the GWR model
print(gwr_model)

# --------------------------
# PREDICTION WITH THE GWR MODEL
# --------------------------
# The gwr.basic() function returns local parameter estimates and local R2 values.
# For predictions, you can extract the fitted values from gwr_model$SDF.
O3_proj$GWR_Predicted <- gwr_model$SDF$prediction

# --------------------------
# EVALUATION & COMPARISON
# --------------------------
# Compare the global linear model (lm_model_o3) with the GWR model.
# For example, compare AIC values:
aic_lm <- AIC(lm_model_o3)
aic_gwr <- gwr_model$results$AICc
cat("AIC for global linear model:", aic_lm, "\n")
cat("AICc for GWR model:", aic_gwr, "\n")

# You can also compare residual standard error or compute RMSE for both models.
# Here is an example RMSE calculation for the GWR model:
rmse_gwr <- sqrt(mean((O3_proj$Value - O3_proj$GWR_Predicted)^2, na.rm = TRUE))
cat("RMSE for GWR model:", rmse_gwr, "\n")

# Similarly, for the global linear model:
lm_predictions <- predict(lm_model_o3, newdata = as.data.frame(O3_proj))
rmse_lm <- sqrt(mean((O3_proj$Value - lm_predictions)^2, na.rm = TRUE))
cat("RMSE for global linear model:", rmse_lm, "\n")

# You can also compare local R2 from the GWR model (stored in gwr_model$SDF$Local_R2) 
# to understand how well the model fits locally.
summary(gwr_model$SDF$Local_R2)


gwr_r2 <- final_gwr_model$GW.diagnostic$R2        # Global R-squared
gwr_aicc <- final_gwr_model$GW.diagnostic$AICc    # AICc

lm_model_o3 <- lm(model_formula, data = O3_data)

lm_summary <- summary(lm_model_o3)
lm_r2 <- lm_summary$r.squared
lm_aic <- AIC(lm_model_o3)
# Compute AICc for fair comparison
n <- nrow(O3_data)
k <- length(coef(lm_model_o3))
lm_aicc <- lm_aic + (2 * k * (k + 1)) / (n - k - 1)