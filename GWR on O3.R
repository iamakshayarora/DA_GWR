install.packages("GWmodel")
install.packages("sp")

library(GWmodel)
library(sp)
library(dplyr)


head(O3_data[, c("Longitude", "Latitude", "Altitude")])


O3_df <- as.data.frame(O3_data)
summary(O3_df)

O3_df <- O3_df %>%
  mutate(
    log_BuildingDistance = log1p(`Building.Distance`),
    log_KerbDistance = log1p(`Kerb.Distance`),
    log_InletHeight = log1p(`Inlet.Height`)
  )

coordinates(O3_df) <- ~Longitude + Latitude

proj4string(O3_df) <- CRS("+init=epsg:4326")

O3_proj <- spTransform(O3_df, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"))


model_formula_gwr <- Value ~ Year + Month +
  #Air.Quality.Station.Type + Air.Quality.Station.Area +
  log_InletHeight + log_BuildingDistance + log_KerbDistance
  #Main.Emission.Sources + Measurement.Method

bw_opt <- bw.gwr(model_formula_gwr, data = O3_proj, approach = "CV", kernel = "bisquare", adaptive = TRUE)
cat("Optimal bandwidth (fixed):", bw_opt, "\n")


gwr_model <- gwr.basic(model_formula_gwr, data = O3_proj, bw = bw_opt, kernel = "bisquare", adaptive = TRUE)


print(gwr_model)


O3_proj$GWR_Predicted <- gwr_model$SDF$prediction


aic_lm <- AIC(lm_model_o3)
aic_gwr <- gwr_model$results$AICc
cat("AIC for global linear model:", aic_lm, "\n")
cat("AICc for GWR model:", aic_gwr, "\n")


rmse_gwr <- sqrt(mean((O3_proj$Value - O3_proj$GWR_Predicted)^2, na.rm = TRUE))
cat("RMSE for GWR model:", rmse_gwr, "\n")


lm_predictions <- predict(lm_model_o3, newdata = as.data.frame(O3_proj))
rmse_lm <- sqrt(mean((O3_proj$Value - lm_predictions)^2, na.rm = TRUE))
cat("RMSE for global linear model:", rmse_lm, "\n")


summary(gwr_model$SDF$Local_R2)


gwr_r2 <- final_gwr_model$GW.diagnostic$R2        
gwr_aicc <- final_gwr_model$GW.diagnostic$AICc    

lm_model_o3 <- lm(model_formula, data = O3_data)

lm_summary <- summary(lm_model_o3)
lm_r2 <- lm_summary$r.squared
lm_aic <- AIC(lm_model_o3)
n <- nrow(O3_data)
k <- length(coef(lm_model_o3))
lm_aicc <- lm_aic + (2 * k * (k + 1)) / (n - k - 1)