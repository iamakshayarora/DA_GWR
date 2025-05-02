#plotting residuals and performing spatial variation check
merged_data_final$residuals <- residuals(lm_model)
#aggregate data by mean residuals
aggregated_data <- merged_data_final %>%
  group_by(Longitude, Latitude) %>%
  summarise(
    mean_residual = mean(residuals, na.rm = TRUE),
    count      = n()
  ) %>%
  ungroup()
#plotting residuals
aggregated_data <- merged_data_final %>%
  group_by(Longitude, Latitude) %>%
  summarise(
    mean_residual = mean(residuals, na.rm = TRUE),
    count      = n()
  ) %>%
  ungroup()
#checking spatial autocorrelation
library(spdep)

# 1. Create a matrix of coordinates
coords <- cbind(aggregated_data$Longitude, aggregated_data$Latitude)

# 2. Find 4 nearest neighbors for each point
# (Adjust k to your needs)
knn <- knearneigh(coords, k = 4)

# 3. Convert knn object to a neighbors list
nb <- knn2nb(knn)

# 4. Convert neighbors list to a listw object (spatial weights)
listw_obj <- nb2listw(nb, style = "W")  # style="W" for row-standardized

# Example: if your coordinates are in degrees and you assume 0.5 degrees
# is your threshold for neighbors (this is arbitrary; choose carefully!)
dist_threshold <- 0.5

dist_nb <- dnearneigh(coords, 0, dist_threshold)
listw_obj <- nb2listw(dist_nb, style = "W")
moran_test <- moran.test(aggregated_data$mean_residual, listw_obj)
print(moran_test)