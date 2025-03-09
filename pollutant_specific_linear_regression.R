O3_data <- merged_data_final[merged_data_final$"Air Pollutant" == "O3", ]

cor_matrix <- cor(O3_data[, sapply(O3_data, is.numeric)], use = "complete.obs")
print(cor_matrix)


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

model_formula <- Value ~ 
  Year + Month +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + Kerb.Distance + Building.Distance +
  Main.Emission.Sources +
  Measurement.Method


lm_model_o3 <- lm(model_formula, data = O3_data)

summary(lm_model_o3)



alias(lm_model_o3)


vif(lm_model_o3)  # Values >5-10 indicate multicollinearity

set.seed(123) 
sample_indices <- sample(nrow(O3_data), 50000)
lm_sample_03 <- lm(model_formula, data = O3_data[sample_indices, ])
plot(lm_sample_03)

