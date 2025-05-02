PM2_5_data <- merged_data_final[merged_data_final$"Air Pollutant" == "PM2.5", ]

str(PM2_5_data)

#starting with check correlation among the numerical attributes
print(cor(PM2_5_data[, c("Value", "Inlet.Height", "Kerb.Distance", "Building.Distance", "Altitude")]))
#Building Distance and Kerb Distance has high correlation ~0.7
#rest is fine, so we exclude Building.Distance for now

#refactor all non numerical columns
PM2_5_data$Year <- factor(PM2_5_data$Year)
PM2_5_data$Month <- factor(PM2_5_data$Month)
PM2_5_data$Hour <- factor(PM2_5_data$Hour)
PM2_5_data$Air.Quality.Station.Type <- factor(PM2_5_data$Air.Quality.Station.Type)
PM2_5_data$Air.Quality.Station.Area <- factor(PM2_5_data$Air.Quality.Station.Area)
PM2_5_data$Main.Emission.Sources <- factor(PM2_5_data$Main.Emission.Sources)
PM2_5_data$Measurement.Method <- factor(PM2_5_data$Measurement.Method)
PM2_5_data$Measurement.Type <- factor(PM2_5_data$Measurement.Type)


#now moving on the checking the factored variables via their box plots
ggplot(PM2_5_data, aes(x = `Month`, y=`Value`, group=`Month`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Month", x = "Month", y="Value") +
  theme_minimal()

ggplot(PM2_5_data, aes(x = `Year`, y=`Value`, group=`Year`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Year", x = "Year", y="Value") +
  theme_minimal()#not any clear pattern or deviation but we keep it for now

ggplot(PM2_5_data, aes(x = `Hour`, y=`Value`, group=`Hour`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Hour", x = "Hour", y="Value") +
  theme_minimal()#no hourly information, exclude this

ggplot(PM2_5_data, aes(x = `Air.Quality.Station.Type`, y=`Value`, group=`Air.Quality.Station.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Type", x = "Air.Quality.Station.Type", y="Value") +
  theme_minimal()

ggplot(PM2_5_data, aes(x = `Air.Quality.Station.Area`, y=`Value`, group=`Air.Quality.Station.Area`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Air.Quality.Station.Area", x = "Air.Quality.Station.Area", y="Value") +
  theme_minimal()

ggplot(PM2_5_data, aes(x = `Main.Emission.Sources`, y=`Value`, group=`Main.Emission.Sources`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Main.Emission.Sources", x = "Main.Emission.Sources", y="Value") +
  theme_minimal()

ggplot(PM2_5_data, aes(x = `Measurement.Method`, y=`Value`, group=`Measurement.Method`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Method", x = "Measurement.Method", y="Value") +
  theme_minimal()


ggplot(PM2_5_data, aes(x = `Measurement.Type`, y=`Value`, group=`Measurement.Type`)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of Measurement.Type", x = "Measurement.Type", y="Value") +
  theme_minimal()#only 1 factor remaining after removing Measurement.Method NAs, so we ignore this too


#All non numerical = columns have more than 1 factor, so we will keep them all

PM2_5_data <- PM2_5_data[!is.na(PM2_5_data$Measurement.Method), ] #removing NAs
#factor and check boxplot again

library(rsample)
set.seed(123)

# Create a data split object
data_split_PM2_5 <- initial_split(PM2_5_data, prop = 0.9)

# Extract the training and testing (validation) datasets
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
  Year + Month + Altitude +
  Air.Quality.Station.Type + Air.Quality.Station.Area + 
  Inlet.Height + Kerb.Distance +
  Main.Emission.Sources +
  Measurement.Method


lm_model_PM2_5 <- lm(model_formula_PM2_5, data = train_PM2_5_df)

summary(lm_model_PM2_5)

plot(lm_model_PM2_5)


validation_PM2_5_df$predicted <- predict(lm_model_PM2_5, newdata = validation_PM2_5_df)
str(validation_PM2_5_df)

library(Metrics)

# Assume 'actual_values' and 'predicted_values' are your vectors
rmse_metrics_PM2_5 <- rmse(validation_PM2_5_df$Value, validation_PM2_5_df$predicted)
print(paste("RMSE (Metrics package):", rmse_metrics_PM2_5))

alias(lm_model_PM2_5)
colSums(is.na(PM2_5_data))


#########################################################################
#GWR

PM2_5_data <- PM2_5_data %>%
  mutate(
    log_KerbDistance = log1p(`Kerb.Distance`),
    log_InletHeight = log1p(`Inlet.Height`)
  )



model_formula_PM2_5_gwr <- Value ~ 
  Year + Month + Altitude +
  #Air.Quality.Station.Type + Air.Quality.Station.Area + 
  log_InletHeight + log_KerbDistance #+
  #Main.Emission.Sources #+
  #Measurement.Method


set.seed(123)

data_split_PM2_5_gwr <- initial_split(PM2_5_data, prop = 0.9)

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

# For parallel (built-in):
# cl <- makeCluster(num_cores, type = "PSOCK") # Or "FORK" on Unix-like systems

cat("Using", getDoParWorkers(), "cores for parallel processing.\n")

# 3. Run bw.gwr with parallel.bw = TRUE
# Ensure your formula and data are correct and clean of NAs/Infs as discussed before
tryCatch({
  bw_opt_PM2_5 <- bw.gwr(model_formula_PM2_5_gwr,
                         data = train_PM2_5_gwr_df_proj,
                         approach = "CV",
                         kernel = "bisquare",
                         adaptive = FALSE,
                         parallel.method = "cluster" # Use the 'cluster' method
  )
  print(bw_opt_PM2_5)
}, error = function(e) {
  cat("An error occurred during bandwidth selection:", e$message, "\n")
})


# 4. Stop the parallel cluster when done
stopCluster(cl)
registerDoSEQ()

#bw_opt_PM2_5 <- bw.gwr(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, approach = "CV", kernel = "bisquare", adaptive = FALSE)
#cat("Optimal bandwidth (fixed):", bw_opt_PM2_5, "\n")

gwr_model_PM2_5 <- gwr.basic(model_formula_PM2_5_gwr, data = train_PM2_5_gwr_df_proj, bw = bw_opt_PM2_5, kernel = "bisquare", adaptive = FALSE)

print(gwr_model_PM2_5)

