does_not_start_with_ie <- !startsWith(combined_data$Samplingpoint, "IE")
print(does_not_start_with_ie)

distinct_sampling_point <- unique(combined_data$Samplingpoint)
print(distinct_sampling_point)



library(dplyr)
library(stringr)


combined_data <- combined_data %>%
  mutate(Samplingpoint_ID = sub("^IE/", "", Samplingpoint)) 

merged_data <- combined_data %>%
  left_join(metadata, by = c("Samplingpoint_ID" = "Sampling Point Id"))


print(head(merged_data))


write.csv(head(merged_data, 100), "merged_data_sample.csv", row.names = FALSE)
