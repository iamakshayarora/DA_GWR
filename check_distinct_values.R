str(merged_data)

print(unique(merged_data$Pollutant))
print(unique(merged_data[, c("Pollutant", "Air Pollutant")]), n = Inf)

#distinct Air Pollutants with count
print(merged_data %>%
  group_by(`Air Pollutant`) %>%
  summarise(count = n()) %>%
  arrange(desc(count)), n = Inf)

#null records in Value column
print(sum(is.na(merged_data[["Value"]]) | merged_data[["Value"]] == "NA"))

#null records in Air Pollutant column
print(sum(is.na(merged_data[["Air Pollutant"]]) | merged_data[["Air Pollutant"]] == "NA"))


#distinct Station Names with count
print(merged_data %>%
        group_by(`Air Quality Station Name`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)

#distinct Inlet Height with count
print(merged_data %>%
        group_by(`Inlet Height`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)

#null records in Inlet Height column
print(sum(is.na(merged_data[["Inlet Height"]]) | merged_data[["Inlet Height"]] == "NA"))


#distinct Building Distance with count
print(merged_data %>%
        group_by(`Building Distance`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Kerb Distance with count
print(merged_data %>%
        group_by(`Kerb Distance`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Distance Source with count
print(merged_data %>%
        group_by(`Distance Source`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)

#distinct Main Emission Sources with count
print(merged_data %>%
        group_by(`Main Emission Sources`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Measurement Type with count
print(merged_data %>%
        group_by(`Measurement Type`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Measurement Method with count
print(merged_data %>%
        group_by(`Measurement Method`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Measurement Equipment with count
print(merged_data %>%
        group_by(`Measurement Equipment`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Sampling Method with count
print(merged_data %>%
        group_by(`Sampling Method`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Analytical Technique with count
print(merged_data %>%
        group_by(`Analytical Technique`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)


#distinct Validity with count
print(merged_data %>%
        group_by(`Validity`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)), n = Inf)

#distinct Value with count
print(merged_data %>%
        group_by(`Value`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)))



merged_data_final <- merged_data %>% select(-c("Verification", "ResultTime","FkObservationLog", "Samplingpoint_ID", "B-G Namespace", "Year", "Air Quality Network", "Air Quality Network Name", "Timezone", "Air Quality Station EoI Code", "Operational Activity End", "Heating Emissions", "Heating Emissions Unit", "Mobile", "Traffic Emissions", "Traffic Emissions Unit", "Industrial Emissions", "Industrial Emissions Unit", "Municipality", "Dispersion Local", "Dispersion Regional", "Distance Junction", "Distance Junction Unit", "Heavy Duty Fraction", "Height Facades", "Street Width", "Traffic Speed", "Traffic Volume", "Process Activity End", "Other Measurement Method","Other Measurement Equipment", "Demonstration Report", "Other Sampling Method", "Other Analytical Technique", "Documentation", "QA Report", "Duration", "Duration Unit", "Cadence", "Cadence Unit", "Source Data URL", "Imported"))

merged_data_final <- merged_data_final %>% filter(Value != "-999") #removing missing values(-999)

str(merged_data_final)
summary(merged_data_final)


#distinct Value with count
print(merged_data_final %>%
        group_by(`Value`) %>%
        summarise(count = n()) %>%
        arrange(desc(count)))
