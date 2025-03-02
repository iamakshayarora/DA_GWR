library(readr)
library(dplyr)

getwd()


folder_path <- "CSV"

csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

sample_data <- read_csv(csv_files[1], col_types = cols())


column_types <- spec(sample_data)

print(column_types)


combined_data <- bind_rows(lapply(csv_files, read_csv, col_types = column_types))

print(head(combined_data))


library(tidyr)

summarize_data <- function(df) {
  summary_stats <- df %>%
    summarise(across(everything(), list(
      distinct_values = ~n_distinct(.),
      missing_values = ~sum(is.na(.)),
      most_common_value = ~names(sort(table(.), decreasing = TRUE)[1])
    ), .names = "{.col}_{.fn}")) %>%
    pivot_longer(everything(), names_to = c("Column", ".value"), names_sep = "_")
  
  return(summary_stats)
}

summary_stats <- summarize_data(combined_data)


print(summary_stats)


str(combined_data)
summary(combined_data)