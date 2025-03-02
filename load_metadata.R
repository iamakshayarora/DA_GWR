metadata_path <- "Metadata.csv"
metadata <- read_csv(metadata_path)

print(head(metadata))

summarize_data2 <- function(df) {
  summary_stats <- df %>%
    summarise(across(everything(), list(
      distinct_values = ~n_distinct(.),
      missing_values = ~sum(is.na(.)),
      most_common_value = ~names(sort(table(.), decreasing = TRUE)[1])
    ), .names = "{.col}_{.fn}")) %>%
    pivot_longer(everything(), names_to = c("Column", ".value"), names_sep = "_")
  
  return(summary_stats)
}

metadata_summary <- summarize_data2(metadata)

print(metadata_summary)


str(metadata) 
summary(metadata) 
sapply(metadata, unique)
