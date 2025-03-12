library(tidyverse)

pivotLonger <- function(df, factorColNames) {
  # Pivot the data frame so that outcome measures become factors instead of columns
  long_df <- df %>%
    pivot_longer(
      cols = -all_of(factorColNames), # Exclude all factor columns from pivoting
      names_to = "OutcomeMeasure", # New column to store the original column names
      values_to = "Value" # New column to store the values
    )
  
  return(long_df)
}

unpivotLonger <- function(long_df) {
  # Get all columns except OutcomeMeasure and Value
  factorColNames <- setdiff(names(long_df), c("OutcomeMeasure", "Value"))
  
  # Pivot the data frame back to wide format
  wide_df <- long_df %>%
    pivot_wider(
      id_cols = all_of(factorColNames),
      names_from = OutcomeMeasure,
      values_from = Value
    )
  
  return(wide_df)
}