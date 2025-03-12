library(tidyverse)
library(reshape2)

pivotLonger <- function(df, factorColNames) {
  # Pivot the data frame so that outcome measures become factors instead of columns

  # Identify factor columns (non-numeric columns)
  factor_cols <- names(df)[!sapply(df, is.numeric)]
  
  # Identify outcome measure columns (numeric columns)
  outcome_cols <- names(df)[sapply(df, is.numeric)]
  
  # Pivot the data frame
  long_df <- df %>%
    pivot_longer(
      cols = all_of(outcome_cols),
      names_to = "OutcomeMeasure",
      values_to = "Value"
    ) %>%
    # Reorder columns to put OutcomeMeasure first, then factors, then Value
    select(OutcomeMeasure, all_of(factor_cols), Value)
  
  # Set all columns to be factors except Value
  long_df <- long_df %>%
    mutate(across(-Value, as.factor))
  
  return(long_df)
}

unpivotLonger <- function(long_df, colNamesToRemove = c("OutcomeMeasure", "Value")) {
  # Get all columns except OutcomeMeasure and Value
  factorColNames <- setdiff(names(long_df), colNamesToRemove)

  outcomeColNames <- levels(long_df$OutcomeMeasure)
  
  # Pivot the data frame back to wide format
  wide_df <- long_df %>%
    pivot_wider(
      id_cols = all_of(outcomeColNames),
      names_from = OutcomeMeasure,
      values_from = Value
    )
  
  return(wide_df)
}