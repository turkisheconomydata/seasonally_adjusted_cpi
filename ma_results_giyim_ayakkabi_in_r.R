library(RJDemetra)
library(readxl)
library(seasonal)
library(writexl)
library(dplyr)

df <- read_excel("historical_data/historical_special_coverage_exp_db.xlsx", sheet = 1)

df <- df %>%
  filter(grepl("Endeks", index_type), grepl("Clothing", type_eng)) %>%
  select(data_date, value) %>%
  rename(giyim_ayakkabi = value)

df$data_date <- as.Date(df$data_date)

df <- df %>%
  filter(data_date > as.Date("2016-11-01"))
df

ts_data <- ts(df$giyim_ayakkabi, start = c(2016, 12), frequency = 12)

seasonal_adjust_giyim_ayakkabi <- function(ts_data) {
  # Create specification object for TRAMO-SEATS with specific model
  spec <- tramoseats_spec(
    "RSAfull", 
    transform.function = "Log",
    automdl.enabled = FALSE,  # Disable automatic model selection
    arima.p = 0,
    arima.d = 1,
    arima.q = 1,
    arima.bp = 0,
    arima.bd = 1,
    arima.bq = 1,
    #arima.mu = TRUE,
    usrdef.outliersEnabled = TRUE,
    # Aykırı değer tipleri ve tarihleri (örnek)
    usrdef.outliersType = c("LS","LS","AO"),
    usrdef.outliersDate = c("2021-12-01","2023-08-01","2021-05-01")
  )
  
  # Run seasonal adjustment
  result <- tramoseats(ts_data, spec)
  
  return(result)
}

# Apply seasonal adjustment
giyim_ayakkabi_sa <- seasonal_adjust_giyim_ayakkabi(ts_data)



# Extract the series components safely
if (!is.null(giyim_ayakkabi_sa$final$series)) {
  sa_series <- giyim_ayakkabi_sa$final$series
  
  # Convert to data frame with proper dates
  result_df <- data.frame(
    Date = time(sa_series),
    Original = as.numeric(ts_data),
    Seasonally_Adjusted = as.numeric(sa_series[,"sa"]),
    Trend = as.numeric(sa_series[,"t"]),
    Seasonal = as.numeric(sa_series[,"s"]),
    Irregular = as.numeric(sa_series[,"i"])
  )
  
  # Export to Excel
  write_xlsx(result_df, path = "ma_results/giyim_ayakkabi_sa_series_alternatives.xlsx")
  
  # Print summary
  print("Seasonal adjustment completed successfully!")
  print("Results exported to 'giyim_ayakkabi_sa_series.xlsx'")
  
  # View the first few rows
  print(head(result_df))
  
} else {
  print("Seasonal adjustment failed - no series data found")
  # Check for errors
  if (!is.null(giyim_ayakkabi_sa$diagnostics)) {
    print(giyim_ayakkabi_sa$diagnostics)
  }
}

# Return the result for inspection
giyim_ayakkabi_sa