library(RJDemetra)
library(readxl)
library(seasonal)
library(writexl)
library(dplyr)

df <- read_excel("historical_data/historical_special_coverage_exp_db.xlsx", sheet = 1)

df <- df %>%
  filter(grepl("Endeks", index_type), grepl("Other se", type_eng)) %>%
  select(data_date, value) %>%
  rename(diger_hizmetler = value)

df$data_date <- as.Date(df$data_date)

df <- df %>%
  filter(data_date > as.Date("2016-11-01"))
df

ts_data <- ts(df$diger_hizmetler, start = c(2016, 12), frequency = 12)

seasonal_adjust_diger_hizmetler <- function(ts_data) {
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
    arima.bq = 0,
    arima.mu = TRUE,
    usrdef.outliersEnabled = TRUE,
    # Aykırı değer tipleri ve tarihleri (örnek)
    usrdef.outliersType = c("LS","LS","LS","LS","LS","LS","LS","LS"),
    usrdef.outliersDate = c("2023-09-01","2021-12-01","2023-08-01","2023-07-01",
                            "2021-01-01","2025-01-01","2023-01-01","2018-09-01")
  )
  
  # Run seasonal adjustment
  result <- tramoseats(ts_data, spec)
  
  return(result)
}

# Apply seasonal adjustment
diger_hizmetler_sa <- seasonal_adjust_diger_hizmetler(ts_data)



# Extract the series components safely
if (!is.null(diger_hizmetler_sa$final$series)) {
  sa_series <- diger_hizmetler_sa$final$series
  
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
  write_xlsx(result_df, path = "ma_results/diger_hizmetler_sa_series_alternatives.xlsx")
  
  # Print summary
  print("Seasonal adjustment completed successfully!")
  print("Results exported to 'diger_hizmetler_sa_series.xlsx'")
  
  # View the first few rows
  print(head(result_df))
  
} else {
  print("Seasonal adjustment failed - no series data found")
  # Check for errors
  if (!is.null(diger_hizmetler_sa$diagnostics)) {
    print(diger_hizmetler_sa$diagnostics)
  }
}

# Return the result for inspection
diger_hizmetler_sa