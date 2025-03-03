# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

getwd()

library(readxl)
thewinddata2 <- read_excel("thewinddata2.xlsx")
getwd()
thewinddata2 <- read_excel(file.choose())


# Extract the "Wind, wave, tidal1 (Mtoe)" column and convert it to a time series
wind_wave_tidal_ts <- ts(thewinddata2$`Wind, wave, tidal1 (Mtoe)`, start = 1990, frequency = 1)  # Annual data

# Plot the time series to visualize trends
plot.ts(wind_wave_tidal_ts, main = "Wind, Wave, Tidal Energy (Mtoe)", ylab = "Energy (Mtoe)", xlab = "Year")

# Step 1: Check stationarity
adf_test <- adf.test(wind_wave_tidal_ts)
print(adf_test)

# If non-stationary, apply differencing
if (adf_test$p.value > 0.05) {
  wind_wave_tidal_ts_diff <- diff(wind_wave_tidal_ts)
  plot.ts(wind_wave_tidal_ts_diff, main = "Differenced Time Series", ylab = "Differenced Energy", xlab = "Year")
  adf_test_diff <- adf.test(wind_wave_tidal_ts_diff)
  print(adf_test_diff)
}

# Step 2: Identify ARIMA parameters using ACF and PACF
acf(wind_wave_tidal_ts, main = "ACF for Wind, Wave, Tidal Energy")
pacf(wind_wave_tidal_ts, main = "PACF for Wind, Wave, Tidal Energy")

# Use auto.arima to find the best model
best_model <- auto.arima(wind_wave_tidal_ts, seasonal = FALSE)
summary(best_model)

# Step 3: Check residuals for white noise
checkresiduals(best_model)

# Step 4: Forecast for 10 years
forecast_horizon <- 10
forecast_values <- forecast(best_model, h = forecast_horizon)

# Plot the forecast
plot(forecast_values, main = "10-Year Forecast for Wind, Wave, Tidal Energy",
     xlab = "Year", ylab = "Energy (Mtoe)")

# Display forecasted values
print(forecast_values)

# Save forecast results to a CSV file
write.csv(forecast_values, file = "forecast_wind_wave_tidal.csv")




# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

# Assuming you have already loaded your dataset using read_excel and named it `thewinddata2`
# Extract the "Solar photovoltaic (Mtoe)" column and filter data starting from 2005
solar_photovoltaic_filtered <- thewinddata2[thewinddata2$Year >= 2005, ]

# Convert the filtered data to a time series
solar_photovoltaic_ts <- ts(solar_photovoltaic_filtered$`Solar photovoltaic (Mtoe)`, 
                            start = 2005, frequency = 1)  # Annual data

# Plot the time series to visualize trends
plot.ts(solar_photovoltaic_ts, main = "Solar Photovoltaic Energy (Mtoe)", 
        ylab = "Energy (Mtoe)", xlab = "Year")

# Step 1: Check stationarity
adf_test <- adf.test(solar_photovoltaic_ts)
print(adf_test)

# If non-stationary, apply differencing
if (adf_test$p.value > 0.05) {
  solar_photovoltaic_ts_diff <- diff(solar_photovoltaic_ts)
  plot.ts(solar_photovoltaic_ts_diff, main = "Differenced Time Series", 
          ylab = "Differenced Energy", xlab = "Year")
  adf_test_diff <- adf.test(solar_photovoltaic_ts_diff)
  print(adf_test_diff)
} else {
  solar_photovoltaic_ts_diff <- solar_photovoltaic_ts  # If stationary, no differencing needed
}

# Step 2: Identify ARIMA parameters using ACF and PACF
acf(solar_photovoltaic_ts_diff, main = "ACF for Solar Photovoltaic Energy")
pacf(solar_photovoltaic_ts_diff, main = "PACF for Solar Photovoltaic Energy")

# Use auto.arima to find the best model
best_model <- auto.arima(solar_photovoltaic_ts_diff, seasonal = FALSE)
summary(best_model)

# Step 3: Check residuals for white noise
checkresiduals(best_model)

# Step 4: Forecast for 10 years
forecast_horizon <- 10
forecast_values <- forecast(best_model, h = forecast_horizon)

# Plot the forecast
plot(forecast_values, main = "10-Year Forecast for Solar Photovoltaic Energy",
     xlab = "Year", ylab = "Energy (Mtoe)")

# Display forecasted values
print(forecast_values)

# Save forecast results to a CSV file
write.csv(forecast_values, file = "forecast_solar_photovoltaic.csv")




