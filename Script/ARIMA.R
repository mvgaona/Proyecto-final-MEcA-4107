p_load(forecast)
rain <- c(987,1025,978,774,1563,569,1456,789,1479,566,1563,1698)
# Convert it to a time series object.

rain_ts <- ts(rain,start = c(2020,1),frequency = 12)


# Print the timeseries data.

print(rain_ts)


summary(rain_ts)
plot(rain_ts,main = "Before prediction")
model <- auto.arima(rain_ts) 
model
#Predictions
forecast_data <- forecast(model, 10) 


print(forecast_data)


plot(forecast_data, main = "forecasting_data for rain_ts") 
