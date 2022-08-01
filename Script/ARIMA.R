p_load(forecast)
# Convert it to a time series object.


BASEOF<- readRDS("../Datos/Bases oficiales/Base_de_datos_oficial.rds")
# Print the timeseries data.
####----0 hs----#
BASEOF0 <-as.numeric(BASEOF$PBN0)
BASEOF <- ts(BASEOF0)
print(BASEOF0)
summary(BASEOF0)
plot(BASEOF0 , main = "Before prediction")
model <- auto.arima(BASEOF0) 
model
#Predictions
forecast_data <- forecast(model, 20) 
print(forecast_data)
plot(forecast_data, main = "forecasting_data for PBN") 
