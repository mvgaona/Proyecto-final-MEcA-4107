# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### Proyecto final #####

#####Código de predicción de los precios de las viviendas con las variables ya seleccionadas y guardadas en archivos rds exportados del código: Limpieza de datos

install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería#Se cargan las librerías a usar en el presente Problem Set
p_load(Matrix,
       recipes,
       rio, 
       tidyverse,
       glmnet,
       dplyr,
       readr,
       gamlr,
       tidymodels,
       ggplot2,
       scales,
       rvest,
       stringr,
       boot,
       modeest,
       stargazer,
       sf,
       leaflet,
       tmaptools,
       class,
       rgeos,
       nngeo,
       osmdata,
       randomForest,
       xgboost,
       nnls,
       data.table,
       ranger, SuperLearner,
       prophet)
df <- read.csv('https://raw.githubusercontent.com/facebook/prophet/main/examples/example_wp_log_peyton_manning.csv')
m <- prophet(df)
class(m)
#Las predicciones se realizan en un marco de datos con una columna ds que contiene las fechas para las que se realizarán las predicciones. La función make_future_dataframe toma el objeto modelo y una serie de períodos para pronosticar y produce un marco de datos adecuado. De forma predeterminada, también incluirá las fechas históricas para que podamos evaluar el ajuste dentro de la muestra.
#La función make_future_dataframe toma el modelo de objeto y una serie de períodos para pronosticar y producir un marco de datos adecuado
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)
prophet_plot_components(m, forecast)
#Prophet_plot_components para ver el pronóstico desglosado en tendencia, estacionalidad semanal y estacionalidad anual.
