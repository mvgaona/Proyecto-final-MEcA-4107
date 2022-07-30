# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 3 #####

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
       forecast,
       zoo,
       BiocManager,
       data.table,
       ranger, SuperLearner)
packageurl <-"https://cran.r-project.org/src/contrib/Archive/caret/caret_6.0-80.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
#Cargar datos
datacomp <- economics
data<- economics%>% dplyr::select(date, unemploy)
#Generación de valores de índice para el pronóstico. Que sea una predicción de 12 meses.
extended_data <- data %>% 
  rbind(tibble::tibble(date = seq(from = lubridate::as_date("2015-05-01"),
                                  by = "month", length.out = 12), 
                       unemploy = rep(NA, 12)))

tail(extended_data)
#Dataframe ya tiene las fechas para el pronóstico
#Toca ocuparse de la columna de fecha. xgboost no aborda bien las columnas de fecha, por lo que debemos dividirlo en varias columnas, describiendo la granularidad del tiempo. En este caso meses y años:
extended_data_mod <- extended_data %>%
  dplyr::mutate(., 
                months = lubridate::month(date),
                years = lubridate::year(date))
#Se dividen los datos en conjunto de entrenamiento y conjunto de predicción:
train <- extended_data_mod[1:nrow(data), ] # initial data

pred <- extended_data_mod[(nrow(data) + 1):nrow(extended_data), ] # extended time index
# Se transforman los datos en una forma matricial y se extrae la variable de destino. Además, se debe elimninar  las columnas de fechas y solo usar las recién creadas:

x_train <- xgboost::xgb.DMatrix(as.matrix(train %>%
                                            dplyr::select(months, years)))
x_pred <- xgboost::xgb.DMatrix(as.matrix(pred %>% 
                                           dplyr::select(months, years)))

y_train <- train$unemploy
y_train<- xgboost::xgb.DMatrix(as.matrix(train %>%
                                           dplyr::select(unemploy)))
#PREDICCIÓN XG BOOST 
#Con los datos preparados como en un apartado anterior se puede realizar el modelo de la misma forma que si no tratáramos con los datos de series temporales.
#Se necesita proporcionar el espacio de parámetros para ajustar el modelo., expecificando el método de validación cruzada con el número de pliegues y también se habilitan cálculos paralelos.
xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))
#Ahora se puede construir el modelo, usando árboles
xgb_model <- caret::train(
  x_train, y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbtree",
  nthread = 1)
#Se observanlos mejores valores que se eligieron como hiperparámetros:
xgb_model$bestTune
#Se realiza la predicción
xgb_pred <- xgb_model %>% stats::predict(x_pred)
## prediction en el train set
fitted <- xgb_model %>%
  stats::predict(x_train) %>%
  stats::ts(start = zoo::as.yearmon(min(train$date)), 
            end = zoo::as.yearmon(max(train$date)),
            frequency = 12)

# prediction in a form of ts object
xgb_forecast <- xgb_pred %>%
  stats::ts(start = zoo::as.yearmon(min(pred$date)),
            end = zoo::as.yearmon(max(pred$date)),
            frequency = 12)

# prediction in a form of ts object
xgb_forecast <- xgb_pred %>%
  stats::ts(start = zoo::as.yearmon(min(pred$date)),
            end = zoo::as.yearmon(max(pred$date)),
            frequency = 12)

# forecast object
forecast_list <- list(
  model = xgb_model$modelInfo,
  method = xgb_model$method,
  mean = xgb_forecast,
  x = ts, 
  fitted = fitted,
  residuals = as.numeric(ts) - as.numeric(fitted)
)
class(forecast_list) <- "forecast"
forecast::autoplot(forecast_list)
#Predicción con regresores
p_load(forecast)
x_train <- xgboost::xgb.DMatrix(cbind(
  as.matrix(extended_data_mod %>% dplyr::select(months, years)),
  reg_train))
x_pred <- xgboost::xgb.DMatrix(as.matrix(pred %>% 
                                           dplyr::select(months, years)),
                               reg_pred)

y_train <- data$value
