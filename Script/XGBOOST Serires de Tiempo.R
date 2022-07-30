# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### TRABAJO FINAL #####

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
pred$unemploy[is.na(pred$unemploy)] = 0
# Se transforman los datos en una forma matricial y se extrae la variable de destino. Además, se debe elimninar  las columnas de fechas y solo usar las recién creadas:

x_train <- xgboost::xgb.DMatrix(as.matrix(train %>%
                                            dplyr::select(months, years)))
x_pred <- xgboost::xgb.DMatrix(as.matrix(pred %>% 
                                           dplyr::select(months, years)))

y_train <- train$unemploy

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
  unemploy ~months+years,
  data=train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1)
#verbose=TRUE)


#Se observanlos mejores valores que se eligieron como hiperparámetros:
xgb_model$bestTune
#Se realiza la predicción



xgb_pred <- xgb_model %>% stats::predict(pred)
xgb_pred<-data.frame(xgb_pred)

Resultados<-cbind(pred$date,xgb_pred)


##########################################################################################################################
############################################################################################################################
#Prueba con código realizado para el PS3

x_train <- model.matrix(unemploy ~months+years, data =train)[, -1]
y_train <- train$unemploy

x_test <- model.matrix(~months+years, data =pred)[, -1]
y_test <- pred$unemploy


xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
xgb_test <- xgb.DMatrix(data = x_train, label = y_train) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist <-list(train=xgb_train, test=xgb_test)

model4<- xgb.train(data = xgb_train, max.depth = 100, watchlist=watchlist, nrounds = 1000)

predicciones_mod4 <-predict(model4, xgb_test)

MSE_mod4 <- sqrt(mean((predicciones_mod4-train$unemploy)^2))
Diferencia_mod4 <- (predicciones_mod4 - train$unemploy)
Diferencia_mod4<-data.frame(Diferencia_mod4)





