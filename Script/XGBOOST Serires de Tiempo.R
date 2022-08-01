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
       readxl,
       reshape2,
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









# 
# Generacion_xxx <- data.frame(readRDS("../Datos/Bases oficiales/Generacion_23_30062022.rds")) 
# 
# Aportes<- data.frame(readRDS("../Datos/Bases oficiales/Aportes_energia_dia_30_06_2022.rds")) 
# 
# 
# Aux_precio_bolsa<-Precios_Bolsa
# colnames(Aux_precio_bolsa)[1] <- "Fecha"
# Aux_precio_bolsa<- Aux_precio_bolsa[c(-1),]
# rownames(Aux_precio_bolsa)=NULL
# fecha_day<- seq(from = lubridate::as_date("2000-01-01"),
#                 by = "day", length.out = 8243)
# Aux_precio_bolsa$Fecha<-fecha_day
# fecha_day<-data.frame(fecha_day)
# 
# Aux_ONI<-ONI
# colnames(Aux_ONI)[1] <- "Fecha"
# Nuevo_frame<-left_join(Aux_ONI,Aux_precio_bolsa, by="Fecha")
# 
# filtro<-is.na(Precio_bolsa)
# table(filtro)


# rm(Generacion_0)
# 
# 
# Capacidad_neta_aux<-Capacidad_neta1 %>% 
#   group_by(...2) %>% 
#   summarize(Count = n())
# 
# Capacidad_nta_para_imp<-left_join(Capacidad_neta_aux,Capacidad_neta1, by="...2")
# 
# #Se ponen en minúscula los caracteres de description y title en la base test
# Capacidad_neta$...2<-str_to_lower(string=Capacidad_neta$...2)
# Generadores$...2<-str_to_lower(string=Generadores$...2)
# 
# # Se eliminan las tildes
# Capacidad_neta$...2 <- iconv(Capacidad_neta$...2, from = "UTF-8", to = "ASCII//TRANSLIT")
# Generadores$...2 <- iconv(Generadores$...2, from = "UTF-8", to = "ASCII//TRANSLIT")
# 
# # Se eliminan caracteres especiales
# Capacidad_neta$...2 <- str_replace_all(Capacidad_neta$...2, "[^[:alnum:]]", " ")
# Generadores$...2<- str_replace_all(Generadores$...2, "[^[:alnum:]]", " ")
# 
# # Se eliminan espacios extras
# Capacidad_neta$...2<- gsub("\\s+", " ", str_trim(Capacidad_neta$...2))
# Generadores$...2 <- gsub("\\s+", " ", str_trim(Generadores$...2))
# 
# View(Capacidad_neta)
# Capacidad_neta<- Capacidad_neta[c(-1),]
# 
# Generacion_final<- Capacidad_neta1%>% group_by(...2) %>% filter (! duplicated(...2))
# colnames(Generacion_final)[2] <- "Values_Type"
# colnames(Generacion_final)[3] <- "Values_enersource"
# 
# Generacion_final<-left_join(Capacidad_neta, by="...2")
# 
# rm(Capacidad_neta, Generacion,Generacion_tipo_1 )
# 
