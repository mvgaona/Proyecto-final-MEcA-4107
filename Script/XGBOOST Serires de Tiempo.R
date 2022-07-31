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


lista_agentes <- read.csv("../Datos/Listado_agentes.csv", header=TRUE, stringsAsFactors=FALSE)

ONI <- read_html("https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php")%>%
  html_table()

#Tratamiento de datos para el ONI
ONI_nuevo<-ONI[[9]] #Se extrae la lista donde se encuentra la información

ONI_iter<-ONI_nuevo #Variable auxiliar

ONI_iter<-ONI_iter%>% mutate(X1 = NULL) #Eliminación de primera columna

ONI0<-ONI_iter[55, 1:12] #Extracción primera fila
ONI0_t<-data.frame(t(ONI0)) #Transpuesta para que quede en columna
colnames(ONI0_t)<-c('ONI')
ONI1<-ONI_iter[57, 1:12]
ONI1_t<-data.frame(t(ONI1))
colnames(ONI1_t)<-c('ONI')
ONI2<-ONI_iter[58, 1:12]
ONI2_t<-data.frame(t(ONI2))
colnames(ONI2_t)<-c('ONI')
ONI3<-ONI_iter[59, 1:12]
ONI3_t<-data.frame(t(ONI3))
colnames(ONI3_t)<-c('ONI')
ONI4<-ONI_iter[60, 1:12]
ONI4_t<-data.frame(t(ONI4))
colnames(ONI4_t)<-c('ONI')
ONI5<-ONI_iter[61, 1:12]
ONI5_t<-data.frame(t(ONI5))
colnames(ONI5_t)<-c('ONI')
ONI6<-ONI_iter[62, 1:12]
ONI6_t<-data.frame(t(ONI6))
colnames(ONI6_t)<-c('ONI')
ONI7<-ONI_iter[63, 1:12]
ONI7_t<-data.frame(t(ONI7))
colnames(ONI7_t)<-c('ONI')
ONI8<-ONI_iter[64, 1:12]
ONI8_t<-data.frame(t(ONI8))
colnames(ONI8_t)<-c('ONI')
ONI9<-ONI_iter[65, 1:12]
ONI9_t<-data.frame(t(ONI9))
colnames(ONI9_t)<-c('ONI')
ONI10<-ONI_iter[66, 1:12]
ONI10_t<-data.frame(t(ONI10))
colnames(ONI10_t)<-c('ONI')
ONI12<-ONI_iter[68, 1:12]
ONI12_t<-data.frame(t(ONI12))
colnames(ONI12_t)<-c('ONI')
ONI13<-ONI_iter[69, 1:12]
ONI13_t<-data.frame(t(ONI13))
colnames(ONI13_t)<-c('ONI')
ONI14<-ONI_iter[70, 1:12]
ONI14_t<-data.frame(t(ONI14))
colnames(ONI14_t)<-c('ONI')
ONI15<-ONI_iter[71, 1:12]
ONI15_t<-data.frame(t(ONI15))
colnames(ONI15_t)<-c('ONI')
ONI16<-ONI_iter[72, 1:12]
ONI16_t<-data.frame(t(ONI16))
colnames(ONI16_t)<-c('ONI')
ONI17<-ONI_iter[73, 1:12]
ONI17_t<-data.frame(t(ONI17))
colnames(ONI17_t)<-c('ONI')
ONI18<-ONI_iter[74, 1:12]
ONI18_t<-data.frame(t(ONI18))
colnames(ONI18_t)<-c('ONI')
ONI19<-ONI_iter[75, 1:12]
ONI19_t<-data.frame(t(ONI19))
colnames(ONI19_t)<-c('ONI')
ONI20<-ONI_iter[76, 1:12]
ONI20_t<-data.frame(t(ONI20))
colnames(ONI20_t)<-c('ONI')
ONI21<-ONI_iter[77, 1:12]
ONI21_t<-data.frame(t(ONI21))
colnames(ONI21_t)<-c('ONI')
ONI22<-ONI_iter[79, 1:12]
ONI22_t<-data.frame(t(ONI22))
colnames(ONI22_t)<-c('ONI')
ONI23<-ONI_iter[80, 1:12]
ONI23_t<-data.frame(t(ONI23))
colnames(ONI23_t)<-c('ONI')
ONI24<-ONI_iter[81, 1:12]
ONI24_t<-data.frame(t(ONI24))
colnames(ONI24_t)<-c('ONI')

ONI_month<-rbind(ONI0_t, ONI1_t, ONI2_t, ONI3_t, ONI4_t, ONI5_t, ONI6_t, ONI7_t, ONI8_t, ONI9_t, ONI10_t, ONI12_t, ONI13_t, ONI14_t, ONI15_t, ONI16_t, ONI17_t, ONI18_t, ONI19_t, ONI20_t, ONI21_t, ONI22_t, ONI23_t, ONI24_t)
fecha <- seq(from = lubridate::as_date("2000-01-01"),
           by = "month", length.out = 270)
fecha<-data.frame(fecha)

ONI_month_final<-ONI_month[12:288,]
ONI_month_final<-ONI_month_final[1:270,]
ONI_month_final<-data.frame(ONI_month_final)
ONi_mes<-cbind(fecha,ONI_month_final )
ONi_mes<-ONi_mes%>% mutate(mes = lubridate::month(fecha))
ONi_mes<-ONi_mes%>% mutate(año = lubridate::year(fecha))#

ONi_mes<-ONi_mes%>% mutate(ma = paste(ONi_mes$mes,ONi_mes$año))#


fecha_day<- seq(from = lubridate::as_date("2000-01-01"),
                        by = "day", length.out = 8217)
fecha_day<-data.frame(fecha_day)
ONI_dia<-fecha_day
ONI_dia<-data.frame(ONI_dia)
ONI_dia<-as.data.frame(ONI_dia)
ONI_dia<-ONI_dia%>% mutate(mes = lubridate::month(fecha_day)) #
ONI_dia<-ONI_dia%>% mutate(año = lubridate::year(fecha_day))
ONI_dia<-ONI_dia%>% mutate(ma = paste(ONI_dia$mes, ONI_dia$año))#

ONI_final<-left_join(ONI_dia, ONi_mes, by="ma" )
ONI_final<-ONI_final%>% mutate(mes.x= NULL)
ONI_final<-ONI_final%>% mutate(año.x= NULL)
ONI_final<-ONI_final%>% mutate(ma= NULL)
ONI_final<-ONI_final%>% mutate(fecha= NULL)
ONI_final<-ONI_final%>% mutate(mes.y= NULL)
ONI_final<-ONI_final%>% mutate(año.y= NULL)

colnames(ONI_final) <- c('Fecha','ONI')
saveRDS(ONI_final, "../Datos/Bases oficiales/ONI.rds" )

############################################################################################
### Aporte Diario_dia

Aportes_energia<-data.frame(readRDS("../Datos/Bases oficiales/Aportes_Diarios.rds")) 
Aportes_energia$...5[is.na(Aportes_energia$...5)] = 0 #Se imputa cero a los aportes de energía con NA
Aportes_energia<- Aportes_energia[c(-1),] #Se eilimina primera fila con referencia a los nombres de los archivos excel
Aportes_energia$...5<-as.numeric(Aportes_energia$...5) #Se vuelven números los valores del excel

Aportes_energia$Day<-as.Date(Aportes_energia$Histórico.Aportes) #Se convierte en formato de fecha la columna
Aporte_dia<-aggregate(Aportes_energia$...5, by=list(Aportes_energia$Day), sum) #Se suman los aportes de energía de cada río

Aportes_energia_dia<-cbind(Aporte_dia$Group.1, Aporte_dia$x )
colnames(Aportes_energia_dia) <- c('Fecha','Aportes_total')
saveRDS(Aportes_energia_dia, "../Datos/Bases oficiales/Aportes_energia_dia.rds" )

