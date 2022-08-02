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

### Base oficial
rm(list = ls()) 
fecha = seq(from = lubridate::as_date("2000-01-01"),
           by = "day", length.out = 8217) 

fecha<-data.frame(fecha)

BASEOF<- readRDS("../Datos/Bases oficiales/Base_de_datos_oficial.rds")
#Se transforman a números los siguientes 
cols.num<-c('PBN0', 'PBN1', 'PBN2', 'PBN3', 'PBN4', 'PBN5', 'PBN6', 'PBN7', 'PBN8', 'PBN9', 'PBN10', 'PBN11', 'PBN12', 'PBN13', 'PBN14', 'PBN15', 'PBN16', 'PBN17', 'PBN18', 'PBN19', 'PBN20', 'PBN21', 'PBN22', 'PBN23', 'ONI')
BASEOF[cols.num] <- sapply(BASEOF[cols.num],as.numeric)
sapply(BASEOF, class)
BASEOF<-cbind(fecha,BASEOF)
BASEOF<-BASEOF%>% mutate(Fecha = NULL) 

BASEOF<- BASEOF %>%
  dplyr::mutate(., 
                mes = lubridate::month(fecha),
                año = lubridate::year(fecha),
                dia=lubridate::day(fecha) )

#saveRDS(BASEOF,"../Datos/Bases oficiales/Base_de_datos_oficial_3.rds" )


train <- BASEOF[1:5751, ] # initial data (70% de los datos desde el 2001)
pred <- BASEOF[(5752:8217), ] # extended time index (30% restante)



##-----0 hs-----#

x_train0 <- model.matrix(PBN0 ~Gen_CoGenerador0+ Gen_Hidraulica0 + Gen_Termica0 + Gen_Eolica0 + Gen_Solar0 + ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train0 <- train$PBN0
x_test0 <- model.matrix(PBN0~Gen_CoGenerador0+ Gen_Hidraulica0 + Gen_Termica0 + Gen_Eolica0 + Gen_Solar0 + ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test0 <- pred$PBN0
xgb_train0 <- xgb.DMatrix(data = x_train0, label = y_train0)
xgb_test0 <- xgb.DMatrix(data = x_test0, label = y_test0) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist0 <-list(train=xgb_train0, test=xgb_test0)

model0<- xgb.train(data = xgb_train0, max.depth = 100, watchlist=watchlist0, nrounds = 1000)
saveRDS(model0,"../App_web/Pred_precio_bolsa_horario/data/model0.rds" )
predicciones_mod0 <-predict(model0, xgb_test0)
predicciones_mod0<- data.frame(predicciones_mod0)

##-------1 hs----##
x_train1 <- model.matrix(PBN1 ~Gen_CoGenerador1+ Gen_Hidraulica1 + Gen_Termica1 + Gen_Eolica1 + Gen_Solar1 + ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train1 <- train$PBN1

x_test1 <- model.matrix(PBN1~Gen_CoGenerador1+ Gen_Hidraulica1 + Gen_Termica1 + Gen_Eolica1 + Gen_Solar1 + ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test1 <- pred$PBN1
xgb_train1 <- xgb.DMatrix(data = x_train1, label = y_train1)
xgb_test1 <- xgb.DMatrix(data = x_test1, label = y_test1) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist1 <-list(train=xgb_train1, test=xgb_test1)

model1<- xgb.train(data = xgb_train1, max.depth = 100, watchlist=watchlist1, nrounds = 1000)
saveRDS(model1,"../App_web/Pred_precio_bolsa_horario/data/model1.rds" )
predicciones_mod1 <-predict(model1, xgb_test1)
predicciones_mod1<- data.frame(predicciones_mod1)
##----2 hs----##
x_train2 <- model.matrix(PBN2 ~Gen_CoGenerador2 + Gen_Hidraulica2 + Gen_Termica2 + Gen_Eolica2 + Gen_Solar2 + ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train2 <- train$PBN2
x_test2 <- model.matrix(PBN2~Gen_CoGenerador2+ Gen_Hidraulica2 + Gen_Termica2 + Gen_Eolica2 + Gen_Solar2 + ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test2 <- pred$PBN2
xgb_train2 <- xgb.DMatrix(data = x_train2, label = y_train2)
xgb_test2 <- xgb.DMatrix(data = x_test2, label = y_test2) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist2 <-list(train=xgb_train2, test=xgb_test2)

model2<- xgb.train(data = xgb_train2, max.depth = 100, watchlist=watchlist2, nrounds = 1000)
saveRDS(model2,"../App_web/Pred_precio_bolsa_horario/data/model2.rds" )
predicciones_mod2 <-predict(model2, xgb_test2)
predicciones_mod2<- data.frame(predicciones_mod2)

##----3 hs----##
x_train3 <- model.matrix(PBN3 ~Gen_CoGenerador3 + Gen_Hidraulica3 + Gen_Termica3 + Gen_Eolica3 + Gen_Solar3 + ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train3 <- train$PBN3
x_test3 <- model.matrix(PBN3~Gen_CoGenerador3+ Gen_Hidraulica3 + Gen_Termica3 + Gen_Eolica3 + Gen_Solar3 + ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test3 <- pred$PBN3
xgb_train3 <- xgb.DMatrix(data = x_train3, label = y_train3)
xgb_test3 <- xgb.DMatrix(data = x_test3, label = y_test3) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist3 <-list(train=xgb_train3, test=xgb_test3)

model3<- xgb.train(data = xgb_train3, max.depth = 100, watchlist=watchlist3, nrounds = 1000)
saveRDS(model3,"../App_web/Pred_precio_bolsa_horario/data/model3.rds" )
predicciones_mod3 <-predict(model3, xgb_test3)
predicciones_mod3<- data.frame(predicciones_mod3)

##----4 hs----##
x_train4 <- model.matrix(PBN4~Gen_CoGenerador4+ Gen_Hidraulica4 + Gen_Termica4 + Gen_Eolica4 + Gen_Solar4+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train4 <- train$PBN4
x_test4 <- model.matrix(PBN4~Gen_CoGenerador4+ Gen_Hidraulica4 + Gen_Termica4 + Gen_Eolica4 + Gen_Solar4 + ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test4 <- pred$PBN4
xgb_train4 <- xgb.DMatrix(data = x_train4, label = y_train4)
xgb_test4 <- xgb.DMatrix(data = x_test4, label = y_test4) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist4 <-list(train=xgb_train4, test=xgb_test4)

model4<- xgb.train(data = xgb_train4, max.depth = 100, watchlist=watchlist4, nrounds = 1000)
saveRDS(model4,"../App_web/Pred_precio_bolsa_horario/data/model4.rds" )
predicciones_mod4 <-predict(model4, xgb_test4)
predicciones_mod4<- data.frame(predicciones_mod4)

##----5 hs----#
x_train5 <- model.matrix(PBN5~Gen_CoGenerador5+ Gen_Hidraulica5 + Gen_Termica5 + Gen_Eolica5 + Gen_Solar5+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train5 <- train$PBN5
x_test5 <- model.matrix(PBN5~Gen_CoGenerador5+ Gen_Hidraulica5 + Gen_Termica5 + Gen_Eolica5 + Gen_Solar5+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test5 <- pred$PBN5
xgb_train5 <- xgb.DMatrix(data = x_train5, label = y_train5)
xgb_test5 <- xgb.DMatrix(data = x_test5, label = y_test5) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist5 <-list(train=xgb_train5, test=xgb_test5)

model5<- xgb.train(data = xgb_train5, max.depth = 100, watchlist=watchlist5, nrounds = 1000)
saveRDS(model5,"../App_web/Pred_precio_bolsa_horario/data/model5.rds" )
predicciones_mod5 <-predict(model5, xgb_test5)
predicciones_mod5<- data.frame(predicciones_mod5)

##----6 hs----##
x_train6 <- model.matrix(PBN6~Gen_CoGenerador6+ Gen_Hidraulica6 + Gen_Termica6 + Gen_Eolica6 + Gen_Solar6+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train6 <- train$PBN6
x_test6 <- model.matrix(PBN6~Gen_CoGenerador6+ Gen_Hidraulica6 + Gen_Termica6 + Gen_Eolica6 + Gen_Solar6+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test6 <- pred$PBN6
xgb_train6 <- xgb.DMatrix(data = x_train6, label = y_train6)
xgb_test6 <- xgb.DMatrix(data = x_test6, label = y_test6) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist6 <-list(train=xgb_train6, test=xgb_test6)

model6<- xgb.train(data = xgb_train6, max.depth = 100, watchlist=watchlist6, nrounds = 1000)
saveRDS(model6,"../App_web/Pred_precio_bolsa_horario/data/model6.rds" )
predicciones_mod6 <-predict(model6, xgb_test6)
predicciones_mod6<- data.frame(predicciones_mod6)

##----7 hs----#
x_train7 <- model.matrix(PBN7~Gen_CoGenerador7+ Gen_Hidraulica7 + Gen_Termica7 + Gen_Eolica7 + Gen_Solar7+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train7 <- train$PBN7
x_test7 <- model.matrix(PBN7~Gen_CoGenerador7+ Gen_Hidraulica7 + Gen_Termica7 + Gen_Eolica7 + Gen_Solar7+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test7 <- pred$PBN7
xgb_train7 <- xgb.DMatrix(data = x_train7, label = y_train7)
xgb_test7 <- xgb.DMatrix(data = x_test7, label = y_test7) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist7 <-list(train=xgb_train7, test=xgb_test7)
model7<- xgb.train(data = xgb_train7, max.depth = 100, watchlist=watchlist7, nrounds = 1000)
saveRDS(model7,"../App_web/Pred_precio_bolsa_horario/data/model7.rds" )
predicciones_mod7 <-predict(model7, xgb_test7)
predicciones_mod7<- data.frame(predicciones_mod7)

##----8 hs----#
x_train8 <- model.matrix(PBN8~Gen_CoGenerador8+ Gen_Hidraulica8 + Gen_Termica8 + Gen_Eolica8 + Gen_Solar8+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train8 <- train$PBN8
x_test8 <- model.matrix(PBN8~Gen_CoGenerador8+ Gen_Hidraulica8 + Gen_Termica8 + Gen_Eolica8 + Gen_Solar8+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test8 <- pred$PBN8
xgb_train8 <- xgb.DMatrix(data = x_train8, label = y_train8)
xgb_test8 <- xgb.DMatrix(data = x_test8, label = y_test8) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist8 <-list(train=xgb_train8, test=xgb_test8)
model8<- xgb.train(data = xgb_train8, max.depth = 100, watchlist=watchlist8, nrounds = 1000)
saveRDS(model8,"../App_web/Pred_precio_bolsa_horario/data/model8.rds" )
predicciones_mod8 <-predict(model8, xgb_test8)
predicciones_mod8<- data.frame(predicciones_mod8)

##----9 hs----##
x_train9 <- model.matrix(PBN9~Gen_CoGenerador9+ Gen_Hidraulica9 + Gen_Termica9 + Gen_Eolica9 + Gen_Solar9+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train9 <- train$PBN9
x_test9<- model.matrix(PBN9~Gen_CoGenerador9+ Gen_Hidraulica9 + Gen_Termica9 + Gen_Eolica9 + Gen_Solar9+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test9 <- pred$PBN9
xgb_train9 <- xgb.DMatrix(data = x_train9, label = y_train9)
xgb_test9 <- xgb.DMatrix(data = x_test9, label = y_test9) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist9 <-list(train=xgb_train9, test=xgb_test9)
model9<- xgb.train(data = xgb_train9, max.depth = 100, watchlist=watchlist9, nrounds = 1000)
saveRDS(model9,"../App_web/Pred_precio_bolsa_horario/data/model9.rds" )
predicciones_mod9 <-predict(model9, xgb_test9)
predicciones_mod9<- data.frame(predicciones_mod9)

##----10 hs----##
x_train10 <- model.matrix(PBN10~Gen_CoGenerador10+ Gen_Hidraulica10 + Gen_Termica10 + Gen_Eolica10 + Gen_Solar10+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train10 <- train$PBN10
x_test10 <- model.matrix(PBN10~Gen_CoGenerador10+ Gen_Hidraulica10 + Gen_Termica10 + Gen_Eolica10 + Gen_Solar10+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test10 <- pred$PBN10
xgb_train10 <- xgb.DMatrix(data = x_train10, label = y_train10)
xgb_test10<- xgb.DMatrix(data = x_test10, label = y_test10) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist10 <-list(train=xgb_train10, test=xgb_test10)
model10<- xgb.train(data = xgb_train10, max.depth = 100, watchlist=watchlist10, nrounds = 1000)
saveRDS(model10,"../App_web/Pred_precio_bolsa_horario/data/model10.rds" )
predicciones_mod10 <-predict(model10, xgb_test10)
predicciones_mod10<- data.frame(predicciones_mod10)

##----11 hs----##
x_train11 <- model.matrix(PBN11~Gen_CoGenerador11+ Gen_Hidraulica11 + Gen_Termica11 + Gen_Eolica11 + Gen_Solar11+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train11 <- train$PBN11
x_test11 <- model.matrix(PBN11~Gen_CoGenerador11+ Gen_Hidraulica11 + Gen_Termica11 + Gen_Eolica11 + Gen_Solar11+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test11 <- pred$PBN11
xgb_train11 <- xgb.DMatrix(data = x_train11, label = y_train11)
xgb_test11 <- xgb.DMatrix(data = x_test11, label = y_test11) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist11 <-list(train=xgb_train11, test=xgb_test11)
model11<- xgb.train(data = xgb_train11, max.depth = 100, watchlist=watchlist11, nrounds = 1000)
saveRDS(model11,"../App_web/Pred_precio_bolsa_horario/data/model11.rds" )
predicciones_mod11 <-predict(model11, xgb_test11)
predicciones_mod11<- data.frame(predicciones_mod11)

##----12 hs----##
x_train12 <- model.matrix(PBN12~Gen_CoGenerador12+ Gen_Hidraulica12 + Gen_Termica12 + Gen_Eolica12 + Gen_Solar12+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train12 <- train$PBN12
x_test12 <- model.matrix(PBN12~Gen_CoGenerador12+ Gen_Hidraulica12 + Gen_Termica12 + Gen_Eolica12 + Gen_Solar12+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test12 <- pred$PBN12
xgb_train12 <- xgb.DMatrix(data = x_train12, label = y_train12)
xgb_test12 <- xgb.DMatrix(data = x_test12, label = y_test12) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist12 <-list(train=xgb_train12, test=xgb_test12)
model12<- xgb.train(data = xgb_train12, max.depth = 100, watchlist=watchlist12, nrounds = 1000)
saveRDS(model12,"../App_web/Pred_precio_bolsa_horario/data/model12.rds" )
predicciones_mod12 <-predict(model12, xgb_test12)
predicciones_mod12<- data.frame(predicciones_mod12)

##----13 hs----##
x_train13 <- model.matrix(PBN13~Gen_CoGenerador13+ Gen_Hidraulica13 + Gen_Termica13 + Gen_Eolica13 + Gen_Solar13+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train13 <- train$PBN12
x_test13 <- model.matrix(PBN13~Gen_CoGenerador13+ Gen_Hidraulica13 + Gen_Termica13 + Gen_Eolica13 + Gen_Solar13+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test13 <- pred$PBN13
xgb_train13 <- xgb.DMatrix(data = x_train13, label = y_train13)
xgb_test13 <- xgb.DMatrix(data = x_test13, label = y_test13) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist13 <-list(train=xgb_train13, test=xgb_test13)
model13<- xgb.train(data = xgb_train13, max.depth = 100, watchlist=watchlist13, nrounds = 1000)
saveRDS(model13,"../App_web/Pred_precio_bolsa_horario/data/model13.rds" )
predicciones_mod13 <-predict(model13, xgb_test13)
predicciones_mod13<- data.frame(predicciones_mod13)

##----14 hs----##
x_train14 <- model.matrix(PBN14~Gen_CoGenerador14+ Gen_Hidraulica14 + Gen_Termica14 + Gen_Eolica14 + Gen_Solar14+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train14 <- train$PBN14
x_test14 <- model.matrix(PBN14~Gen_CoGenerador14+ Gen_Hidraulica14 + Gen_Termica14 + Gen_Eolica14 + Gen_Solar14+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test14 <- pred$PBN14
xgb_train14 <- xgb.DMatrix(data = x_train14, label = y_train14)
xgb_test14 <- xgb.DMatrix(data = x_test14, label = y_test14) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist14 <-list(train=xgb_train14, test=xgb_test14)
model14<- xgb.train(data = xgb_train14, max.depth = 100, watchlist=watchlist14, nrounds = 1000)
saveRDS(model14,"../App_web/Pred_precio_bolsa_horario/data/model14.rds" )
predicciones_mod14 <-predict(model14, xgb_test14)
predicciones_mod14<- data.frame(predicciones_mod14)

##----15 hs----##
x_train15 <- model.matrix(PBN15~Gen_CoGenerador15+ Gen_Hidraulica15 + Gen_Termica15 + Gen_Eolica15 + Gen_Solar15+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train15 <- train$PBN15
x_test15 <- model.matrix(PBN15~Gen_CoGenerador15+ Gen_Hidraulica15 + Gen_Termica15 + Gen_Eolica15 + Gen_Solar15+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test15 <- pred$PBN15
xgb_train15 <- xgb.DMatrix(data = x_train15, label = y_train15)
xgb_test15 <- xgb.DMatrix(data = x_test15, label = y_test15) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist15 <-list(train=xgb_train15, test=xgb_test15)
model15<- xgb.train(data = xgb_train15, max.depth = 100, watchlist=watchlist15, nrounds = 1000)
saveRDS(model15,"../App_web/Pred_precio_bolsa_horario/data/model15.rds" )
predicciones_mod15 <-predict(model15, xgb_test15)
predicciones_mod15<- data.frame(predicciones_mod15)

##----16 hs----##
x_train16 <- model.matrix(PBN16~Gen_CoGenerador16+ Gen_Hidraulica16 + Gen_Termica16 + Gen_Eolica16 + Gen_Solar16+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train16 <- train$PBN16
x_test16 <- model.matrix(PBN16~Gen_CoGenerador16+ Gen_Hidraulica16 + Gen_Termica16+ Gen_Eolica16 + Gen_Solar16 + ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test16 <- pred$PBN16
xgb_train16 <- xgb.DMatrix(data = x_train16, label = y_train16)
xgb_test16<- xgb.DMatrix(data = x_test16, label = y_test16) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist16 <-list(train=xgb_train16, test=xgb_test16)
model16<- xgb.train(data = xgb_train16, max.depth = 10, watchlist=watchlist16, nrounds = 1000)
saveRDS(model16,"../App_web/Pred_precio_bolsa_horario/data/model16.rds" )
predicciones_mod16 <-predict(model16, xgb_test16)
predicciones_mod16<- data.frame(predicciones_mod16)


##----17 hs----##
x_train17 <- model.matrix(PBN17~Gen_CoGenerador17+ Gen_Hidraulica17 + Gen_Termica17 + Gen_Eolica17 + Gen_Solar17+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train17 <- train$PBN17
x_test17<- model.matrix(PBN17~Gen_CoGenerador17+ Gen_Hidraulica17 + Gen_Termica17 + Gen_Eolica17 + Gen_Solar17+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test17<- pred$PBN17
xgb_train17<- xgb.DMatrix(data = x_train17, label = y_train17)
xgb_test17 <- xgb.DMatrix(data = x_test17, label = y_test17) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist17 <-list(train=xgb_train17, test=xgb_test17)
model17<- xgb.train(data = xgb_train17, max.depth = 100, watchlist=watchlist17, nrounds = 1000)
saveRDS(model17,"../App_web/Pred_precio_bolsa_horario/data/model17.rds" )
predicciones_mod17 <-predict(model17, xgb_test17)
predicciones_mod17<- data.frame(predicciones_mod17)


##----18 hs----##
x_train18 <- model.matrix(PBN18~Gen_CoGenerador18+ Gen_Hidraulica18 + Gen_Termica18 + Gen_Eolica18 + Gen_Solar18+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train18 <- train$PBN18
x_test18 <- model.matrix(PBN18~Gen_CoGenerador18+ Gen_Hidraulica18 + Gen_Termica18 + Gen_Eolica18 + Gen_Solar18+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test18 <- pred$PBN18
xgb_train18 <- xgb.DMatrix(data = x_train18, label = y_train18)
xgb_test18 <- xgb.DMatrix(data = x_test18, label = y_test18) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist18 <-list(train=xgb_train18, test=xgb_test18)
model18<- xgb.train(data = xgb_train18, max.depth = 100, watchlist=watchlist18, nrounds = 1000)
saveRDS(model18,"../App_web/Pred_precio_bolsa_horario/data/model18.rds" )
predicciones_mod18 <-predict(model18, xgb_test18)
predicciones_mod18<- data.frame(predicciones_mod18)

##----19 hs----##
x_train19 <- model.matrix(PBN19~Gen_CoGenerador19+ Gen_Hidraulica19 + Gen_Termica19 + Gen_Eolica19 + Gen_Solar19+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train19 <- train$PBN19
x_test19 <- model.matrix(PBN19~Gen_CoGenerador19+ Gen_Hidraulica19 + Gen_Termica19 + Gen_Eolica19 + Gen_Solar19+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test19 <- pred$PBN19
xgb_train19 <- xgb.DMatrix(data = x_train19, label = y_train19)
xgb_test19 <- xgb.DMatrix(data = x_test19, label = y_test19) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist19 <-list(train=xgb_train19, test=xgb_test19)
model19<- xgb.train(data = xgb_train19, max.depth = 100, watchlist=watchlist19, nrounds = 1000)
saveRDS(model19,"../App_web/Pred_precio_bolsa_horario/data/model19.rds" )
predicciones_mod19 <-predict(model19, xgb_test19)
predicciones_mod19<- data.frame(predicciones_mod19)

##----20 hs----##
x_train20 <- model.matrix(PBN20~Gen_CoGenerador20+ Gen_Hidraulica20 + Gen_Termica20 + Gen_Eolica20 + Gen_Solar20+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train20 <- train$PBN20
x_test20 <- model.matrix(PBN20~Gen_CoGenerador20+ Gen_Hidraulica20 + Gen_Termica20 + Gen_Eolica20 + Gen_Solar20+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test20 <- pred$PBN20
xgb_train20 <- xgb.DMatrix(data = x_train20, label = y_train20)
xgb_test20 <- xgb.DMatrix(data = x_test20, label = y_test20) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist20 <-list(train=xgb_train20, test=xgb_test20)
model20<- xgb.train(data = xgb_train20, max.depth = 100, watchlist=watchlist20, nrounds = 1000)
saveRDS(model20,"../App_web/Pred_precio_bolsa_horario/data/model20.rds" )
predicciones_mod20 <-predict(model20, xgb_test20)
predicciones_mod20<- data.frame(predicciones_mod20)

##----21 hs----##
x_train21 <- model.matrix(PBN21~Gen_CoGenerador21+ Gen_Hidraulica21 + Gen_Termica21 + Gen_Eolica21 + Gen_Solar21+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train21 <- train$PBN21
x_test21 <- model.matrix(PBN21~Gen_CoGenerador21+ Gen_Hidraulica21 + Gen_Termica21 + Gen_Eolica21 + Gen_Solar21+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test21 <- pred$PBN21
xgb_train21 <- xgb.DMatrix(data = x_train21, label = y_train21)
xgb_test21 <- xgb.DMatrix(data = x_test21, label = y_test21) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist21 <-list(train=xgb_train21, test=xgb_test21)
model21<- xgb.train(data = xgb_train21, max.depth = 100, watchlist=watchlist21, nrounds = 1000)
saveRDS(model21,"../App_web/Pred_precio_bolsa_horario/data/model21.rds" )
predicciones_mod21 <-predict(model21, xgb_test21)
predicciones_mod21<- data.frame(predicciones_mod21)

##----22 hs----##
x_train22 <- model.matrix(PBN22~Gen_CoGenerador22+ Gen_Hidraulica22 + Gen_Termica22 + Gen_Eolica22 + Gen_Solar22+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train22 <- train$PBN22
x_test22 <- model.matrix(PBN22~Gen_CoGenerador22+ Gen_Hidraulica22 + Gen_Termica22+ Gen_Eolica22 + Gen_Solar22+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test22 <- pred$PBN22
xgb_train22 <- xgb.DMatrix(data = x_train22, label = y_train22)
xgb_test22 <- xgb.DMatrix(data = x_test22, label = y_test22) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist22 <-list(train=xgb_train22, test=xgb_test22)
model22<- xgb.train(data = xgb_train22, max.depth = 100, watchlist=watchlist22, nrounds = 1000)
saveRDS(model22,"../App_web/Pred_precio_bolsa_horario/data/model22.rds" )
predicciones_mod22 <-predict(model22, xgb_test22)
predicciones_mod22<- data.frame(predicciones_mod22)
##----23 hs----##
x_train23 <- model.matrix(~Gen_CoGenerador23+ Gen_Hidraulica23 + Gen_Termica23 + Gen_Eolica23+ Gen_Solar23+ ONI +TRM + Aportes_total+dia+mes+año, data =train)[, -1]
y_train23 <- train$PBN23
x_test23<- model.matrix(~Gen_CoGenerador23+ Gen_Hidraulica23 + Gen_Termica23 + Gen_Eolica23 + Gen_Solar23+ ONI + TRM + Aportes_total+dia+mes+año, data =pred)[, -1]
y_test23<- pred$PBN23
xgb_train23<- xgb.DMatrix(data = x_train23, label = y_train23)
xgb_test23 <- xgb.DMatrix(data = x_test23, label = y_test23) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train
watchlist23 <-list(train=xgb_train23, test=xgb_test23)
model23<- xgb.train(data = xgb_train23, max.depth = 100, watchlist=watchlist23, nrounds = 1000)
saveRDS(model23,"../App_web/Pred_precio_bolsa_horario/data/model23.rds" )
predicciones_mod23 <-predict(model23, xgb_test23)
predicciones_mod23<- data.frame(predicciones_mod23)

var=1
Prediccion_final<-case_when(var==1~1,
                            var==2~2)

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

