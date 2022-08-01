require(pacman)
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
Gen0<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_0_30062022.rds")
colnames(Gen0) <- c('Fecha', 'Gen_CoGenerador0','Gen_Hidraulica0', 'Gen_Termica0', 'Gen_Eolica0', 'Gen_Solar0')
Gen1<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_1_30062022.rds")
colnames(Gen1) <- c('...1', 'Gen_CoGenerador1','Gen_Hidraulica1', 'Gen_Termica1', 'Gen_Eolica1', 'Gen_Solar1')
Gen1$...1 <- NULL 
Gen2<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_2_30062022.rds")
colnames(Gen2) <- c('Fecha', 'Gen_CoGenerador2','Gen_Hidraulica2', 'Gen_Termica2', 'Gen_Eolica2', 'Gen_Solar2')
Gen2$Fecha <- NULL 

Gen3<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_3_30062022.rds")
colnames(Gen3) <- c('Fecha', 'Gen_CoGenerador3','Gen_Hidraulica3', 'Gen_Termica3', 'Gen_Eolica3', 'Gen_Solar3')
Gen3$Fecha <- NULL 

Gen4<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_4_30062022.rds")
colnames(Gen4) <- c('Fecha', 'Gen_CoGenerador4','Gen_Hidraulica4', 'Gen_Termica4', 'Gen_Eolica4', 'Gen_Solar4')
Gen4$Fecha <- NULL 

Gen5<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_5_30062022.rds")
colnames(Gen5) <- c('Fecha', 'Gen_CoGenerador5','Gen_Hidraulica5', 'Gen_Termica5', 'Gen_Eolica5', 'Gen_Solar5')
Gen5$Fecha <- NULL 

Gen6<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_6_30062022.rds")
colnames(Gen6) <- c('Fecha', 'Gen_CoGenerador6','Gen_Hidraulica6', 'Gen_Termica6', 'Gen_Eolica6', 'Gen_Solar6')
Gen6$Fecha <- NULL 

Gen7<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_7_30062022.rds")
colnames(Gen7) <- c('Fecha', 'Gen_CoGenerador7','Gen_Hidraulica7', 'Gen_Termica7', 'Gen_Eolica7', 'Gen_Solar7')
Gen7$Fecha <- NULL 

Gen8<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_8_30062022.rds")
colnames(Gen8) <- c('Fecha', 'Gen_CoGenerador8','Gen_Hidraulica8', 'Gen_Termica8', 'Gen_Eolica8', 'Gen_Solar8')
Gen8$Fecha <- NULL 

Gen9<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_9_30062022.rds")
colnames(Gen9) <- c('Fecha', 'Gen_CoGenerador9','Gen_Hidraulica9', 'Gen_Termica9', 'Gen_Eolica9', 'Gen_Solar9')
Gen9$Fecha <- NULL 

Gen10<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_10_30062022.rds")
colnames(Gen10) <- c('Fecha', 'Gen_CoGenerador10','Gen_Hidraulica10', 'Gen_Termica10', 'Gen_Eolica10', 'Gen_Solar10')
Gen10$Fecha <- NULL 

Gen11<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_11_30062022.rds")
colnames(Gen11) <- c('Fecha', 'Gen_CoGenerador11','Gen_Hidraulica11', 'Gen_Termica11', 'Gen_Eolica11', 'Gen_Solar11')
Gen11$Fecha <- NULL 

Gen12<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_12_30062022.rds")
colnames(Gen12) <- c('Fecha', 'Gen_CoGenerador12','Gen_Hidraulica12', 'Gen_Termica12', 'Gen_Eolica12', 'Gen_Solar12')
Gen12$Fecha <- NULL 

Gen13<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_13_30062022.rds")
colnames(Gen13) <- c('Fecha', 'Gen_CoGenerador13','Gen_Hidraulica13', 'Gen_Termica13', 'Gen_Eolica13', 'Gen_Solar13')
Gen13$Fecha <- NULL 

Gen14<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_14_30062022.rds")
colnames(Gen14) <- c('Fecha', 'Gen_CoGenerador14','Gen_Hidraulica14', 'Gen_Termica14', 'Gen_Eolica14', 'Gen_Solar14')
Gen14$Fecha <- NULL 

Gen15<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_15_30062022.rds")
colnames(Gen15) <- c('Fecha', 'Gen_CoGenerador15','Gen_Hidraulica15', 'Gen_Termica15', 'Gen_Eolica15', 'Gen_Solar15')
Gen15$Fecha <- NULL 

Gen16<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_16_30062022.rds")
colnames(Gen16) <- c('Fecha', 'Gen_CoGenerador16','Gen_Hidraulica16', 'Gen_Termica16', 'Gen_Eolica16', 'Gen_Solar16')
Gen16$Fecha <- NULL 

Gen17<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_17_30062022.rds")
colnames(Gen17) <- c('Fecha', 'Gen_CoGenerador17','Gen_Hidraulica17', 'Gen_Termica17', 'Gen_Eolica17', 'Gen_Solar17')
Gen17$Fecha <- NULL 

Gen18<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_18_30062022.rds")
colnames(Gen18) <- c('Fecha', 'Gen_CoGenerador18','Gen_Hidraulica18', 'Gen_Termica18', 'Gen_Eolica18', 'Gen_Solar18')
Gen18$Fecha <- NULL 

Gen19<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_19_30062022.rds")
colnames(Gen19) <- c('Fecha', 'Gen_CoGenerador19','Gen_Hidraulica19', 'Gen_Termica19', 'Gen_Eolica19', 'Gen_Solar19')
Gen19$Fecha <- NULL 

Gen20<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_20_30062022.rds")
colnames(Gen20) <- c('Fecha', 'Gen_CoGenerador20','Gen_Hidraulica20', 'Gen_Termica20', 'Gen_Eolica20', 'Gen_Solar20')
Gen20$Fecha <- NULL 

Gen21<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_21_30062022.rds")
colnames(Gen21) <- c('Fecha', 'Gen_CoGenerador21','Gen_Hidraulica21', 'Gen_Termica21', 'Gen_Eolica21', 'Gen_Solar21')
Gen21$Fecha <- NULL 

Gen22<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_22_30062022.rds")
colnames(Gen22) <- c('Fecha', 'Gen_CoGenerador22','Gen_Hidraulica22', 'Gen_Termica22', 'Gen_Eolica22', 'Gen_Solar22')
Gen22$Fecha <- NULL 

Gen23<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_23_30062022.rds")
colnames(Gen23) <- c('Fecha', 'Gen_CoGenerador23','Gen_Hidraulica23', 'Gen_Termica23', 'Gen_Eolica23', 'Gen_Solar23')
Gen23$Fecha <- NULL 

GEN<- cbind(Gen0, Gen1, Gen2, Gen3, Gen4, Gen5, Gen6, Gen7, Gen8, Gen9, Gen10, Gen11, Gen12, Gen13, Gen14, Gen15, Gen16, Gen17, Gen18, Gen19,Gen20, Gen21, Gen22, Gen23)
saveRDS(GEN, "../Datos/Bases oficiales/GENCOMPLETO.rds" )
####PRECIOS NACIONALES DE BOLSA
PB<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Precios_Bolsa_2.rds")
colnames(PB) <- c('Fecha', 'PBN0', 'PBN1','PBN2', 'PBN3', 'PBN4', 'PBN5', 'PBN6', 'PBN7', 'PBN8', 'PBN9', 'PBN10', 'PBN11', 'PBN12', 'PBN13', 'PBN14', 'PBN15', 'PBN16', 'PBN17', 'PBN18', 'PBN19','PBN20', 'PBN21', 'PBN22','PBN23')
PB <- PB[c(-1),]
PB <- PB[c(-8219:-8246),]
PB <- PB[c(-8218),]
saveRDS(PB, "../Datos/Bases oficiales/PBof.rds" )
 PB$Fecha <- NULL
ONI<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/ONI.rds")
TRM <- readRDS(("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/TRM.rds"))
TRM <- TRM[c(-8218:-8247),]
AD <- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Aportes_energia_dia_30_06_2022.rds")
B_Of<- cbind(GEN, PB, ONI$ONI, TRM$TRM , AD$Aportes_total)
saveRDS(B_Of, "../Datos/Bases oficiales/Base_de_datos_oficial.rds" )

#####
BASEOF<- data.frame(BASEOF)
colnames(BASEOF) <- c('Fecha', 'Precio_Bolsa_Nacional', 'Gen_CoGenerador','Gen_Hidraulica', 'Gen_Termica', 'Gen_Eolica', 'Gen_Solar', 'Aportes_Diarios', 'ONI', 'TRM')
saveRDS(BASEOF, "../Datos/Bases oficiales/BASEOF.rds" )

