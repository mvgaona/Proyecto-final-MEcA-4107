# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### TRABAJO FINAL #####
BOF<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/base_de_datos_oficial_3.rds")
TRM<- BOF$TRM 
class(TRM)
plot(hist(TRM),col = "blue", main="Histograma TRM desde 01/01/2000- 30/06/2022",
     xlab="TRM",
     ylab="Frecuencia")
min(TRM)
max(TRM)
mean(TRM)
modeTRM <- function(TRM){
  return(as.numeric(names(which.max(table(TRM)))))}
modeTRM(TRM)
summary(TRM)
##----aPORTES TOTALES----#
AT<- BOF$Aportes_total
class(AT)
plot(hist(AT),col = "purple", main="Histograma Aportes totales (kWh) desde 01/01/2000- 30/06/2022",
     xlab="Aportes Totales",
     ylab="Frecuencia")
min(AT)
max(AT)
mean(AT)
modeAT <- function(AT){
  return(as.numeric(names(which.max(table(AT)))))}
modeTRM(AT)
summary(AT)
##----ONI----##
ONI<- BOF$ONI
class(ONI)
plot(hist(ONI),col = "purple", main="Histograma índice ONI desde 01/01/2000- 30/06/2022",
     xlab="Índice ONI",
     ylab="Frecuencia")
min(ONI)
max(ONI)
mean(ONI)
modeAT <- function(ONI){
  return(as.numeric(names(which.max(table(ONI)))))}
modeTRM(ONI)
summary(ONI)
##----Generación por tipo de recursos----##