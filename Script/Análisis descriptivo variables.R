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
modeONI <- function(ONI){
  return(as.numeric(names(which.max(table(ONI)))))}
modeONI(ONI)
summary(ONI)
##----Generación por tipo de recursos----##
GENCOMPLETO <- readRDS("C:/Users/norma/OneDrive/Escritorio/Valeria/Proyecto-final-MEcA-4107/Datos/Bases oficiales/GENCOMPLETO.rds")
#-CoGenerador
#Hora 0
COGEN0<- GENCOMPLETO$Gen_CoGenerador0
class(COGEN0)
plot(hist(COGEN0),col = "purple", main="Histograma CoGenerador hora0 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador0",
     ylab="Frecuencia")
min(COGEN0)
max(COGEN0)
mean(COGEN0)
modeCOGEN0 <- function(COGEN0){
  return(as.numeric(names(which.max(table(COGEN0)))))}
modeCOGEN0(COGEN0)
#Hora 1
COGEN1<- GENCOMPLETO$Gen_CoGenerador2
class(COGEN1)
plot(hist(COGEN1),col = "purple", main="Histograma CoGenerador hora1 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 1",
     ylab="Frecuencia")
summary(COGEN1)
modeCOGEN1 <- function(COGEN1){
  return(as.numeric(names(which.max(table(COGEN1)))))}
modeCOGEN1(COGEN1)
#Hora2
COGEN2<- GENCOMPLETO$Gen_CoGenerador2
class(COGEN2)
plot(hist(COGEN2),col = "purple", main="Histograma CoGenerador hora2 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 2",
     ylab="Frecuencia")
summary(COGEN2)
modeCOGEN2 <- function(COGEN2){
  return(as.numeric(names(which.max(table(COGEN2)))))}
modeCOGEN1(COGEN2)
#Hora3
COGEN3<- GENCOMPLETO$Gen_CoGenerador3
class(COGEN3)
plot(hist(COGEN3),col = "purple", main="Histograma CoGenerador hora 3 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 3",
     ylab="Frecuencia")
summary(COGEN3)
modeCOGEN3 <- function(COGEN3){
  return(as.numeric(names(which.max(table(COGEN3)))))}
modeCOGEN1(COGEN3)
#Hora4
COGEN4<- GENCOMPLETO$Gen_CoGenerador4
class(COGEN4)
plot(hist(COGEN4),col = "purple", main="Histograma CoGenerador hora 4 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 4",
     ylab="Frecuencia")
summary(COGEN4)

modeCOGEN3 <- function(COGEN4){
  return(as.numeric(names(which.max(table(COGEN4)))))}
modeCOGEN1(COGEN4)
install.packages(stargazer )
