# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

####==TRABAJO FINAL==#####
#En el siguiente código se presenta la descripción de las variables que se utilizarán en los 24 modelos.
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
GENCOMPLETO <- readRDS(".../Bases oficiales/GENCOMPLETO.rds")
##========CoGenerador=======##
#Hora 0
#Hora 0
COGEN0<- GENCOMPLETO$Gen_CoGenerador0
class(COGEN0)
plot(hist(COGEN0),col = "purple", main="Histograma CoGenerador hora0 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador0",
     ylab="Frecuencia")
MinCOGEN0<-min(COGEN0)
MaxCOGEN0<-max(COGEN0)
MediaCOGEN0<-mean(COGEN0)
modeCOGEN0 <- function(COGEN0){
  return(as.numeric(names(which.max(table(COGEN0)))))}
modaCOGEN0<-modeCOGEN0(COGEN0)
ADCOGEN0<- cbind(summary(COGEN0))
ADCOGEN0<- rbind(ADCOGEN0, modaCOGEN0)
ADCOGEN0<- data.frame(ADCOGEN0)
summary(COGEN0)
rownames(ADCOGEN0)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN0)<- c()
#Hora 1
COGEN1<- GENCOMPLETO$Gen_CoGenerador1
class(COGEN1)
plot(hist(COGEN1),col = "purple", main="Histograma CoGenerador hora1 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 1",
     ylab="Frecuencia")
MinCOGEN1<-min(COGEN1)
MaxCOGEN1<-max(COGEN1)
MediaCOGEN1<-mean(COGEN1)
summary(COGEN1)
modeCOGEN1 <- function(COGEN1){
  return(as.numeric(names(which.max(table(COGEN1)))))}
modaCOGEN1<-modeCOGEN1(COGEN1)
ADCOGEN1<- cbind(summary(COGEN1))
ADCOGEN1<- rbind(ADCOGEN1, modaCOGEN1)
ADCOGEN1<- data.frame(ADCOGEN1)
rownames(ADCOGEN1)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN1) <- c("AD. CoGenerador1")
#Hora2
COGEN2<- GENCOMPLETO$Gen_CoGenerador2
class(COGEN2)
plot(hist(COGEN2),col = "purple", main="Histograma CoGenerador hora2 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 2",
     ylab="Frecuencia")
summary(COGEN2)
MinCOGEN2<-min(COGEN2)
MaxCOGEN2<-max(COGEN2)
MediaCOGEN2<-mean(COGEN2)
modeCOGEN2 <- function(COGEN2){
  return(as.numeric(names(which.max(table(COGEN2)))))}
modaCOGEN2<-modeCOGEN2(COGEN2)
ADCOGEN2<- cbind(summary(COGEN2))
ADCOGEN2<- rbind(ADCOGEN2, modaCOGEN2)
ADCOGEN2<- data.frame(ADCOGEN2)
rownames(ADCOGEN2)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN2) <- c("AD. CoGenerador2")

#Hora3
COGEN3<- GENCOMPLETO$Gen_CoGenerador3
class(COGEN3)
plot(hist(COGEN3),col = "purple", main="Histograma CoGenerador hora 3 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 3",
     ylab="Frecuencia")
summary(COGEN3)
MinCOGEN3<-min(COGEN3)
MaxCOGEN3<-max(COGEN3)
MediaCOGEN3<-mean(COGEN3)
modeCOGEN3 <- function(COGEN3){
  return(as.numeric(names(which.max(table(COGEN3)))))}
modaCOGEN3<-modeCOGEN3(COGEN3)
ADCOGEN3<- cbind(summary(COGEN3))
ADCOGEN3<- rbind(ADCOGEN3, modaCOGEN3)
ADCOGEN3<- data.frame(ADCOGEN3)
rownames(ADCOGEN3)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN3) <- c("AD. CoGenerador3")
#Hora4
COGEN4<- GENCOMPLETO$Gen_CoGenerador4
class(COGEN4)
plot(hist(COGEN4),col = "purple", main="Histograma CoGenerador hora 4 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 4",
     ylab="Frecuencia")
summary(COGEN4)
MinCOGEN4<-min(COGEN4)
MaxCOGEN4<-max(COGEN4)
MediaCOGEN4<-mean(COGEN4)
modeCOGEN4 <- function(COGEN4){
  return(as.numeric(names(which.max(table(COGEN4)))))}
modaCOGEN4<-modeCOGEN4(COGEN4)

ADCOGEN4<- cbind(summary(COGEN4))
ADCOGEN4<- rbind(ADCOGEN4, modaCOGEN4)
ADCOGEN4<- data.frame(ADCOGEN4)
rownames(ADCOGEN4)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN4) <- c("AD. CoGenerador4")

#Hora5
COGEN5<- GENCOMPLETO$Gen_CoGenerador5
class(COGEN5)
plot(hist(COGEN5),col = "purple", main="Histograma CoGenerador hora 5 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 5",
     ylab="Frecuencia")
summary(COGEN5)
MinCOGEN5<-min(COGEN5)
MaxCOGEN5<-max(COGEN5)
MediaCOGEN5<-mean(COGEN5)
modeCOGEN5 <- function(COGEN5){
  return(as.numeric(names(which.max(table(COGEN5)))))}
modaCOGEN5<-modeCOGEN5(COGEN5)
ADCOGEN5<- cbind(summary(COGEN5))
ADCOGEN5<- rbind(ADCOGEN5, modaCOGEN5)
ADCOGEN5<- data.frame(ADCOGEN5)
rownames(ADCOGEN5)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN5) <- c("AD. CoGenerador5")
#Hora6
COGEN6<- GENCOMPLETO$Gen_CoGenerador6
class(COGEN6)
plot(hist(COGEN6),col = "purple", main="Histograma CoGenerador hora 6 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 6",
     ylab="Frecuencia")
summary(COGEN6)
MinCOGEN6<-min(COGEN6)
MaxCOGEN6<-max(COGEN6)
MediaCOGEN6<-mean(COGEN6)
modeCOGEN6 <- function(COGEN6){
  return(as.numeric(names(which.max(table(COGEN6)))))}
modaCOGEN6<-modeCOGEN6(COGEN6)
ADCOGEN6<- cbind(summary(COGEN6))
ADCOGEN6<- rbind(ADCOGEN6, modaCOGEN6)
ADCOGEN6<- data.frame(ADCOGEN6)
rownames(ADCOGEN6)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN6) <- c("AD. CoGenerador6")

#Hora7
COGEN7<- GENCOMPLETO$Gen_CoGenerador7
class(COGEN7)
plot(hist(COGEN7),col = "purple", main="Histograma CoGenerador hora 7 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 7",
     ylab="Frecuencia")
summary(COGEN7)
MinCOGEN7<-min(COGEN7)
MaxCOGEN7<-max(COGEN7)
MediaCOGEN7<-mean(COGEN7)
modeCOGEN7 <- function(COGEN7){
  return(as.numeric(names(which.max(table(COGEN7)))))}
modaCOGEN7<-modeCOGEN7(COGEN7)
ADCOGEN7<- cbind(summary(COGEN7))
ADCOGEN7<- rbind(ADCOGEN7, modaCOGEN7)
ADCOGEN7<- data.frame(ADCOGEN7)
rownames(ADCOGEN7)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN7) <- c("AD. CoGenerador7")

             ##--TABLA 1--##
T1<- cbind(ADCOGEN0, ADCOGEN1, ADCOGEN2, ADCOGEN3, ADCOGEN4, ADCOGEN5, ADCOGEN6, ADCOGEN7)
colnames(T1)<- c("AD. CoGenerador0", "AD. CoGenerador1", "AD. CoGenerador2", "AD. CoGenerador3", "AD. CoGenerador4", "AD. CoGenerador5","AD. CoGenerador6", "AD. CoGenerador7")
rownames(T1)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
stargazer(T1, data = "latex")
require(xls)
write.xlsx(T1, "T1.xlsx")

                 #####-----####
#Hora8
COGEN8<- GENCOMPLETO$Gen_CoGenerador8
class(COGEN8)
plot(hist(COGEN8),col = "purple", main="Histograma CoGenerador hora 8 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 8",
     ylab="Frecuencia")
summary(COGEN8)

MinCOGEN8<-min(COGEN8)
MaxCOGEN8<-max(COGEN8)
MediaCOGEN8<-mean(COGEN8)
modeCOGEN8<- function(COGEN8){
  return(as.numeric(names(which.max(table(COGEN8)))))}
modaCOGEN8<-modeCOGEN8(COGEN8)
ADCOGEN8<- cbind(summary(COGEN8))
ADCOGEN8<- rbind(ADCOGEN8, modaCOGEN8)
ADCOGEN8<- data.frame(ADCOGEN8)
rownames(ADCOGEN8)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN8) <- c("AD. CoGenerador8")
#Hora9
COGEN9<- GENCOMPLETO$Gen_CoGenerador9
class(COGEN9)
plot(hist(COGEN9),col = "purple", main="Histograma CoGenerador hora 9 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 9",
     ylab="Frecuencia")
summary(COGEN9)
MinCOGEN9<-min(COGEN9)
MaxCOGEN9<-max(COGEN9)
MediaCOGEN9<-mean(COGEN9)
modeCOGEN9<- function(COGEN9){
  return(as.numeric(names(which.max(table(COGEN9)))))}
modaCOGEN9<-modeCOGEN9(COGEN9)
ADCOGEN9<- cbind(summary(COGEN9))
ADCOGEN9<- rbind(ADCOGEN9, modaCOGEN9)
ADCOGEN9<- data.frame(ADCOGEN9)
rownames(ADCOGEN7)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN7) <- c("AD. CoGenerador9")
#Hora10
COGEN10<- GENCOMPLETO$Gen_CoGenerador10
class(COGEN10)
plot(hist(COGEN10),col = "purple", main="Histograma CoGenerador hora 10 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 10",
     ylab="Frecuencia")
summary(COGEN10)
MinCOGEN10<-min(COGEN10)
MaxCOGEN10<-max(COGEN10)
MediaCOGEN10<-mean(COGEN10)
modeCOGEN10<- function(COGEN10){
  return(as.numeric(names(which.max(table(COGEN10)))))}
modaCOGEN10<-modeCOGEN10(COGEN10)
ADCOGEN10<- cbind(summary(COGEN10))
ADCOGEN10<- rbind(ADCOGEN10, modaCOGEN10)
ADCOGEN10<- data.frame(ADCOGEN10)
rownames(ADCOGEN10)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN10) <- c("AD. CoGenerador10")

#Hora11
COGEN11<- GENCOMPLETO$Gen_CoGenerador11
class(COGEN11)
plot(hist(COGEN11),col = "purple", main="Histograma CoGenerador hora 11 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 11",
     ylab="Frecuencia")
summary(COGEN11)
MinCOGEN11<-min(COGEN11)
MaxCOGEN11<-max(COGEN11)
MediaCOGEN11<-mean(COGEN11)
modeCOGEN11<- function(COGEN11){
  return(as.numeric(names(which.max(table(COGEN11)))))}
modaCOGEN11<-modeCOGEN11(COGEN11)
ADCOGEN11<- cbind(summary(COGEN11))
ADCOGEN11<- rbind(ADCOGEN11, modaCOGEN11)
ADCOGEN11<- data.frame(ADCOGEN11)
rownames(ADCOGEN11)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN11) <- c("AD. CoGenerador11")

#Hora12
COGEN12<- GENCOMPLETO$Gen_CoGenerador12
class(COGEN12)
plot(hist(COGEN12),col = "purple", main="Histograma CoGenerador hora 12 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 12",
     ylab="Frecuencia")
summary(COGEN12)
MinCOGEN12<-min(COGEN12)
MaxCOGEN12<-max(COGEN12)
MediaCOGEN12<-mean(COGEN12)
modeCOGEN12<- function(COGEN12){
  return(as.numeric(names(which.max(table(COGEN12)))))}
modaCOGEN12<-modeCOGEN12(COGEN12)
ADCOGEN12<- cbind(summary(COGEN12))
ADCOGEN12<- rbind(ADCOGEN12, modaCOGEN12)
ADCOGEN12<- data.frame(ADCOGEN12)
rownames(ADCOGEN12)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN12) <- c("AD. CoGenerador12")


#Hora13
COGEN13<- GENCOMPLETO$Gen_CoGenerador13
class(COGEN13)
plot(hist(COGEN13),col = "purple", main="Histograma CoGenerador hora 13 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 13",
     ylab="Frecuencia")
summary(COGEN13)
MinCOGEN13<-min(COGEN13)
MaxCOGEN13<-max(COGEN13)
MediaCOGEN13<-mean(COGEN13)
modeCOGEN13<- function(COGEN13){
  return(as.numeric(names(which.max(table(COGEN13)))))}
modaCOGEN13<-modeCOGEN13(COGEN13)
ADCOGEN13<- cbind(summary(COGEN13))
ADCOGEN13<- rbind(ADCOGEN13, modaCOGEN13)
ADCOGEN13<- data.frame(ADCOGEN13)
rownames(ADCOGEN13)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN13) <- c("AD. CoGenerador13")

#Hora14
COGEN14<- GENCOMPLETO$Gen_CoGenerador14
class(COGEN14)
plot(hist(COGEN14),col = "purple", main="Histograma CoGenerador hora 14 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 14",
     ylab="Frecuencia")
summary(COGEN14)
MinCOGEN14<-min(COGEN14)
MaxCOGEN14<-max(COGEN14)
MediaCOGEN14<-mean(COGEN14)
modeCOGEN14<- function(COGEN14){
  return(as.numeric(names(which.max(table(COGEN14)))))}
modaCOGEN14<-modeCOGEN14(COGEN14)
ADCOGEN14<- cbind(summary(COGEN14))
ADCOGEN14<- rbind(ADCOGEN14, modaCOGEN14)
ADCOGEN14<- data.frame(ADCOGEN14)
rownames(ADCOGEN14)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN14) <- c("AD. CoGenerador14")

                   ###----TABLA 2---###
T2<- cbind(ADCOGEN8, ADCOGEN9, ADCOGEN10, ADCOGEN11, ADCOGEN12, ADCOGEN13, ADCOGEN14)
colnames(T2)<- c("AD. CoGenerador8", "AD. CoGenerador9", "AD. CoGenerador10", "AD. CoGenerador11", "AD. CoGenerador12", "AD. CoGenerador13","AD. CoGenerador14")
rownames(T2)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
stargazer(T2, data = "latex")
require(xls)
write.xlsx(T2, "T2.xlsx")
                  ###------------###
#Hora15
COGEN15<- GENCOMPLETO$Gen_CoGenerador15
class(COGEN15)
plot(hist(COGEN15),col = "purple", main="Histograma CoGenerador hora 15 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 15",
     ylab="Frecuencia")
summary(COGEN15)
MinCOGEN15<-min(COGEN15)
MaxCOGEN15<-max(COGEN15)
MediaCOGEN15<-mean(COGEN15)
modeCOGEN15<- function(COGEN15){
  return(as.numeric(names(which.max(table(COGEN15)))))}
modaCOGEN15<-modeCOGEN15(COGEN15)
ADCOGEN15<- cbind(summary(COGEN15))
ADCOGEN15<- rbind(ADCOGEN15, modaCOGEN15)
ADCOGEN15<- data.frame(ADCOGEN15)
rownames(ADCOGEN15)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN15) <- c("AD. CoGenerador15")

#Hora16
COGEN16<- GENCOMPLETO$Gen_CoGenerador16
class(COGEN16)
plot(hist(COGEN16),col = "purple", main="Histograma CoGenerador hora 16 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 16",
     ylab="Frecuencia")
summary(COGEN16)
MinCOGEN16<-min(COGEN16)
MaxCOGEN16<-max(COGEN16)
MediaCOGEN16<-mean(COGEN16)
modeCOGEN16<- function(COGEN16){
  return(as.numeric(names(which.max(table(COGEN16)))))}
modaCOGEN16<-modeCOGEN16(COGEN16)
ADCOGEN16<- cbind(summary(COGEN16))
ADCOGEN16<- rbind(ADCOGEN16, modaCOGEN16)
ADCOGEN16<- data.frame(ADCOGEN16)
rownames(ADCOGEN16)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN16) <- c("AD. CoGenerador16")

#Hora17
COGEN17<- GENCOMPLETO$Gen_CoGenerador17
class(COGEN17)
plot(hist(COGEN17),col = "purple", main="Histograma CoGenerador hora 17 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 17",
     ylab="Frecuencia")
summary(COGEN17)
MinCOGEN17<-min(COGEN17)
MaxCOGEN17<-max(COGEN17)
MediaCOGEN17<-mean(COGEN17)
modeCOGEN17<- function(COGEN17){
  return(as.numeric(names(which.max(table(COGEN17)))))}
modaCOGEN17<-modeCOGEN17(COGEN17)
ADCOGEN17<- cbind(summary(COGEN17))
ADCOGEN17<- rbind(ADCOGEN17, modaCOGEN17)
ADCOGEN17<- data.frame(ADCOGEN17)
rownames(ADCOGEN17)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN17) <- c("AD. CoGenerador17")

#Hora18
COGEN18<- GENCOMPLETO$Gen_CoGenerador18
class(COGEN18)
plot(hist(COGEN18),col = "purple", main="Histograma CoGenerador hora 18 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 18",
     ylab="Frecuencia")
summary(COGEN18)
MinCOGEN18<-min(COGEN18)
MaxCOGEN18<-max(COGEN18)
MediaCOGEN18<-mean(COGEN18)
modeCOGEN18<- function(COGEN18){
  return(as.numeric(names(which.max(table(COGEN18)))))}
modaCOGEN18<-modeCOGEN18(COGEN18)
ADCOGEN18<- cbind(summary(COGEN18))
ADCOGEN18<- rbind(ADCOGEN18, modaCOGEN18)
ADCOGEN18<- data.frame(ADCOGEN18)
rownames(ADCOGEN18)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN18) <- c("AD. CoGenerador18")


#Hora19
COGEN19<- GENCOMPLETO$Gen_CoGenerador19
class(COGEN19)
plot(hist(COGEN19),col = "purple", main="Histograma CoGenerador hora 19 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 19",
     ylab="Frecuencia")
summary(COGEN19)
MinCOGEN19<-min(COGEN19)
MaxCOGEN19<-max(COGEN19)
MediaCOGEN19<-mean(COGEN19)
modeCOGEN19<- function(COGEN19){
  return(as.numeric(names(which.max(table(COGEN19)))))}
modaCOGEN19<-modeCOGEN19(COGEN19)
ADCOGEN19<- cbind(summary(COGEN19))
ADCOGEN19<- rbind(ADCOGEN19, modaCOGEN19)
ADCOGEN19<- data.frame(ADCOGEN19)
rownames(ADCOGEN19)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN19) <- c("AD. CoGenerador19")

                        ###---TABLA 3---###
T3<- cbind(ADCOGEN15, ADCOGEN16, ADCOGEN17, ADCOGEN18, ADCOGEN19)
colnames(T3)<- c("AD. CoGenerador15", "AD. CoGenerador16", "AD. CoGenerador17", "AD. CoGenerador18", "AD. CoGenerador19")
rownames(T3)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
stargazer(T3, data = "latex")
require(xls)
write.xlsx(T3, "T3.xlsx")
                        ###------------###

#Hora20
COGEN20<- GENCOMPLETO$Gen_CoGenerador20
class(COGEN20)
plot(hist(COGEN20),col = "purple", main="Histograma CoGenerador hora 20 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 20",
     ylab="Frecuencia")
summary(COGEN20)
MinCOGEN20<-min(COGEN20)
MaxCOGEN20<-max(COGEN20)
MediaCOGEN20<-mean(COGEN20)
modeCOGEN20<- function(COGEN20){
  return(as.numeric(names(which.max(table(COGEN20)))))}
modaCOGEN20<-modeCOGEN20(COGEN20)

ADCOGEN20<- cbind(summary(COGEN20))
ADCOGEN20<- rbind(ADCOGEN20, modaCOGEN20)
ADCOGEN20<- data.frame(ADCOGEN20)
rownames(ADCOGEN20)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN20) <- c("AD. CoGenerador20")

#Hora21
COGEN21<- GENCOMPLETO$Gen_CoGenerador21
class(COGEN21)
plot(hist(COGEN21),col = "purple", main="Histograma CoGenerador hora 21 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 21",
     ylab="Frecuencia")
summary(COGEN21)
MinCOGEN21<-min(COGEN21)
MaxCOGEN21<-max(COGEN21)
MediaCOGEN21<-mean(COGEN21)
modeCOGEN21<- function(COGEN21){
  return(as.numeric(names(which.max(table(COGEN21)))))}
modaCOGEN21<-modeCOGEN21(COGEN21)
ADCOGEN21<- cbind(summary(COGEN21))
ADCOGEN21<- rbind(ADCOGEN21, modaCOGEN21)
ADCOGEN21<- data.frame(ADCOGEN21)
rownames(ADCOGEN21)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN21) <- c("AD. CoGenerador21")

#Hora22
COGEN22<- GENCOMPLETO$Gen_CoGenerador22
class(COGEN22)
plot(hist(COGEN22),col = "purple", main="Histograma CoGenerador hora 22 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 22",
     ylab="Frecuencia")
summary(COGEN22)
MinCOGEN22<-min(COGEN22)
MaxCOGEN22<-max(COGEN22)
MediaCOGEN22<-mean(COGEN22)
modeCOGEN22<- function(COGEN22){
  return(as.numeric(names(which.max(table(COGEN22)))))}
modaCOGEN22<-modeCOGEN22(COGEN22)
ADCOGEN22<- cbind(summary(COGEN22))
ADCOGEN22<- rbind(ADCOGEN22, modaCOGEN22)
ADCOGEN22<- data.frame(ADCOGEN22)
rownames(ADCOGEN22)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN22) <- c("AD. CoGenerador22")

#Hora23
COGEN23<- GENCOMPLETO$Gen_CoGenerador23
class(COGEN23)
plot(hist(COGEN23),col = "purple", main="Histograma CoGenerador hora 23 desde 01/01/2000- 30/06/2022",
     xlab="CoGenerador 23",
     ylab="Frecuencia")
summary(COGEN23)
MinCOGEN23<-min(COGEN23)
MaxCOGEN23<-max(COGEN23)
MediaCOGEN23<-mean(COGEN23)
modeCOGEN23<- function(COGEN23){
  return(as.numeric(names(which.max(table(COGEN23)))))}
modaCOGEN23<-modeCOGEN23(COGEN23)
ADCOGEN23<- cbind(summary(COGEN23))
ADCOGEN23<- rbind(ADCOGEN23, modaCOGEN23)
ADCOGEN23<- data.frame(ADCOGEN23)
rownames(ADCOGEN23)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADCOGEN23) <- c("AD. CoGenerador23")
                       ####----TABLA4----####
T4<- cbind(ADCOGEN20, ADCOGEN21, ADCOGEN22, ADCOGEN23)
colnames(T4)<- c("AD. CoGenerador20", "AD. CoGenerador21", "AD. CoGenerador22", "AD. CoGenerador23")
rownames(T4)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
stargazer(T4, data = "latex")
require(xls)
write.xlsx(T4, "T4.xlsx")
                        ###------------###
##===========GEN HIDRÁULICA=========##
#Hora 0
GENHIDRA0<- GENCOMPLETO$Gen_Hidraulica0
class(GENHIDRA0)
plot(hist(GENHIDRA0),col = "purple", main="Histograma GENHIDRAerador hora0 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador0",
     ylab="Frecuencia")
MinGENHIDRA0<-min(GENHIDRA0)
MaxGENHIDRA0<-max(GENHIDRA0)
MediaGENHIDRA0<-mean(GENHIDRA0)
modeGENHIDRA0 <- function(GENHIDRA0){
  return(as.numeric(names(which.max(table(GENHIDRA0)))))}
modaGENHIDRA0<-modeGENHIDRA0(GENHIDRA0)
#Hora 1
GENHIDRA1<- GENCOMPLETO$Gen_Hidraulica1
class(GENHIDRA1)
plot(hist(GENHIDRA1),col = "purple", main="Histograma GENHIDRAerador hora1 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 1",
     ylab="Frecuencia")
MinGENHIDRA1<-min(GENHIDRA1)
MaxGENHIDRA1<-max(GENHIDRA1)
MediaGENHIDRA1<-mean(GENHIDRA1)
summary(GENHIDRA1)
modeGENHIDRA1 <- function(GENHIDRA1){
  return(as.numeric(names(which.max(table(GENHIDRA1)))))}
modaGENHIDRA1<-modeGENHIDRA1(GENHIDRA1)
#Hora2
GENHIDRA2<- GENCOMPLETO$Gen_Hidraulica2
class(GENHIDRA2)
plot(hist(GENHIDRA2),col = "purple", main="Histograma GENHIDRAerador hora2 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 2",
     ylab="Frecuencia")
summary(GENHIDRA2)
MinGENHIDRA2<-min(GENHIDRA2)
MaxGENHIDRA2<-max(GENHIDRA2)
MediaGENHIDRA2<-mean(GENHIDRA2)
modeGENHIDRA2 <- function(GENHIDRA2){
  return(as.numeric(names(which.max(table(GENHIDRA2)))))}
modaGENHIDRA2<-modeGENHIDRA2(GENHIDRA2)
#Hora3
GENHIDRA3<- GENCOMPLETO$Gen_Hidraulica3
class(GENHIDRA3)
plot(hist(GENHIDRA3),col = "purple", main="Histograma GENHIDRAerador hora 3 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 3",
     ylab="Frecuencia")
summary(GENHIDRA3)
MinGENHIDRA3<-min(GENHIDRA3)
MaxGENHIDRA3<-max(GENHIDRA3)
MediaGENHIDRA3<-mean(GENHIDRA3)
modeGENHIDRA3 <- function(GENHIDRA3){
  return(as.numeric(names(which.max(table(GENHIDRA3)))))}
modaGENHIDRA3<-modeGENHIDRA3(GENHIDRA3)
#Hora4
GENHIDRA4<- GENCOMPLETO$Gen_Hidraulica4
class(GENHIDRA4)
plot(hist(GENHIDRA4),col = "purple", main="Histograma GENHIDRAerador hora 4 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 4",
     ylab="Frecuencia")
summary(GENHIDRA4)
MinGENHIDRA4<-min(GENHIDRA4)
MaxGENHIDRA4<-max(GENHIDRA4)
MediaGENHIDRA4<-mean(GENHIDRA4)
modeGENHIDRA4 <- function(GENHIDRA4){
  return(as.numeric(names(which.max(table(GENHIDRA4)))))}
modaGENHIDRA4<-modeGENHIDRA4(GENHIDRA4)

#Hora5
GENHIDRA5<- GENCOMPLETO$Gen_Hidraulica5
class(GENHIDRA5)
plot(hist(GENHIDRA5),col = "purple", main="Histograma GENHIDRAerador hora 5 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 5",
     ylab="Frecuencia")
summary(GENHIDRA5)
MinGENHIDRA5<-min(GENHIDRA5)
MaxGENHIDRA5<-max(GENHIDRA5)
MediaGENHIDRA5<-mean(GENHIDRA5)
modeGENHIDRA5 <- function(GENHIDRA5){
  return(as.numeric(names(which.max(table(GENHIDRA5)))))}
modaGENHIDRA5<-modeGENHIDRA4(GENHIDRA5)

#Hora6
GENHIDRA6<- GENCOMPLETO$Gen_Hidraulica6
class(GENHIDRA6)
plot(hist(GENHIDRA6),col = "purple", main="Histograma GENHIDRAerador hora 6 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 6",
     ylab="Frecuencia")
summary(GENHIDRA6)
MinGENHIDRA6<-min(GENHIDRA6)
MaxGENHIDRA6<-max(GENHIDRA6)
MediaGENHIDRA6<-mean(GENHIDRA6)
modeGENHIDRA6 <- function(GENHIDRA6){
  return(as.numeric(names(which.max(table(GENHIDRA6)))))}
modaGENHIDRA6<-modeGENHIDRA6(GENHIDRA6)

#Hora7
GENHIDRA7<- GENCOMPLETO$Gen_Hidraulica7
class(GENHIDRA7)
plot(hist(GENHIDRA7),col = "purple", main="Histograma GENHIDRAerador hora 7 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 7",
     ylab="Frecuencia")
summary(GENHIDRA7)
MinGENHIDRA7<-min(GENHIDRA7)
MaxGENHIDRA7<-max(GENHIDRA7)
MediaGENHIDRA7<-mean(GENHIDRA7)
modeGENHIDRA7 <- function(GENHIDRA7){
  return(as.numeric(names(which.max(table(GENHIDRA7)))))}
modaGENHIDRA7<-modeGENHIDRA7(GENHIDRA7)

#Hora8
GENHIDRA8<- GENCOMPLETO$Gen_Hidraulica8
class(GENHIDRA8)
plot(hist(GENHIDRA8),col = "purple", main="Histograma GENHIDRAerador hora 8 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 8",
     ylab="Frecuencia")
summary(GENHIDRA8)

MinGENHIDRA8<-min(GENHIDRA8)
MaxGENHIDRA8<-max(GENHIDRA8)
MediaGENHIDRA8<-mean(GENHIDRA8)
modeGENHIDRA8<- function(GENHIDRA8){
  return(as.numeric(names(which.max(table(GENHIDRA8)))))}
modaGENHIDRA8<-modeGENHIDRA8(GENHIDRA8)

#Hora9
GENHIDRA9<- GENCOMPLETO$Gen_Hidraulica9
class(GENHIDRA9)
plot(hist(GENHIDRA9),col = "purple", main="Histograma GENHIDRAerador hora 9 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 9",
     ylab="Frecuencia")
summary(GENHIDRA9)
MinGENHIDRA9<-min(GENHIDRA9)
MaxGENHIDRA9<-max(GENHIDRA9)
MediaGENHIDRA9<-mean(GENHIDRA9)
modeGENHIDRA9<- function(GENHIDRA9){
  return(as.numeric(names(which.max(table(GENHIDRA9)))))}
modaGENHIDRA9<-modeGENHIDRA9(GENHIDRA9)


#Hora10
GENHIDRA10<- GENCOMPLETO$Gen_Hidraulica10
class(GENHIDRA10)
plot(hist(GENHIDRA10),col = "purple", main="Histograma GENHIDRAerador hora 10 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 10",
     ylab="Frecuencia")
summary(GENHIDRA10)
MinGENHIDRA10<-min(GENHIDRA10)
MaxGENHIDRA10<-max(GENHIDRA10)
MediaGENHIDRA10<-mean(GENHIDRA10)
modeGENHIDRA10<- function(GENHIDRA10){
  return(as.numeric(names(which.max(table(GENHIDRA10)))))}
modaGENHIDRA10<-modeGENHIDRA10(GENHIDRA10)

#Hora11
GENHIDRA11<- GENCOMPLETO$Gen_Hidraulica11
class(GENHIDRA11)
plot(hist(GENHIDRA11),col = "purple", main="Histograma GENHIDRAerador hora 11 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 11",
     ylab="Frecuencia")
summary(GENHIDRA11)
MinGENHIDRA11<-min(GENHIDRA11)
MaxGENHIDRA11<-max(GENHIDRA11)
MediaGENHIDRA11<-mean(GENHIDRA11)
modeGENHIDRA11<- function(GENHIDRA11){
  return(as.numeric(names(which.max(table(GENHIDRA11)))))}
modaGENHIDRA11<-modeGENHIDRA11(GENHIDRA11)

#Hora12
GENHIDRA12<- GENCOMPLETO$Gen_Hidraulica12
class(GENHIDRA12)
plot(hist(GENHIDRA12),col = "purple", main="Histograma GENHIDRAerador hora 12 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 12",
     ylab="Frecuencia")
summary(GENHIDRA12)
MinGENHIDRA12<-min(GENHIDRA12)
MaxGENHIDRA12<-max(GENHIDRA12)
MediaGENHIDRA12<-mean(GENHIDRA12)
modeGENHIDRA12<- function(GENHIDRA12){
  return(as.numeric(names(which.max(table(GENHIDRA12)))))}
modaGENHIDRA12<-modeGENHIDRA12(GENHIDRA12)


#Hora13
GENHIDRA13<- GENCOMPLETO$Gen_Hidraulica13
class(GENHIDRA13)
plot(hist(GENHIDRA13),col = "purple", main="Histograma GENHIDRAerador hora 13 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 13",
     ylab="Frecuencia")
summary(GENHIDRA13)
MinGENHIDRA13<-min(GENHIDRA13)
MaxGENHIDRA13<-max(GENHIDRA13)
MediaGENHIDRA13<-mean(GENHIDRA13)
modeGENHIDRA13<- function(GENHIDRA13){
  return(as.numeric(names(which.max(table(GENHIDRA13)))))}
modaGENHIDRA13<-modeGENHIDRA13(GENHIDRA13)

#Hora14
GENHIDRA14<- GENCOMPLETO$Gen_Hidraulica14
class(GENHIDRA14)
plot(hist(GENHIDRA14),col = "purple", main="Histograma GENHIDRAerador hora 14 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 14",
     ylab="Frecuencia")
summary(GENHIDRA14)
MinGENHIDRA14<-min(GENHIDRA14)
MaxGENHIDRA14<-max(GENHIDRA14)
MediaGENHIDRA14<-mean(GENHIDRA14)
modeGENHIDRA14<- function(GENHIDRA14){
  return(as.numeric(names(which.max(table(GENHIDRA14)))))}
modaGENHIDRA14<-modeGENHIDRA14(GENHIDRA14)


#Hora15
GENHIDRA15<- GENCOMPLETO$Gen_Hidraulica15
class(GENHIDRA15)
plot(hist(GENHIDRA15),col = "purple", main="Histograma GENHIDRAerador hora 15 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 15",
     ylab="Frecuencia")
summary(GENHIDRA15)
MinGENHIDRA15<-min(GENHIDRA15)
MaxGENHIDRA15<-max(GENHIDRA15)
MediaGENHIDRA15<-mean(GENHIDRA15)
modeGENHIDRA15<- function(GENHIDRA15){
  return(as.numeric(names(which.max(table(GENHIDRA15)))))}
modaGENHIDRA15<-modeGENHIDRA15(GENHIDRA15)

#Hora16
GENHIDRA16<- GENCOMPLETO$Gen_Hidraulica16
class(GENHIDRA16)
plot(hist(GENHIDRA16),col = "purple", main="Histograma GENHIDRAerador hora 16 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 16",
     ylab="Frecuencia")
summary(GENHIDRA16)
MinGENHIDRA16<-min(GENHIDRA16)
MaxGENHIDRA16<-max(GENHIDRA16)
MediaGENHIDRA16<-mean(GENHIDRA16)
modeGENHIDRA16<- function(GENHIDRA16){
  return(as.numeric(names(which.max(table(GENHIDRA16)))))}
modaGENHIDRA16<-modeGENHIDRA16(GENHIDRA16)



#Hora17
GENHIDRA17<- GENCOMPLETO$Gen_Hidraulica17
class(GENHIDRA17)
plot(hist(GENHIDRA17),col = "purple", main="Histograma GENHIDRAerador hora 17 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 17",
     ylab="Frecuencia")
summary(GENHIDRA17)
MinGENHIDRA17<-min(GENHIDRA17)
MaxGENHIDRA17<-max(GENHIDRA17)
MediaGENHIDRA17<-mean(GENHIDRA17)
modeGENHIDRA17<- function(GENHIDRA17){
  return(as.numeric(names(which.max(table(GENHIDRA17)))))}
modaGENHIDRA17<-modeGENHIDRA17(GENHIDRA17)

#Hora18
GENHIDRA18<- GENCOMPLETO$Gen_Hidraulica18
class(GENHIDRA18)
plot(hist(GENHIDRA18),col = "purple", main="Histograma GENHIDRAerador hora 18 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 18",
     ylab="Frecuencia")
summary(GENHIDRA18)
MinGENHIDRA18<-min(GENHIDRA18)
MaxGENHIDRA18<-max(GENHIDRA18)
MediaGENHIDRA18<-mean(GENHIDRA18)
modeGENHIDRA18<- function(GENHIDRA18){
  return(as.numeric(names(which.max(table(GENHIDRA18)))))}
modaGENHIDRA18<-modeGENHIDRA18(GENHIDRA18)


#Hora19
GENHIDRA19<- GENCOMPLETO$Gen_Hidraulica19
class(GENHIDRA19)
plot(hist(GENHIDRA19),col = "purple", main="Histograma GENHIDRAerador hora 19 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 19",
     ylab="Frecuencia")
summary(GENHIDRA19)
MinGENHIDRA19<-min(GENHIDRA19)
MaxGENHIDRA19<-max(GENHIDRA19)
MediaGENHIDRA19<-mean(GENHIDRA19)
modeGENHIDRA19<- function(GENHIDRA19){
  return(as.numeric(names(which.max(table(GENHIDRA19)))))}
modaGENHIDRA19<-modeGENHIDRA19(GENHIDRA19)


#Hora20
GENHIDRA20<- GENCOMPLETO$Gen_Hidraulica20
class(GENHIDRA20)
plot(hist(GENHIDRA20),col = "purple", main="Histograma GENHIDRAerador hora 20 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 20",
     ylab="Frecuencia")
summary(GENHIDRA20)
MinGENHIDRA20<-min(GENHIDRA20)
MaxGENHIDRA20<-max(GENHIDRA20)
MediaGENHIDRA20<-mean(GENHIDRA20)
modeGENHIDRA20<- function(GENHIDRA20){
  return(as.numeric(names(which.max(table(GENHIDRA20)))))}
modaGENHIDRA20<-modeGENHIDRA20(GENHIDRA20)


#Hora21
GENHIDRA21<- GENCOMPLETO$Gen_Hidraulica21
class(GENHIDRA21)
plot(hist(GENHIDRA21),col = "purple", main="Histograma GENHIDRAerador hora 21 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 21",
     ylab="Frecuencia")
summary(GENHIDRA21)
MinGENHIDRA21<-min(GENHIDRA21)
MaxGENHIDRA21<-max(GENHIDRA21)
MediaGENHIDRA21<-mean(GENHIDRA21)
modeGENHIDRA21<- function(GENHIDRA21){
  return(as.numeric(names(which.max(table(GENHIDRA21)))))}
modaGENHIDRA21<-modeGENHIDRA21(GENHIDRA21)


#Hora22
GENHIDRA22<- GENCOMPLETO$Gen_Hidraulica22
class(GENHIDRA22)
plot(hist(GENHIDRA22),col = "purple", main="Histograma GENHIDRAerador hora 22 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 22",
     ylab="Frecuencia")
summary(GENHIDRA22)
MinGENHIDRA22<-min(GENHIDRA22)
MaxGENHIDRA22<-max(GENHIDRA22)
MediaGENHIDRA22<-mean(GENHIDRA22)
modeGENHIDRA22<- function(GENHIDRA22){
  return(as.numeric(names(which.max(table(GENHIDRA22)))))}
modaGENHIDRA22<-modeGENHIDRA22(GENHIDRA22)


#Hora23
GENHIDRA23<- GENCOMPLETO$Gen_Hidraulica23
class(GENHIDRA23)
plot(hist(GENHIDRA23),col = "purple", main="Histograma GENHIDRAerador hora 23 desde 01/01/2000- 30/06/2022",
     xlab="GENHIDRAerador 23",
     ylab="Frecuencia")
summary(GENHIDRA23)
MinGENHIDRA23<-min(GENHIDRA23)
MaxGENHIDRA23<-max(GENHIDRA23)
MediaGENHIDRA23<-mean(GENHIDRA23)
modeGENHIDRA23<- function(GENHIDRA23){
  return(as.numeric(names(which.max(table(GENHIDRA23)))))}
modaGENHIDRA23<-modeGENHIDRA23(GENHIDRA23)

##=========GEN TÉRMICA ======##
#Hora 0
GENTERM0<- GENCOMPLETO$Gen_Termica0
class(GENTERM0)
plot(hist(GENTERM0),col = "purple", main="Histograma GenTermica hora0 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica0",
     ylab="Frecuencia")
MinGENTERM0<-min(GENTERM0)
MaxGENTERM0<-max(GENTERM0)
MediaGENTERM0<-mean(GENTERM0)
modeGENTERM0 <- function(GENTERM0){
  return(as.numeric(names(which.max(table(GENTERM0)))))}
modaGENTERM0<-modeGENTERM0(GENTERM0)
ADGENTERM0<- cbind(summary(GENTERM0))
ADGENTERM0 <-data.frame(ADGENTERM0)
ADGENTERM0<- rbind(ADGENTERM0, modaGENTERM0)
rownames(ADGENTERM0)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM0) <- c("AD. Gen Térmica0")
View(ADGENTERM0)
#Hora 1
GENTERM1<- GENCOMPLETO$Gen_Termica1
class(GENTERM1)
plot(hist(GENTERM1),col = "purple", main="Histograma GenTermica hora1 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 1",
     ylab="Frecuencia")
MinGENTERM1<-min(GENTERM1)
MaxGENTERM1<-max(GENTERM1)
MediaGENTERM1<-mean(GENTERM1)
summary(GENTERM1)
modeGENTERM1 <- function(GENTERM1){
  return(as.numeric(names(which.max(table(GENTERM1)))))}
modaGENTERM1<-modeGENTERM1(GENTERM1)
ADGENTERM1<- cbind(summary(GENTERM1))
ADGENTERM1 <-data.frame(ADGENTERM1)
ADGENTERM1<- rbind(ADGENTERM1, modaGENTERM1)
rownames(ADGENTERM1)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM1) <- c("AD. Gen Térmica1")
View(ADGENTERM1)
#Hora2
GENTERM2<- GENCOMPLETO$Gen_Termica2
class(GENTERM2)
plot(hist(GENTERM2),col = "purple", main="Histograma GenTermica hora2 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 2",
     ylab="Frecuencia")
summary(GENTERM2)
MinGENTERM2<-min(GENTERM2)
MaxGENTERM2<-max(GENTERM2)
MediaGENTERM2<-mean(GENTERM2)
modeGENTERM2 <- function(GENTERM2){
  return(as.numeric(names(which.max(table(GENTERM2)))))}
modaGENTERM2<-modeGENTERM2(GENTERM2)
ADGENTERM2<- cbind(summary(GENTERM2))
ADGENTERM2 <-data.frame(ADGENTERM2)
ADGENTERM2<- rbind(ADGENTERM2, modaGENTERM2)
rownames(ADGENTERM2)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM2) <- c("AD. Gen Térmica2")
View(ADGENTERM2)

#Hora3
GENTERM3<- GENCOMPLETO$Gen_Termica3
class(GENTERM3)
plot(hist(GENTERM3),col = "purple", main="Histograma GenTermica hora 3 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 3",
     ylab="Frecuencia")
summary(GENTERM3)
MinGENTERM3<-min(GENTERM3)
MaxGENTERM3<-max(GENTERM3)
MediaGENTERM3<-mean(GENTERM3)
modeGENTERM3 <- function(GENTERM3){
  return(as.numeric(names(which.max(table(GENTERM3)))))}
modaGENTERM3<-modeGENTERM3(GENTERM3)
ADGENTERM3<- cbind(summary(GENTERM3))
ADGENTERM3 <-data.frame(ADGENTERM3)
ADGENTERM3<- rbind(ADGENTERM3, modaGENTERM3)
rownames(ADGENTERM3)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM3) <- c("AD. Gen Térmica3")
View(ADGENTERM3)
#Hora4
GENTERM4<- GENCOMPLETO$Gen_Termica4
class(GENTERM4)
plot(hist(GENTERM4),col = "purple", main="Histograma GenTermica hora 4 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 4",
     ylab="Frecuencia")
summary(GENTERM4)
MinGENTERM4<-min(GENTERM4)
MaxGENTERM4<-max(GENTERM4)
MediaGENTERM4<-mean(GENTERM4)
modeGENTERM4 <- function(GENTERM4){
  return(as.numeric(names(which.max(table(GENTERM4)))))}
modaGENTERM4<-modeGENTERM4(GENTERM4)
ADGENTERM4<- cbind(summary(GENTERM4))
ADGENTERM4 <-data.frame(ADGENTERM4)
ADGENTERM4<- rbind(ADGENTERM4, modaGENTERM4)
rownames(ADGENTERM4)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM4) <- c("AD. Gen Térmica4")
View(ADGENTERM4)
#Hora5
GENTERM5<- GENCOMPLETO$Gen_Termica5
class(GENTERM5)
plot(hist(GENTERM5),col = "purple", main="Histograma GenTermica hora 5 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 5",
     ylab="Frecuencia")
summary(GENTERM5)
MinGENTERM5<-min(GENTERM5)
MaxGENTERM5<-max(GENTERM5)
MediaGENTERM5<-mean(GENTERM5)
modeGENTERM5 <- function(GENTERM5){
  return(as.numeric(names(which.max(table(GENTERM5)))))}
modaGENTERM5<-modeGENTERM5(GENTERM5)
ADGENTERM5<- cbind(summary(GENTERM5))
ADGENTERM5 <-data.frame(ADGENTERM5)
ADGENTERM5<- rbind(ADGENTERM5, modaGENTERM5)
rownames(ADGENTERM5)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM5) <- c("AD. Gen Térmica5")
View(ADGENTERM5)
#Hora6
GENTERM6<- GENCOMPLETO$Gen_Termica6
class(GENTERM6)
plot(hist(GENTERM6),col = "purple", main="Histograma GenTermica hora 6 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 6",
     ylab="Frecuencia")
summary(GENTERM6)
MinGENTERM6<-min(GENTERM6)
MaxGENTERM6<-max(GENTERM6)
MediaGENTERM6<-mean(GENTERM6)
modeGENTERM6 <- function(GENTERM6){
  return(as.numeric(names(which.max(table(GENTERM6)))))}
modaGENTERM6<-modeGENTERM6(GENTERM6)
ADGENTERM6<- cbind(summary(GENTERM6))
ADGENTERM6 <-data.frame(ADGENTERM6)
ADGENTERM6<- rbind(ADGENTERM6, modaGENTERM6)
rownames(ADGENTERM6)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM6) <- c("AD. Gen Térmica6")
View(ADGENTERM6)
#Hora7
GENTERM7<- GENCOMPLETO$Gen_Termica7
class(GENTERM7)
plot(hist(GENTERM7),col = "purple", main="Histograma GenTermica hora 7 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 7",
     ylab="Frecuencia")
summary(GENTERM7)
MinGENTERM7<-min(GENTERM7)
MaxGENTERM7<-max(GENTERM7)
MediaGENTERM7<-mean(GENTERM7)
modeGENTERM7 <- function(GENTERM7){
  return(as.numeric(names(which.max(table(GENTERM7)))))}
modaGENTERM7<-modeGENTERM7(GENTERM7)
ADGENTERM6<- cbind(summary(GENTERM6))
ADGENTERM6 <-data.frame(ADGENTERM6)
ADGENTERM6<- rbind(ADGENTERM6, modaGENTERM6)
rownames(ADGENTERM6)<- c("Mínimo.", "1er.Cu.", "Mediana.", "Media.", "3er. Cu.", "Maximo.", "Moda.")
colnames(ADGENTERM6) <- c("AD. Gen Térmica6")
View(ADGENTERM6)


#Hora8
GENTERM8<- GENCOMPLETO$Gen_Termica8
class(GENTERM8)
plot(hist(GENTERM8),col = "purple", main="Histograma GenTermica hora 8 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 8",
     ylab="Frecuencia")
summary(GENTERM8)

MinGENTERM8<-min(GENTERM8)
MaxGENTERM8<-max(GENTERM8)
MediaGENTERM8<-mean(GENTERM8)
modeGENTERM8<- function(GENTERM8){
  return(as.numeric(names(which.max(table(GENTERM8)))))}
modaGENTERM8<-modeGENTERM8(GENTERM8)

#Hora9
GENTERM9<- GENCOMPLETO$Gen_Termica9
class(GENTERM9)
plot(hist(GENTERM9),col = "purple", main="Histograma GenTermica hora 9 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 9",
     ylab="Frecuencia")
summary(GENTERM9)
MinGENTERM9<-min(GENTERM9)
MaxGENTERM9<-max(GENTERM9)
MediaGENTERM9<-mean(GENTERM9)
modeGENTERM9<- function(GENTERM9){
  return(as.numeric(names(which.max(table(GENTERM9)))))}
modaGENTERM9<-modeGENTERM9(GENTERM9)


#Hora10
GENTERM10<- GENCOMPLETO$Gen_Termica10
class(GENTERM10)
plot(hist(GENTERM10),col = "purple", main="Histograma GenTermica hora 10 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 10",
     ylab="Frecuencia")
summary(GENTERM10)
MinGENTERM10<-min(GENTERM10)
MaxGENTERM10<-max(GENTERM10)
MediaGENTERM10<-mean(GENTERM10)
modeGENTERM10<- function(GENTERM10){
  return(as.numeric(names(which.max(table(GENTERM10)))))}
modaGENTERM10<-modeGENTERM10(GENTERM10)

#Hora11
GENTERM11<- GENCOMPLETO$Gen_Termica11
class(GENTERM11)
plot(hist(GENTERM11),col = "purple", main="Histograma GenTermica hora 11 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 11",
     ylab="Frecuencia")
summary(GENTERM11)
MinGENTERM11<-min(GENTERM11)
MaxGENTERM11<-max(GENTERM11)
MediaGENTERM11<-mean(GENTERM11)
modeGENTERM11<- function(GENTERM11){
  return(as.numeric(names(which.max(table(GENTERM11)))))}
modaGENTERM11<-modeGENTERM11(GENTERM11)

#Hora12
GENTERM12<- GENCOMPLETO$Gen_Termica12
class(GENTERM12)
plot(hist(GENTERM12),col = "purple", main="Histograma GenTermica hora 12 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 12",
     ylab="Frecuencia")
summary(GENTERM12)
MinGENTERM12<-min(GENTERM12)
MaxGENTERM12<-max(GENTERM12)
MediaGENTERM12<-mean(GENTERM12)
modeGENTERM12<- function(GENTERM12){
  return(as.numeric(names(which.max(table(GENTERM12)))))}
modaGENTERM12<-modeGENTERM12(GENTERM12)


#Hora13
GENTERM13<- GENCOMPLETO$Gen_Termica13
class(GENTERM13)
plot(hist(GENTERM13),col = "purple", main="Histograma GenTermica hora 13 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 13",
     ylab="Frecuencia")
summary(GENTERM13)
MinGENTERM13<-min(GENTERM13)
MaxGENTERM13<-max(GENTERM13)
MediaGENTERM13<-mean(GENTERM13)
modeGENTERM13<- function(GENTERM13){
  return(as.numeric(names(which.max(table(GENTERM13)))))}
modaGENTERM13<-modeGENTERM13(GENTERM13)

#Hora14
GENTERM14<- GENCOMPLETO$Gen_Termica14
class(GENTERM14)
plot(hist(GENTERM14),col = "purple", main="Histograma GenTermica hora 14 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 14",
     ylab="Frecuencia")
summary(GENTERM14)
MinGENTERM14<-min(GENTERM14)
MaxGENTERM14<-max(GENTERM14)
MediaGENTERM14<-mean(GENTERM14)
modeGENTERM14<- function(GENTERM14){
  return(as.numeric(names(which.max(table(GENTERM14)))))}
modaGENTERM14<-modeGENTERM14(GENTERM14)


#Hora15
GENTERM15<- GENCOMPLETO$Gen_Termica15
class(GENTERM15)
plot(hist(GENTERM15),col = "purple", main="Histograma GenTermica hora 15 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 15",
     ylab="Frecuencia")
summary(GENTERM15)
MinGENTERM15<-min(GENTERM15)
MaxGENTERM15<-max(GENTERM15)
MediaGENTERM15<-mean(GENTERM15)
modeGENTERM15<- function(GENTERM15){
  return(as.numeric(names(which.max(table(GENTERM15)))))}
modaGENTERM15<-modeGENTERM15(GENTERM15)

#Hora16
GENTERM16<- GENCOMPLETO$Gen_Termica16
class(GENTERM16)
plot(hist(GENTERM16),col = "purple", main="Histograma GenTermica hora 16 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 16",
     ylab="Frecuencia")
summary(GENTERM16)
MinGENTERM16<-min(GENTERM16)
MaxGENTERM16<-max(GENTERM16)
MediaGENTERM16<-mean(GENTERM16)
modeGENTERM16<- function(GENTERM16){
  return(as.numeric(names(which.max(table(GENTERM16)))))}
modaGENTERM16<-modeGENTERM16(GENTERM16)



#Hora17
GENTERM17<- GENCOMPLETO$Gen_Termica17
class(GENTERM17)
plot(hist(GENTERM17),col = "purple", main="Histograma GenTermica hora 17 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 17",
     ylab="Frecuencia")
summary(GENTERM17)
MinGENTERM17<-min(GENTERM17)
MaxGENTERM17<-max(GENTERM17)
MediaGENTERM17<-mean(GENTERM17)
modeGENTERM17<- function(GENTERM17){
  return(as.numeric(names(which.max(table(GENTERM17)))))}
modaGENTERM17<-modeGENTERM17(GENTERM17)

#Hora18
GENTERM18<- GENCOMPLETO$Gen_Termica18
class(GENTERM18)
plot(hist(GENTERM18),col = "purple", main="Histograma GenTermica hora 18 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 18",
     ylab="Frecuencia")
summary(GENTERM18)
MinGENTERM18<-min(GENTERM18)
MaxGENTERM18<-max(GENTERM18)
MediaGENTERM18<-mean(GENTERM18)
modeGENTERM18<- function(GENTERM18){
  return(as.numeric(names(which.max(table(GENTERM18)))))}
modaGENTERM18<-modeGENTERM18(GENTERM18)


#Hora19
GENTERM19<- GENCOMPLETO$Gen_Termica19
class(GENTERM19)
plot(hist(GENTERM19),col = "purple", main="Histograma GenTermica hora 19 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 19",
     ylab="Frecuencia")
summary(GENTERM19)
MinGENTERM19<-min(GENTERM19)
MaxGENTERM19<-max(GENTERM19)
MediaGENTERM19<-mean(GENTERM19)
modeGENTERM19<- function(GENTERM19){
  return(as.numeric(names(which.max(table(GENTERM19)))))}
modaGENTERM19<-modeGENTERM19(GENTERM19)


#Hora20
GENTERM20<- GENCOMPLETO$Gen_Termica20
class(GENTERM20)
plot(hist(GENTERM20),col = "purple", main="Histograma GenTermica hora 20 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 20",
     ylab="Frecuencia")
summary(GENTERM20)
MinGENTERM20<-min(GENTERM20)
MaxGENTERM20<-max(GENTERM20)
MediaGENTERM20<-mean(GENTERM20)
modeGENTERM20<- function(GENTERM20){
  return(as.numeric(names(which.max(table(GENTERM20)))))}
modaGENTERM20<-modeGENTERM20(GENTERM20)


#Hora21
GENTERM21<- GENCOMPLETO$Gen_Termica21
class(GENTERM21)
plot(hist(GENTERM21),col = "purple", main="Histograma GenTermica hora 21 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 21",
     ylab="Frecuencia")
summary(GENTERM21)
MinGENTERM21<-min(GENTERM21)
MaxGENTERM21<-max(GENTERM21)
MediaGENTERM21<-mean(GENTERM21)
modeGENTERM21<- function(GENTERM21){
  return(as.numeric(names(which.max(table(GENTERM21)))))}
modaGENTERM21<-modeGENTERM21(GENTERM21)


#Hora22
GENTERM22<- GENCOMPLETO$Gen_Termica22
class(GENTERM22)
plot(hist(GENTERM22),col = "purple", main="Histograma GenTermica hora 22 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 22",
     ylab="Frecuencia")
summary(GENTERM22)
MinGENTERM22<-min(GENTERM22)
MaxGENTERM22<-max(GENTERM22)
MediaGENTERM22<-mean(GENTERM22)
modeGENTERM22<- function(GENTERM22){
  return(as.numeric(names(which.max(table(GENTERM22)))))}
modaGENTERM22<-modeGENTERM22(GENTERM22)


#Hora23
GENTERM23<- GENCOMPLETO$Gen_Termica23
class(GENTERM23)
plot(hist(GENTERM23),col = "purple", main="Histograma GenTermica hora 23 desde 01/01/2000- 30/06/2022",
     xlab="GenTermica 23",
     ylab="Frecuencia")
summary(GENTERM23)
MinGENTERM23<-min(GENTERM23)
MaxGENTERM23<-max(GENTERM23)
MediaGENTERM23<-mean(GENTERM23)
modeGENTERM23<- function(GENTERM23){
  return(as.numeric(names(which.max(table(GENTERM23)))))}
modaGENTERM23<-modeGENTERM23(GENTERM23)

