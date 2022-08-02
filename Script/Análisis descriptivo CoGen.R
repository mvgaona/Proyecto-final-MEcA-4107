#-CoGenerador
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
