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
library(readxl)
file.choose()
####Aportes Diarios
Aportes_Diario_2000 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2000.xlsx")
summary(Aportes_Diario_2000)
Aportes_Diario_2000<- Aportes_Diario_2000[c(-1),]
View(Aportes_Diario_2000)
colnames(Aportes_Diario_2000) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2001 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2001.xlsx")
View(Aportes_Diario_2001)
colnames(Aportes_Diario_2001) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2001<- Aportes_Diario_2001[c(-1,-2),]
Aportes_Diario_2002 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2002.xlsx")
View(Aportes_Diario_2002)
colnames(Aportes_Diario_2002) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2002<- Aportes_Diario_2002[c(-1,-2),]
Aportes_Diario_2003 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2003.xlsx")
View(Aportes_Diario_2003)
Aportes_Diario_2003<- Aportes_Diario_2003[c(-1,-2),]
colnames(Aportes_Diario_2003) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2004 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2004.xlsx")
View(Aportes_Diario_2004)
Aportes_Diario_2004<- Aportes_Diario_2004[c(-1,-2),]
colnames(Aportes_Diario_2004) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2005 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2005.xlsx")
View(Aportes_Diario_2005)
Aportes_Diario_2005<- Aportes_Diario_2005[c(-1,-2),]
colnames(Aportes_Diario_2005) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2006 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2006.xlsx")
View(Aportes_Diario_2006)
Aportes_Diario_2006<- Aportes_Diario_2006[c(-1,-2),]
colnames(Aportes_Diario_2006) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2007 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2007.xlsx")
Aportes_Diario_2007<- Aportes_Diario_2007[c(-1,-2),]
View(Aportes_Diario_2007)
colnames(Aportes_Diario_2007) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2008 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2008.xlsx")
Aportes_Diario_2008<- Aportes_Diario_2008[c(-1,-2),]
View(Aportes_Diario_2008)
colnames(Aportes_Diario_2008) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2009 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2009.xlsx")
Aportes_Diario_2009<- Aportes_Diario_2009[c(-1,-2),]
View(Aportes_Diario_2009)
colnames(Aportes_Diario_2009) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2010 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2010.xlsx")
Aportes_Diario_2010<- Aportes_Diario_2010[c(-1,-2),]
View(Aportes_Diario_2010)
colnames(Aportes_Diario_2010) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2011 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2011.xlsx")
Aportes_Diario_2011<- Aportes_Diario_2011[c(-1,-2),]
View(Aportes_Diario_2011)
colnames(Aportes_Diario_2011) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2012 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2012.xlsx")
Aportes_Diario_2012<- Aportes_Diario_2012[c(-1,-2),]
View(Aportes_Diario_2012)
colnames(Aportes_Diario_2012) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2013 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2013.xlsx")
Aportes_Diario_2013<- Aportes_Diario_2013[c(-1,-2),]
View(Aportes_Diario_2013)
colnames(Aportes_Diario_2013) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2014 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2014.xlsx")
Aportes_Diario_2014<- Aportes_Diario_2014[c(-1,-2),]
View(Aportes_Diario_2014)
colnames(Aportes_Diario_2014) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2015 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2015.xlsx")
Aportes_Diario_2015<- Aportes_Diario_2015[c(-1,-2),]
View(Aportes_Diario_2015)
colnames(Aportes_Diario_2015) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2016 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2016.xlsx")
Aportes_Diario_2016<- Aportes_Diario_2016[c(-1,-2),]
View(Aportes_Diario_2016)
colnames(Aportes_Diario_2016) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2017 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2017.xlsx")
Aportes_Diario_2017<- Aportes_Diario_2017[c(-1,-2),]
View(Aportes_Diario_2017)
colnames(Aportes_Diario_2017) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2018 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2018.xlsx")
Aportes_Diario_2018<- Aportes_Diario_2018[c(-1,-2),]
View(Aportes_Diario_2018)
colnames(Aportes_Diario_2018) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2019 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2019.xlsx")
Aportes_Diario_2019<- Aportes_Diario_2019[c(-1,-2),]
View(Aportes_Diario_2019)
colnames(Aportes_Diario_2019) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2020 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2020.xlsx")
Aportes_Diario_2020<- Aportes_Diario_2020[c(-1,-2),]
View(Aportes_Diario_2020)
colnames(Aportes_Diario_2020) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2021 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2021.xlsx")
Aportes_Diario_2021<- Aportes_Diario_2021[c(-1,-2),]
View(Aportes_Diario_2021)
colnames(Aportes_Diario_2021) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diario_2022<- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Aportes_Diario_2022.xlsx")
Aportes_Diario_2022<- Aportes_Diario_2022[c(-1,-2),]
View(Aportes_Diario_2022)
colnames(Aportes_Diario_2022) <- c('Histórico Aportes','...2', '...3', '...4', '...5', '...6')
Aportes_Diarios<- rbind(Aportes_Diario_2000, Aportes_Diario_2001, Aportes_Diario_2002, Aportes_Diario_2003, Aportes_Diario_2004,Aportes_Diario_2005, Aportes_Diario_2006, Aportes_Diario_2007, Aportes_Diario_2008, Aportes_Diario_2009, Aportes_Diario_2010, Aportes_Diario_2011, Aportes_Diario_2012, Aportes_Diario_2013, Aportes_Diario_2014, Aportes_Diario_2015, Aportes_Diario_2016, Aportes_Diario_2017, Aportes_Diario_2018, Aportes_Diario_2019, Aportes_Diario_2020, Aportes_Diario_2021, Aportes_Diario_2022)
saveRDS(Aportes_Diarios, "C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Aportes_Diarios.rds" )

############################################################################################
### Aporte Diario_dia

Aportes_energia<-data.frame(readRDS("../Datos/Bases oficiales/Aportes_Diarios.rds")) 
Aportes_energia$...5[is.na(Aportes_energia$...5)] = 0 #Se imputa cero a los aportes de energía con NA
Aportes_energia<- Aportes_energia[c(-1),] #Se eilimina primera fila con referencia a los nombres de los archivos excel
Aportes_energia$...5<-as.numeric(Aportes_energia$...5) #Se vuelven números los valores del excel

Aportes_energia$Day<-as.Date(Aportes_energia$Histórico.Aportes) #Se convierte en formato de fecha la columna
Aporte_dia<-aggregate(Aportes_energia$...5, by=list(Aportes_energia$Day), sum) #Se suman los aportes de energía de cada río


colnames(Aporte_dia) <- c('Fecha','Aportes_total')


saveRDS(Aporte_dia, "../Datos/Bases oficiales/Aportes_energia_dia.rds")

Aporte_dia_30_06_2022<-Aporte_dia[1:8217,]
saveRDS(Aporte_dia_30_06_2022, "../Datos/Bases oficiales/Aportes_energia_dia_30_06_2022.rds")



###Precio de bolsa

Precio_Bolsa_2000 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2000.xlsx")
colnames(Precio_Bolsa_2000) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
View(Precio_Bolsa_2000)
Precio_Bolsa_2000<-Precio_Bolsa_2000%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2001 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2001.xlsx")
colnames(Precio_Bolsa_2001) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24','...25' )
Precio_Bolsa_2001<- Precio_Bolsa_2001[c(-1,-2),]
View(Precio_Bolsa_2001)
Precio_Bolsa_2002 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2002.xlsx")
colnames(Precio_Bolsa_2002) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2002<- Precio_Bolsa_2002[c(-1,-2),]
View(Precio_Bolsa_2002)
Precio_Bolsa_2003 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2003.xlsx")
colnames(Precio_Bolsa_2003) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
View(Precio_Bolsa_2003)
Precio_Bolsa_2003<- Precio_Bolsa_2003[c(-1,-2),]
View(Precio_Bolsa_2003)
Precio_Bolsa_2004 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2004.xlsx")
colnames(Precio_Bolsa_2004) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2004<- Precio_Bolsa_2004[c(-1,-2),]
View(Precio_Bolsa_2004)
Precio_Bolsa_2005 <- read_excel("../Datos/Precio_Bolsa_2005_.xlsx")
colnames(Precio_Bolsa_2005) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
#Precio_Bolsa_2005<- Precio_Bolsa_2005[c(-1,-2),]
Precio_Bolsa_2005<-Precio_Bolsa_2005%>% mutate(...26 = NULL) #Eliminación de primera columna
View(Precio_Bolsa_2005)
Precio_Bolsa_2006 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2006.xlsx")
Precio_Bolsa_2006<-Precio_Bolsa_2006%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2006<-Precio_Bolsa_2006%>% mutate(...27 = NULL)
colnames(Precio_Bolsa_2006) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2006<- Precio_Bolsa_2006[c(-1,-2),]
View(Precio_Bolsa_2006)
Precio_Bolsa_2007 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2007.xlsx")
Precio_Bolsa_2007<-Precio_Bolsa_2007%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2007<-Precio_Bolsa_2007%>% mutate(...27 = NULL)
colnames(Precio_Bolsa_2007) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2007<- Precio_Bolsa_2007[c(-1,-2),]
View(Precio_Bolsa_2007)
Precio_Bolsa_2008 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2008.xlsx")
Precio_Bolsa_2008<-Precio_Bolsa_2008%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2008<-Precio_Bolsa_2008%>% mutate(...27 = NULL)
colnames(Precio_Bolsa_2008) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2008<- Precio_Bolsa_2008[c(-1,-2),]
View(Precio_Bolsa_2008)
Precio_Bolsa_2009 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2009.xlsx")
Precio_Bolsa_2009<-Precio_Bolsa_2009%>% mutate(...26 = NULL) #Eliminación de primera columna
colnames(Precio_Bolsa_2009) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2009<- Precio_Bolsa_2009[c(-1,-2),]
View(Precio_Bolsa_2009)
Precio_Bolsa_2010 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2010.xlsx")
Precio_Bolsa_2010<-Precio_Bolsa_2010%>% mutate(...26 = NULL) #Eliminación de primera columna
colnames(Precio_Bolsa_2010) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2010<- Precio_Bolsa_2010[c(-1),]
View(Precio_Bolsa_2010)

Precio_Bolsa_2011 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2011.xlsx")
Precio_Bolsa_2011<-Precio_Bolsa_2011%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2011<-Precio_Bolsa_2011%>% mutate(...27 = NULL)
colnames(Precio_Bolsa_2011) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2011<- Precio_Bolsa_2011[c(-1),]
View(Precio_Bolsa_2011)

Precio_Bolsa_2012 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2012.xlsx")
Precio_Bolsa_2012<-Precio_Bolsa_2012%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2012<-Precio_Bolsa_2012%>% mutate(...27 = NULL)
colnames(Precio_Bolsa_2012) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2012<- Precio_Bolsa_2012[c(-1),]
View(Precio_Bolsa_2012)

Precio_Bolsa_2013 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2013.xlsx")
Precio_Bolsa_2013<-Precio_Bolsa_2013%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2013<-Precio_Bolsa_2013%>% mutate(...27 = NULL)
colnames(Precio_Bolsa_2013) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2013<- Precio_Bolsa_2013[c(-1),]
View(Precio_Bolsa_2013)
Precio_Bolsa_2014 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2014.xlsx")
Precio_Bolsa_2014<-Precio_Bolsa_2014%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2014<-Precio_Bolsa_2014%>% mutate(...27 = NULL)
colnames(Precio_Bolsa_2014) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2014<- Precio_Bolsa_2014[c(-1),]
View(Precio_Bolsa_2014)
Precio_Bolsa_2015 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2015.xlsx")
colnames(Precio_Bolsa_2015) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26', "...27")
Precio_Bolsa_2015<-Precio_Bolsa_2015%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2015<-Precio_Bolsa_2015%>% mutate(...27 = NULL)
Precio_Bolsa_2015<- Precio_Bolsa_2015[c(-1),]
View(Precio_Bolsa_2015)
Precio_Bolsa_2016 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2016.xlsx")
Precio_Bolsa_2016<-Precio_Bolsa_2016%>% mutate(...26 = NULL) #Eliminación de primera columna
colnames(Precio_Bolsa_2016) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25')
Precio_Bolsa_2016<- Precio_Bolsa_2016[c(-1),]
View(Precio_Bolsa_2016)
Precio_Bolsa_2017 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2017.xlsx")
colnames(Precio_Bolsa_2017) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2017<-Precio_Bolsa_2017%>% mutate(...26 = NULL) 
Precio_Bolsa_2017<- Precio_Bolsa_2017[c(-1),]
View(Precio_Bolsa_2017)
Precio_Bolsa_2018 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2018.xlsx")
colnames(Precio_Bolsa_2018) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2018<-Precio_Bolsa_2018%>% mutate(...26 = NULL) #Eliminación de primera columna
Precio_Bolsa_2018<- Precio_Bolsa_2018[c(-1),]
View(Precio_Bolsa_2018)
Precio_Bolsa_2019 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2019.xlsx")
colnames(Precio_Bolsa_2019) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2019<-Precio_Bolsa_2019%>% mutate(...26 = NULL)
Precio_Bolsa_2019<- Precio_Bolsa_2019[c(-1),]
View(Precio_Bolsa_2019)
Precio_Bolsa_2020 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2020.xlsx")
Precio_Bolsa_2020<-Precio_Bolsa_2020%>% mutate(...26 = NULL) #Eliminación de primera columna
colnames(Precio_Bolsa_2020) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2020<- Precio_Bolsa_2020[c(-1),]
View(Precio_Bolsa_2020)

Precio_Bolsa_2021 <- read_excel("../Datos/Precio_Bolsa_2021.xlsx")
View(Precio_Bolsa_2021)

colnames(Precio_Bolsa_2021) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2021<-Precio_Bolsa_2021%>% mutate(...26 = NULL) 
View(Precio_Bolsa_2021)
Precio_Bolsa_2022 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2022.xlsx")
colnames(Precio_Bolsa_2022) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2022<-Precio_Bolsa_2022%>% mutate(...26 = NULL) 
Precio_Bolsa_2022<- Precio_Bolsa_2022[c(-1),]
View(Precio_Bolsa_2022)
#rm(Precios_Bolsa)
Precios_Bolsa<- rbind(Precio_Bolsa_2000, Precio_Bolsa_2001, Precio_Bolsa_2002, Precio_Bolsa_2003,Precio_Bolsa_2004, Precio_Bolsa_2005, Precio_Bolsa_2006, Precio_Bolsa_2007, Precio_Bolsa_2008, Precio_Bolsa_2009, Precio_Bolsa_2010, Precio_Bolsa_2011, Precio_Bolsa_2012, Precio_Bolsa_2013, Precio_Bolsa_2014, Precio_Bolsa_2015, Precio_Bolsa_2016, Precio_Bolsa_2017, Precio_Bolsa_2018, Precio_Bolsa_2019, Precio_Bolsa_2020, Precio_Bolsa_2021, Precio_Bolsa_2022)
rownames(Precios_Bolsa)=NULL #Para renumerar la base
saveRDS(Precios_Bolsa, "../Datos/Bases oficiales/Precios_Bolsa_2.rds" )
library(readxl)
#### TRM
TRM <- read_excel("../Datos/TRM.xlsx")
View(TRM)
TRM<- TRM[c(-8248),]
saveRDS(TRM, "../Datos/Bases oficiales/TRM.rds" )
###Generación
Generacion_2000 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2000.xlsx")
View(Generacion_2000)
Generacion_2000<-Generacion_2000%>% mutate(...28 = NULL) #Eliminación de primera columna
colnames(Generacion_2000) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2000)

Generacion_2001 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2001.xlsx")
View(Generacion_2001)
Generacion_2001<- Generacion_2001[c(-1,-2),]
View(Generacion_2001)
colnames(Generacion_2001) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2001)

Generacion_2002 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2002.xlsx")
View(Generacion_2002)
Generacion_2002<-Generacion_2002%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2002)
colnames(Generacion_2002) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2002)
Generacion_2002<- Generacion_2002[c(-1),]
View(Generacion_2002)

Generacion_2003 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2003.xlsx")
View(Generacion_2003)
colnames(Generacion_2003) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2003)
Generacion_2003<- Generacion_2003[c(-1,-2),]
View(Generacion_2003)

Generacion_2004 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2004.xlsx")
View(Generacion_2004)
colnames(Generacion_2004) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2004)
Generacion_2004<- Generacion_2004[c(-1,-2),]
View(Generacion_2004)

Generacion_2005 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2005.xlsx")
View(Generacion_2005)
colnames(Generacion_2005) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2005)
Generacion_2005<- Generacion_2005[c(-1,-2),]
View(Generacion_2005)

Generacion_2006 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2006.xlsx")
View(Generacion_2006)
colnames(Generacion_2006) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2006)
Generacion_2006<- Generacion_2006[c(-1,-2),]
View(Generacion_2006)

Generacion_2007 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2007.xlsx")
View(Generacion_2007)
colnames(Generacion_2007) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2007)
Generacion_2007<- Generacion_2007[c(-1,-2),]
View(Generacion_2007)

Generacion_2008 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2008.xlsx")
View(Generacion_2008)
colnames(Generacion_2008) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2008)
Generacion_2008<- Generacion_2008[c(-1,-2),]
View(Generacion_2008)

Generacion_2009 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2009.xlsx")
View(Generacion_2009)
colnames(Generacion_2009) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2009)
Generacion_2009<- Generacion_2009[c(-1,-2),]
View(Generacion_2009)

Generacion_2010 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2010.xlsx")
View(Generacion_2010)
colnames(Generacion_2010) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2010)
Generacion_2010<- Generacion_2010[c(-1,-2),]
View(Generacion_2010)

Generacion_2011 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2011.xlsx")
View(Generacion_2011)
colnames(Generacion_2011) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2011)
Generacion_2011<- Generacion_2011[c(-1,-2),]
View(Generacion_2011)

Generacion_2012 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2012.xlsx")
View(Generacion_2012)
Generacion_2012<-Generacion_2012%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2012)
colnames(Generacion_2012) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2012)
Generacion_2012<- Generacion_2012[c(-1),]
View(Generacion_2012)

Generacion_2013 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2013.xlsx")
View(Generacion_2013)
Generacion_2013<-Generacion_2013%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2013)
colnames(Generacion_2013) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2013)
Generacion_2013<- Generacion_2013[c(-1),]
View(Generacion_2013)

Generacion_2014 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2014.xlsx")
View(Generacion_2014)
Generacion_2014<-Generacion_2014%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2014)
colnames(Generacion_2014) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2014)
Generacion_2014<- Generacion_2014[c(-1),]
View(Generacion_2014)

Generacion_2015 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2015.xlsx")
View(Generacion_2015)
Generacion_2015<-Generacion_2015%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2015)
colnames(Generacion_2015) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2015)
Generacion_2015<- Generacion_2015[c(-1),]
View(Generacion_2015)

Generacion_2016 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2016.xlsx")
View(Generacion_2016)
Generacion_2016<-Generacion_2016%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2016)
colnames(Generacion_2016) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2016)
Generacion_2016<- Generacion_2016[c(-1),]
View(Generacion_2016)

Generacion_2017 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2017.xlsx")
View(Generacion_2017)
Generacion_2017<-Generacion_2017%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2017)
colnames(Generacion_2017) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2017)
Generacion_2017<- Generacion_2017[c(-1),]
View(Generacion_2017)

Generacion_2018 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2018.xlsx")
View(Generacion_2018)
Generacion_2018<-Generacion_2018%>% mutate(...28 = NULL) #Eliminación de primera columna
View(Generacion_2018)
colnames(Generacion_2018) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2018)
Generacion_2018<- Generacion_2018[c(-1),]
View(Generacion_2018)

# Generacion_2018 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2018.xlsx")
# View(Generacion_2018)
# Generacion_2018<-Generacion_2018%>% mutate(...28 = NULL) #Eliminación de primera columna
# View(Generacion_2018)
# colnames(Generacion_2018) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
# View(Generacion_2018)
# Generacion_2018<- Generacion_2018[c(-1),]
# View(Generacion_2018)

Generacion_2019 <- read_excel("../Datos/Generacion_2019.xlsx")
View(Generacion_2019)
Generacion_2019<-Generacion_2019%>% mutate(Version = NULL) #Eliminación de primera columna
View(Generacion_2019)
colnames(Generacion_2019) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2019)

Generacion_2020 <- read_excel("../Datos/Generacion_Ideal_(kWh)_2020.xlsx")
View(Generacion_2020)
Generacion_2020<-Generacion_2020%>% mutate(`Generacion Ideal (kWh) 2020` = NULL) #Eliminación de primera columna
View(Generacion_2020)
Generacion_2020<-Generacion_2020%>% mutate(...29 = NULL) #Eliminación de primera columna
colnames(Generacion_2020) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2020)
Generacion_2020<- Generacion_2020[c(-1),]
View(Generacion_2020)

Generacion_2021 <- read_excel("../Datos/Generacion_2021.xlsx")
View(Generacion_2021)
Generacion_2021<-Generacion_2021%>% mutate(`Codigo Recurso` = NULL) #Eliminación de primera columna
View(Generacion_2021)
Generacion_2021<-Generacion_2021%>% mutate(Version = NULL) #Eliminación de primera columna
colnames(Generacion_2021) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2021)

Generacion_2022 <- read_excel("../Datos/Generacion_2022.xlsx")
View(Generacion_2022)
Generacion_2022<-Generacion_2022%>% mutate(`Codigo Recurso` = NULL) #Eliminación de primera columna
View(Generacion_2022)
Generacion_2022<-Generacion_2022%>% mutate(Version = NULL) #Eliminación de primera columna
colnames(Generacion_2022) <- c('...1', '...2', '...3','Generacion Ideal', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26','...27', '...26', '...27')
View(Generacion_2022)

Generacion <- rbind(Generacion_2000, Generacion_2001, Generacion_2002, Generacion_2003, Generacion_2004, Generacion_2005, Generacion_2006, Generacion_2007, Generacion_2008, Generacion_2009, Generacion_2010,Generacion_2011, Generacion_2012, Generacion_2013, Generacion_2014, Generacion_2015, Generacion_2016, Generacion_2017, Generacion_2018, Generacion_2019, Generacion_2020, Generacion_2021, Generacion_2022)
View(Generacion)
rownames(Generacion)=NULL #Para renumerar la base
saveRDS(Generacion, "../Datos/Bases oficiales/Generacion_2.rds" )

#####################################################################################################
#### Tipo de generación


Generacion <- data.frame(readRDS("../Datos/Bases oficiales/Generacion_2.rds")) 
lista_agentes <- read.csv("../Datos/Listado_agentes.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(lista_agentes) <- c('Id','Values_Code', '...2', 'Values_Type', 'Values_Disp', 'Values_Rectype', 'Values_Companycode', 'Values_enersource', 'Values_Operacionstartdate', 'Values_state', 'Data')
lista_agentes<- subset(lista_agentes, select = c("Id", "...2", "Values_Type", "Values_Rectype", "Values_enersource"))

Generacion["...2"][Generacion["...2"] == "PCH DE LA LIBERTAD"] <- "PCH LA LIBERTAD" #Cambio punto a punto de información para que coincidan los nombres
Generacion["...2"][Generacion["...2"] == "PROENCA 2"] <- "PROENCA II"
Generacion["...2"][Generacion["...2"] == "SOGAMOSO_P"] <- "SOGAMOSO"
Generacion["...2"][Generacion["...2"] == "COGENERADOR PROENCA 1"] <- "COGENERADOR PROENCA"


#Se importan los datos de los generadores año a año
Capacidad_neta_1<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2005.xlsx")
colnames(Capacidad_neta_1)[1] <- "Capacidad"
Capacidad_neta_1<- Capacidad_neta_1[c(-1,-2),]
Capacidad_neta_2<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2000.xlsx")
colnames(Capacidad_neta_2)[1] <- "Capacidad"
Capacidad_neta_2<- Capacidad_neta_2[c(-1),]
Capacidad_neta_3<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2013.xlsx")
colnames(Capacidad_neta_3)[1] <- "Capacidad"
Capacidad_neta_3<- Capacidad_neta_3[c(-1,-2),]
Capacidad_neta_6<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2001.xlsx")
colnames(Capacidad_neta_6)[1] <- "Capacidad"
Capacidad_neta_6<- Capacidad_neta_6[c(-1,-2),]
Capacidad_neta_7<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2002.xlsx")
colnames(Capacidad_neta_7)[1] <- "Capacidad"
Capacidad_neta_7<- Capacidad_neta_7[c(-1,-2),]
Capacidad_neta_8<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2003.xlsx")
colnames(Capacidad_neta_8)[1] <- "Capacidad"
Capacidad_neta_8<- Capacidad_neta_8[c(-1,-2),]
Capacidad_neta_9<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2004.xlsx")
colnames(Capacidad_neta_9)[1] <- "Capacidad"
Capacidad_neta_9<- Capacidad_neta_9[c(-1,-2),]
Capacidad_neta_10<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2006.xlsx")
colnames(Capacidad_neta_10)[1] <- "Capacidad"
Capacidad_neta_10<- Capacidad_neta_10[c(-1,-2),]
Capacidad_neta_11<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2007.xlsx")
colnames(Capacidad_neta_11)[1] <- "Capacidad"
Capacidad_neta_11<- Capacidad_neta_11[c(-1,-2),]
Capacidad_neta_12<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2008.xlsx")
colnames(Capacidad_neta_12)[1] <- "Capacidad"
Capacidad_neta_12<- Capacidad_neta_12[c(-1,-2),]
Capacidad_neta_13<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2009.xlsx")
colnames(Capacidad_neta_13)[1] <- "Capacidad"
Capacidad_neta_13<- Capacidad_neta_13[c(-1,-2),]
Capacidad_neta_14<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2010.xlsx")
colnames(Capacidad_neta_14)[1] <- "Capacidad"
Capacidad_neta_14<- Capacidad_neta_14[c(-1,-2),]
Capacidad_neta_15<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2011.xlsx")
colnames(Capacidad_neta_15)[1] <- "Capacidad"
Capacidad_neta_15<- Capacidad_neta_15[c(-1,-2),]
Capacidad_neta_16<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2012.xlsx")
colnames(Capacidad_neta_16)[1] <- "Capacidad"
Capacidad_neta_16<- Capacidad_neta_16[c(-1,-2),]
Capacidad_neta_17<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2014.xlsx")
colnames(Capacidad_neta_17)[1] <- "Capacidad"
Capacidad_neta_17<- Capacidad_neta_17[c(-1,-2),]
Capacidad_neta_18<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2015.xlsx")
colnames(Capacidad_neta_18)[1] <- "Capacidad"
Capacidad_neta_18<- Capacidad_neta_18[c(-1,-2),]
Capacidad_neta_19<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2016SEM1.xlsx")
colnames(Capacidad_neta_19)[1] <- "Capacidad"
Capacidad_neta_19<- Capacidad_neta_19[c(-1,-2),]
Capacidad_neta_20<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2016SEM2.xlsx")
colnames(Capacidad_neta_20)[1] <- "Capacidad"
Capacidad_neta_20<- Capacidad_neta_20[c(-1,-2),]
Capacidad_neta_21<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2017SEM1.xlsx")
colnames(Capacidad_neta_21)[1] <- "Capacidad"
Capacidad_neta_21<- Capacidad_neta_21[c(-1,-2),]
Capacidad_neta_22<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2017SEM2.xlsx")
colnames(Capacidad_neta_22)[1] <- "Capacidad"
Capacidad_neta_22<- Capacidad_neta_22[c(-1,-2),]
Capacidad_neta_23<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2018.xlsx")
colnames(Capacidad_neta_23)[1] <- "Capacidad"
Capacidad_neta_23<- Capacidad_neta_23[c(-1,-2),]
Capacidad_neta_24<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2019.xlsx")
colnames(Capacidad_neta_24)[1] <- "Capacidad"
Capacidad_neta_24<- Capacidad_neta_24[c(-1,-2),]
Capacidad_neta_25<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2021.xlsx")
colnames(Capacidad_neta_25)[1] <- "Capacidad"
Capacidad_neta_25<- Capacidad_neta_25[c(-1,-2),]
Capacidad_neta_26<- read_excel("../Datos/Capacidad_Efectiva_Neta_(kW)_2022.xlsx")
colnames(Capacidad_neta_26)[1] <- "Capacidad"
Capacidad_neta_26<- Capacidad_neta_26[c(-1,-2),]

#Se eliminan columnas que no son útiles para la base
Capacidad_neta_13<-Capacidad_neta_13%>% mutate(...8 = NULL)
Capacidad_neta_13<-Capacidad_neta_13%>% mutate(...9 = NULL)
Capacidad_neta_13<-Capacidad_neta_13%>% mutate(...10 = NULL)

Capacidad_neta_14<-Capacidad_neta_14%>% mutate(...8 = NULL)
Capacidad_neta_14<-Capacidad_neta_14%>% mutate(...9 = NULL)
Capacidad_neta_14<-Capacidad_neta_14%>% mutate(...10 = NULL)

Capacidad_neta_15<-Capacidad_neta_15%>% mutate(...8 = NULL)
Capacidad_neta_15<-Capacidad_neta_15%>% mutate(...9 = NULL)
Capacidad_neta_15<-Capacidad_neta_15%>% mutate(...10 = NULL)

Capacidad_neta_19<-Capacidad_neta_19%>% mutate(...8 = NULL)
Capacidad_neta_19<-Capacidad_neta_19%>% mutate(...9 = NULL)
Capacidad_neta_19<-Capacidad_neta_19%>% mutate(...10 = NULL)

Capacidad_neta_20<-Capacidad_neta_20%>% mutate(...8 = NULL)
Capacidad_neta_20<-Capacidad_neta_20%>% mutate(...9 = NULL)
Capacidad_neta_20<-Capacidad_neta_20%>% mutate(...10 = NULL)

Capacidad_neta_21<-Capacidad_neta_21%>% mutate(...8 = NULL)
Capacidad_neta_21<-Capacidad_neta_21%>% mutate(...9 = NULL)
Capacidad_neta_21<-Capacidad_neta_21%>% mutate(...10 = NULL)

Capacidad_neta_22<-Capacidad_neta_22%>% mutate(...8 = NULL)
Capacidad_neta_22<-Capacidad_neta_22%>% mutate(...9 = NULL)
Capacidad_neta_22<-Capacidad_neta_22%>% mutate(...10 = NULL)

Capacidad_neta_23<-Capacidad_neta_23%>% mutate(...8 = NULL)
Capacidad_neta_23<-Capacidad_neta_23%>% mutate(...9 = NULL)
Capacidad_neta_23<-Capacidad_neta_23%>% mutate(...10 = NULL)

Capacidad_neta_24<-Capacidad_neta_24%>% mutate(...8 = NULL)
Capacidad_neta_24<-Capacidad_neta_24%>% mutate(...9 = NULL)
Capacidad_neta_24<-Capacidad_neta_24%>% mutate(...10 = NULL)

Capacidad_neta_25<-Capacidad_neta_25%>% mutate(...8 = NULL)
Capacidad_neta_25<-Capacidad_neta_25%>% mutate(...9 = NULL)
Capacidad_neta_25<-Capacidad_neta_25%>% mutate(...10 = NULL)

Capacidad_neta_26<-Capacidad_neta_26%>% mutate(...8 = NULL)
Capacidad_neta_26<-Capacidad_neta_26%>% mutate(...9 = NULL)
Capacidad_neta_26<-Capacidad_neta_26%>% mutate(...10 = NULL)



#Se unen los datos en 1 sola base
Capacidad_neta_t<-rbind(Capacidad_neta_2, Capacidad_neta_6, Capacidad_neta_7, Capacidad_neta_8, Capacidad_neta_9,Capacidad_neta_1,Capacidad_neta_10,Capacidad_neta_11,Capacidad_neta_12,Capacidad_neta_13,Capacidad_neta_14,Capacidad_neta_15,Capacidad_neta_16,Capacidad_neta_3,Capacidad_neta_17,Capacidad_neta_18,Capacidad_neta_19,Capacidad_neta_20,Capacidad_neta_21,Capacidad_neta_22,Capacidad_neta_23,Capacidad_neta_24,Capacidad_neta_25,Capacidad_neta_26)
Capacidad_final<- Capacidad_neta_t%>% group_by(...2) %>% filter (! duplicated(...2))

#Se eliminan columnas que no sirven
Capacidad_final<-Capacidad_final%>% mutate(Capacidad= NULL)
Capacidad_final<-Capacidad_final%>% mutate(...3= NULL)
Capacidad_final<-Capacidad_final%>% mutate(...6= NULL)
Capacidad_final<-Capacidad_final%>% mutate(...7= NULL)

Generacion_tipo_1<-left_join(Generacion,Capacidad_final, by="...2")
Generacion_tipo_1<-left_join(Generacion_tipo_1, lista_agentes, by = c("...2")) %>% 
  mutate(Values_Type_f = ifelse(is.na(...4), Values_Type, ...4)) 

filtro<-is.na(Generacion_tipo_1$Values_Type_f)
table(is.na(Generacion_tipo_1$Values_Type_f))

Generacion_tipo_1<-Generacion_tipo_1%>% mutate(...3= NULL)
Generacion_tipo_1<-Generacion_tipo_1%>% mutate(...4= NULL)
Generacion_tipo_1<-Generacion_tipo_1%>% mutate(...5.y= NULL)
Generacion_tipo_1<-Generacion_tipo_1%>% mutate(Id= NULL)
Generacion_tipo_1<-Generacion_tipo_1%>% mutate(Values_Type= NULL)
Generacion_tipo_1<-Generacion_tipo_1%>% mutate(Values_Rectype= NULL)
Generacion_tipo_1<-Generacion_tipo_1%>% mutate(Values_enersource= NULL)

#Para los casos NA para la generación horaria, se imputará un valor de cero, debido a que dichos generadores
#no generaron energía durante esas horas

Generacion_tipo_1[is.na(Generacion_tipo_1)] = 0

cantidad_na <- sapply(Generacion_tipo_1, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(Generacion_tipo_1)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizo el porcentaje de los datos que tienen NA
#Se valida que no existen NA's en la base

colnames(Generacion_tipo_1)<-c('Fecha', 'Recurso', '...0', '...1', '...2', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19', '...20', '...21', '...22', '...23', 'Tipo_Generacion')
Generacion_tipo_1<- Generacion_tipo_1[c(-1),]
saveRDS(Generacion_tipo_1, "../Datos/Bases oficiales/Generacion_tipo_Gen.rds")

Generacion_tipo_1<-data.frame(readRDS("../Datos/Bases oficiales/Generacion_tipo_Gen.rds"))

Tipo_num<-case_when(Generacion_tipo_1$Tipo_Generacion=="COGENERADOR"~1,
                    Generacion_tipo_1$Tipo_Generacion=="HIDRAULICA"~2,
                    Generacion_tipo_1$Tipo_Generacion=="TERMICA"~3,
                    Generacion_tipo_1$Tipo_Generacion=="SOLAR"~4,
                    Generacion_tipo_1$Tipo_Generacion=="EOLICA"~5)
Generacion_tipo_1<-cbind(Generacion_tipo_1,Tipo_num)


#Para pasar las columnas de las horas en valores numéricos
cols.num<-c('...0', '...1', '...2', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19', '...20', '...21', '...22', '...23')
Generacion_tipo_1[cols.num] <- sapply(Generacion_tipo_1[cols.num],as.numeric)
sapply(Generacion_tipo_1, class)

#Para hora 0
Gen<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_0 = sum(...0))
Generacion_0<-Gen%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_0)

#Para hora 1
Gen1<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_1 = sum(...1))
Generacion_1<-Gen1%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_1)

#Para hora 2
Gen2<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_2 = sum(...2))
Generacion_2<-Gen2%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_2)

#Para hora 3
Gen3<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_3 = sum(...3))
Generacion_3<-Gen3%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_3)

#Para hora 4
Gen4<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_4 = sum(...4))
Generacion_4<-Gen4%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_4)

#Para hora 5
Gen5<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_5 = sum(...5))
Generacion_5<-Gen5%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_5)

#Para hora 6
Gen6<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_6 = sum(...6))
Generacion_6<-Gen6%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_6)

#Para hora 7
Gen7<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_7 = sum(...7))
Generacion_7<-Gen7%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_7)

#Para hora 8
Gen8<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_8 = sum(...8))
Generacion_8<-Gen8%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_8)

#Para hora 9
Gen9<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_9 = sum(...9))
Generacion_9<-Gen9%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_9)

#Para hora 10
Gen10<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_10 = sum(...10))
Generacion_10<-Gen10%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_10)

#Para hora 11
Gen11<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_11 = sum(...11))
Generacion_11<-Gen11%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_11)

#Para hora 12
Gen12<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_12 = sum(...12))
Generacion_12<-Gen12%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_12)

#Para hora 13
Gen13<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_13 = sum(...13))
Generacion_13<-Gen13%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_13)

#Para hora 14
Gen14<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_14 = sum(...14))
Generacion_14<-Gen14%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_14)

#Para hora 15
Gen15<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_15 = sum(...15))
Generacion_15<-Gen15%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_15)

#Para hora 16
Gen16<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_16 = sum(...16))
Generacion_16<-Gen16%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_16)

#Para hora 17
Gen17<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_17 = sum(...17))
Generacion_17<-Gen17%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_17)

#Para hora 18
Gen18<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_18 = sum(...18))
Generacion_18<-Gen18%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_18)

#Para hora 19
Gen19<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_19 = sum(...19))
Generacion_19<-Gen19%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_19)

#Para hora 20
Gen20<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_20 = sum(...20))
Generacion_20<-Gen20%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_20)

#Para hora 21
Gen21<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_21 = sum(...21))
Generacion_21<-Gen21%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_21)

#Para hora 22
Gen22<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_22 = sum(...22))
Generacion_22<-Gen22%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_22)

#Para hora 23
Gen23<- Generacion_tipo_1 %>%
  group_by(Fecha, Tipo_Generacion) %>%
  summarise(Tipo_recurso_23 = sum(...23))
Generacion_23<-Gen23%>% pivot_wider(names_from=Tipo_Generacion, values_from = Tipo_recurso_23)

#Se imputará con 0 los valores obtenidos porque pueda que para el despacho no se haya contemplado un determinado tipo de generacion

Generacion_0[is.na(Generacion_0)] = 0
Generacion_1[is.na(Generacion_1)] = 0
Generacion_2[is.na(Generacion_2)] = 0
Generacion_3[is.na(Generacion_3)] = 0
Generacion_4[is.na(Generacion_4)] = 0
Generacion_5[is.na(Generacion_5)] = 0
Generacion_6[is.na(Generacion_6)] = 0
Generacion_7[is.na(Generacion_7)] = 0
Generacion_8[is.na(Generacion_8)] = 0
Generacion_9[is.na(Generacion_9)] = 0
Generacion_10[is.na(Generacion_10)] = 0
Generacion_11[is.na(Generacion_11)] = 0
Generacion_12[is.na(Generacion_12)] = 0
Generacion_13[is.na(Generacion_13)] = 0
Generacion_14[is.na(Generacion_14)] = 0
Generacion_15[is.na(Generacion_15)] = 0
Generacion_16[is.na(Generacion_16)] = 0
Generacion_17[is.na(Generacion_17)] = 0
Generacion_18[is.na(Generacion_18)] = 0
Generacion_19[is.na(Generacion_19)] = 0
Generacion_20[is.na(Generacion_20)] = 0
Generacion_21[is.na(Generacion_21)] = 0
Generacion_22[is.na(Generacion_22)] = 0
Generacion_23[is.na(Generacion_23)] = 0


#Hasta el 30 de junio 2022
saveRDS(Generacion_0[1:8217,], "../Datos/Bases oficiales/Generacion_0_30062022.rds")
saveRDS(Generacion_1[1:8217,], "../Datos/Bases oficiales/Generacion_1_30062022.rds")
saveRDS(Generacion_2[1:8217,], "../Datos/Bases oficiales/Generacion_2_30062022.rds")
saveRDS(Generacion_3[1:8217,], "../Datos/Bases oficiales/Generacion_3_30062022.rds")
saveRDS(Generacion_4[1:8217,], "../Datos/Bases oficiales/Generacion_4_30062022.rds")
saveRDS(Generacion_5[1:8217,], "../Datos/Bases oficiales/Generacion_5_30062022.rds")
saveRDS(Generacion_6[1:8217,], "../Datos/Bases oficiales/Generacion_6_30062022.rds")
saveRDS(Generacion_7[1:8217,], "../Datos/Bases oficiales/Generacion_7_30062022.rds")
saveRDS(Generacion_8[1:8217,], "../Datos/Bases oficiales/Generacion_8_30062022.rds")
saveRDS(Generacion_9[1:8217,], "../Datos/Bases oficiales/Generacion_9_30062022.rds")
saveRDS(Generacion_10[1:8217,], "../Datos/Bases oficiales/Generacion_10_30062022.rds")
saveRDS(Generacion_11[1:8217,], "../Datos/Bases oficiales/Generacion_11_30062022.rds")
saveRDS(Generacion_12[1:8217,], "../Datos/Bases oficiales/Generacion_12_30062022.rds")
saveRDS(Generacion_13[1:8217,], "../Datos/Bases oficiales/Generacion_13_30062022.rds")
saveRDS(Generacion_14[1:8217,], "../Datos/Bases oficiales/Generacion_14_30062022.rds")
saveRDS(Generacion_15[1:8217,], "../Datos/Bases oficiales/Generacion_15_30062022.rds")
saveRDS(Generacion_16[1:8217,], "../Datos/Bases oficiales/Generacion_16_30062022.rds")
saveRDS(Generacion_17[1:8217,], "../Datos/Bases oficiales/Generacion_17_30062022.rds")
saveRDS(Generacion_18[1:8217,], "../Datos/Bases oficiales/Generacion_18_30062022.rds")
saveRDS(Generacion_19[1:8217,], "../Datos/Bases oficiales/Generacion_19_30062022.rds")
saveRDS(Generacion_20[1:8217,], "../Datos/Bases oficiales/Generacion_20_30062022.rds")
saveRDS(Generacion_21[1:8217,], "../Datos/Bases oficiales/Generacion_21_30062022.rds")
saveRDS(Generacion_22[1:8217,], "../Datos/Bases oficiales/Generacion_22_30062022.rds")
saveRDS(Generacion_23[1:8217,], "../Datos/Bases oficiales/Generacion_23_30062022.rds")


#################################################################################################
###############ONI#################################################################################


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
ONI<-data.frame(readRDS("../Datos/Bases oficiales/ONI.rds")) 
