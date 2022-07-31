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
###Precio de bolsa
Precio_Bolsa_2000 <- read_excel("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Precio_Bolsa_Nacional_($kwh)_2000.xlsx")
colnames(Precio_Bolsa_2000) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
View(Precio_Bolsa_2000)
Precio_Bolsa_2001 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2001.xlsx")
colnames(Precio_Bolsa_2001) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2001<- Precio_Bolsa_2001[c(-1),]
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
Precio_Bolsa_2004<- Precio_Bolsa_200[c(-1,-2),]
View(Precio_Bolsa_2004)
Precio_Bolsa_2005 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2005.xlsx")
colnames(Precio_Bolsa_2005) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2005<- Precio_Bolsa_2005[c(-1,-2),]
View(Precio_Bolsa_2005)
Precio_Bolsa_2006 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2006.xlsx")
colnames(Precio_Bolsa_2006) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2006<- Precio_Bolsa_2006[c(-1,-2),]
View(Precio_Bolsa_2006)
Precio_Bolsa_2007 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2007.xlsx")
colnames(Precio_Bolsa_2007) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2007<- Precio_Bolsa_2007[c(-1,-2),]
View(Precio_Bolsa_2007)
Precio_Bolsa_2008 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2008.xlsx")
colnames(Precio_Bolsa_2008) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2008<- Precio_Bolsa_2008[c(-1,-2),]
View(Precio_Bolsa_2008)
Precio_Bolsa_2009 <- read_excel("../Datos/Precio_Bolsa_Nacional_($kwh)_2009.xlsx")
colnames(Precio_Bolsa_2009) <- c('...1', 'Precio Bolsa Nacional', '...3', '...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', '...25', '...26')
Precio_Bolsa_2009<- Precio_Bolsa_2009[c(-1,-2),]
View(Precio_Bolsa_2009)
