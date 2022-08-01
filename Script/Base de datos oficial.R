Gen<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Generacion_23_30062022.rds")
ONI<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/ONI.rds")
TRM <- readRDS(("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/TRM.rds"))
TRM <- TRM[c(-8218:-8247),]
AD <- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Aportes_energia_dia_30_06_2022.rds")
PB<- readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Proyecto final/Proyecto-final-MEcA-4107/Datos/Bases oficiales/Precios_Bolsa_2.rds")
PB <- PB[c(-8219:-8246),]
colnames(PB) <- c('...1', '...2', '...3','...4', '...5', '...6', '...7', '...8', '...9', '...10', '...11', '...12', '...13', '...14', '...15', '...16', '...17', '...18', '...19','...20', '...21', '...22','...23', '...24', 'Precio Nacional Bolsa')
PrecioBolsa <- PB[c(-1),]
BASEOF<- cbind(PrecioBolsa$...1,  PrecioBolsa$`Precio Nacional Bolsa`,Gen$COGENERADOR, Gen$HIDRAULICA, Gen$TERMICA, Gen$EOLICA, Gen$SOLAR, AD$Aportes_total, ONI$ONI, TRM$TRM)
BASEOF<- data.frame(BASEOF)
colnames(BASEOF) <- c('Fecha', 'Precio_Bolsa_Nacional', 'Gen_CoGenerador','Gen_Hidraulica', 'Gen_Termica', 'Gen_Eolica', 'Gen_Solar', 'Aportes_Diarios', 'ONI', 'TRM')
saveRDS(BASEOF, "../Datos/Bases oficiales/BASEOF.rds" )

