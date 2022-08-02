#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(dplyr)
library(shiny)
library(xgboost)
library(Matrix)
library(data.table)
library(lubridate)
#source("code_modelo.R")

#Se debe poner como working directory donde esté el presente archivo


#Se importan los modelos finales entrenados para realizar la predicción
model0_<- readRDS("data/model0.rds")
model1_<- readRDS("data/model1.rds")
model2_<- readRDS("data/model2.rds")
model3_<- readRDS("data/model3.rds")
model4_<- readRDS("data/model4.rds")
model5_<- readRDS("data/model5.rds")
model6_<- readRDS("data/model6.rds")
model7_<- readRDS("data/model7.rds")
model8_<- readRDS("data/model8.rds")
model9_<- readRDS("data/model9.rds")
model10_<- readRDS("data/model10.rds")
model11_<- readRDS("data/model11.rds")
model12_<- readRDS("data/model12.rds")
model13_<- readRDS("data/model13.rds")
model14_<- readRDS("data/model14.rds")
model15_<- readRDS("data/model15.rds")
model16_<- readRDS("data/model16.rds")
model17_<- readRDS("data/model17.rds")
model18_<- readRDS("data/model18.rds")
model19_<- readRDS("data/model19.rds")
model20_<- readRDS("data/model20.rds")
model21_<- readRDS("data/model21.rds")
model22_<- readRDS("data/model22.rds")
model23_<- readRDS("data/model23.rds")


ui <- fluidPage(
  
  # Application title
  titlePanel(strong(h1("Predicción del precio de bolsa del Mercado de Energía Mayorista Colombiano"))),
  
  tags$style('.container-fluid {
                             background-color: #034f84;
              }'),
  #Colores de texto
  tags$head(tags$style('h1 {color:#f0efef;}')),
  tags$head(tags$style('h2 {color:#f0efef;}')),
  tags$head(tags$style('h3 {color:#deeaee;}')),
  tags$head(tags$style('h6 {color:#deeaee;}')),
  
  
   sidebarLayout(
    
    sidebarPanel(dateInput("var8", "Fecha para predicción"),
                 numericInput("var6","ONI",value = -1.5),
                 numericInput("var7","Aportes hídricos en kWh",value = 40000000),
                 numericInput("var0","TRM en COP",value = 4519),
                 sliderInput("var1", "Generacion Eólica en kWh",min=0, max=20000, value=0),
                 sliderInput("var2", "Generación Solar en kWh",min=0, max=157726, value=0),
                 sliderInput("var3", "Generación Térmica en kWh",min=0, max=8823450, value=1500000),
                 sliderInput("var4", "Generación Hidráulica en kWh",min=0, max=11790431, value=10000000),
                 sliderInput("var5", "Cogeneración en kWh",min=0, max=178400, value=10000),
                 sliderInput("var9", "Hora para predicción del precio de bolsa",min=0, max=23, value=0),
                 ),
   
    mainPanel(h2("De acuerdo con los parámetros ingresados, el precio de bolsa del MEM para la hora:"),
              
              h3(textOutput("text1")),
              h2("es la siguiente:"),
              verbatimTextOutput("selected_var"),
              
              fluidRow(column(12,align ="center",
                              div(img(src="Image_1.png", height=200, width=300))),
              ),#closefluidRow
              h6("s.f. Fotografía. Recuperado de: https://chicanoticias.com/2022/07/13/urra-es-la-hidroelectrica-de-colombia-con-mas-carga-de-agua-en-su-embalse/ .02 de Agosto 2022"),
              h3("Para realizar este modelo, se tuvo en cuenta el modelo XG Boost con las variables para obtener la predicción solicitada.")
              
              
              )
  )
  
  
)


server <- function(input, output) {
  
  output$text1 <- renderText({input$var9 })

  
  output$selected_var <- renderPrint({
    
    
    mes <- lubridate::month(input$var8)
    año <-lubridate::year(input$var8)
    dia<-lubridate::day(input$var8)
    BASE_WEB<-cbind(input$var5, input$var4, input$var3, input$var1,input$var2, input$var6, input$var0, input$var7, dia,mes,año)
    BASE_WEB<-data.frame(BASE_WEB)
    
    #Predicciones_Bolsa<-case_when(input$var9==0~)
    
    #Para el caso de las 23hrs
    colnames(BASE_WEB)<-c('Gen_CoGenerador23','Gen_Hidraulica23','Gen_Termica23','Gen_Eolica23','Gen_Solar23','ONI','TRM','Aportes_total','dia','mes','año')
    x_test<-as.matrix(BASE_WEB)
    y_test<- as.numeric(c(0))
    xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
    
    Predicciones_PreciosBol23 <-predict(model23_, xgb_test)
   
     #Para el caso de las 0hrs
     colnames(BASE_WEB)<-c('Gen_CoGenerador0','Gen_Hidraulica0','Gen_Termica0','Gen_Eolica0','Gen_Solar0','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol0 <-predict(model0_, xgb_test)
     #Predicciones_PreciosBol0
    #Para el caso de las 1hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador1','Gen_Hidraulica1','Gen_Termica1','Gen_Eolica1','Gen_Solar1','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol1 <-predict(model1_, xgb_test)
   
     #Para el caso de las 2hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador2','Gen_Hidraulica2','Gen_Termica2','Gen_Eolica2','Gen_Solar2','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol2 <-predict(model2_, xgb_test)
     
     
     #Para el caso de las 3hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador3','Gen_Hidraulica3','Gen_Termica3','Gen_Eolica3','Gen_Solar3','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol3 <-predict(model3_, xgb_test)
     
     #Para el caso de las 4hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador4','Gen_Hidraulica4','Gen_Termica4','Gen_Eolica4','Gen_Solar4','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol4 <-predict(model4_, xgb_test)
    
     #Para el caso de las 5hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador5','Gen_Hidraulica5','Gen_Termica5','Gen_Eolica5','Gen_Solar5','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol5 <-predict(model5_, xgb_test)
     
     #Para el caso de las 6hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador6','Gen_Hidraulica6','Gen_Termica6','Gen_Eolica6','Gen_Solar6','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol6 <-predict(model6_, xgb_test)
     
     
     #Para el caso de las 7hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador7','Gen_Hidraulica7','Gen_Termica7','Gen_Eolica7','Gen_Solar7','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol7 <-predict(model7_, xgb_test)

     
    #Para el caso de las 8hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador8','Gen_Hidraulica8','Gen_Termica8','Gen_Eolica8','Gen_Solar8','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol8 <-predict(model8_, xgb_test)
     
     #Para el caso de las 9hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador9','Gen_Hidraulica9','Gen_Termica9','Gen_Eolica9','Gen_Solar9','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol9 <-predict(model9_, xgb_test)
     
     #Para el caso de las 10hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador10','Gen_Hidraulica10','Gen_Termica10','Gen_Eolica10','Gen_Solar10','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol10 <-predict(model10_, xgb_test)
      
     #Para el caso de las 11hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador11','Gen_Hidraulica11','Gen_Termica11','Gen_Eolica11','Gen_Solar11','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol11 <-predict(model11_, xgb_test)
     
    #Para el caso de las 12hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador12','Gen_Hidraulica12','Gen_Termica12','Gen_Eolica12','Gen_Solar12','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol12 <-predict(model12_, xgb_test)
 
     #Para el caso de las 13hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador13','Gen_Hidraulica13','Gen_Termica13','Gen_Eolica13','Gen_Solar13','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol13 <-predict(model13_, xgb_test)
     
    # #Para el caso de las 14hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador14','Gen_Hidraulica14','Gen_Termica14','Gen_Eolica14','Gen_Solar14','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol14 <-predict(model14_, xgb_test)
    
    # #Para el caso de las 15hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador15','Gen_Hidraulica15','Gen_Termica15','Gen_Eolica15','Gen_Solar15','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol15 <-predict(model15_, xgb_test)
     
     #Para el caso de las 16hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador16','Gen_Hidraulica16','Gen_Termica16','Gen_Eolica16','Gen_Solar16','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol16 <-predict(model16_, xgb_test)
     
     #Para el caso de las 17hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador17','Gen_Hidraulica17','Gen_Termica17','Gen_Eolica17','Gen_Solar17','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol17 <-predict(model17_, xgb_test)
      
    #Para el caso de las 18hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador18','Gen_Hidraulica18','Gen_Termica18','Gen_Eolica18','Gen_Solar18','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol18 <-predict(model18_, xgb_test)
  
    #Para el caso de las 19hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador19','Gen_Hidraulica19','Gen_Termica19','Gen_Eolica19','Gen_Solar19','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol19 <-predict(model19_, xgb_test)
     
     #Para el caso de las 20hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador20','Gen_Hidraulica20','Gen_Termica20','Gen_Eolica20','Gen_Solar20','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol20 <-predict(model20_, xgb_test)
      
    #Para el caso de las 21hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador21','Gen_Hidraulica21','Gen_Termica21','Gen_Eolica21','Gen_Solar21','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol21 <-predict(model21_, xgb_test)
     
     #Para el caso de las 22hr
     colnames(BASE_WEB)<-c('Gen_CoGenerador22','Gen_Hidraulica22','Gen_Termica22','Gen_Eolica22','Gen_Solar22','ONI','TRM','Aportes_total','dia','mes','año')
     x_test<-as.matrix(BASE_WEB)
     y_test<- as.numeric(c(0))
     xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
     
     Predicciones_PreciosBol22 <-predict(model22_, xgb_test)
   
     
     # Predicciones_PreciosBol<-switch(input$var9,
     #                                 Predicciones_PreciosBol0,
     #                                 Predicciones_PreciosBol1,
     #                                 Predicciones_PreciosBol2,
     #                                 Predicciones_PreciosBol3,
     #                                 Predicciones_PreciosBol4,
     #                                 Predicciones_PreciosBol5)
     # 
   
    # 
    #  Predicciones_PreciosBol_g1<-ifelse(input$var9==0, Predicciones_PreciosBol0, ifelse(input$var9==1, Predicciones_PreciosBol1, ifelse(input$var9==2, Predicciones_PreciosBol2, ifelse(input$var9==3, Predicciones_PreciosBol3,0))))
    # Predicciones_PreciosBol_g1
    #  Predicciones_PreciosBol_g2<-ifelse(input$var==4, Predicciones_PreciosBol4, ifelse(input$var9==5, Predicciones_PreciosBol5, ifelse(input$var9==6, Predicciones_PreciosBol6, ifelse(input$var9==7, Predicciones_PreciosBol7, 0))))
    # 
    # Predicciones_PreciosBol_g3<-ifelse(input$var9==8, Predicciones_PreciosBol8, ifelse(input$var9==9, Predicciones_PreciosBol9, ifelse(input$var9==10, Predicciones_PreciosBol10, ifelse(input$var9==11, Predicciones_PreciosBol11,0))))
    # 
    # Predicciones_PreciosBol_g4<-ifelse(input$var9==12, Predicciones_PreciosBol12, ifelse(input$var9==13, Predicciones_PreciosBol13, ifelse(input$var9==14, Predicciones_PreciosBol14, ifelse(input$var9==15, Predicciones_PreciosBol15,0))))
    # 
    # Predicciones_PreciosBol_g5<-ifelse(input$var9==16, Predicciones_PreciosBol16, ifelse(input$var9==17, Predicciones_PreciosBol17, ifelse(input$var9==18, Predicciones_PreciosBol18, ifelse(input$var9==19, Predicciones_PreciosBol19,0))))
    # 
    # Predicciones_PreciosBol_g6<-ifelse(input$var9==20, Predicciones_PreciosBol20, ifelse(input$var9==21, Predicciones_PreciosBol21, ifelse(input$var9==22, Predicciones_PreciosBol22, ifelse(input$var9==23, Predicciones_PreciosBol23, 0))))
    # 
    # # 
    # 
     
     if (input$var9==0){
       Predicciones_PreciosBol<-Predicciones_PreciosBol0
     }
     else if (input$var9==1){
       Predicciones_PreciosBol<-Predicciones_PreciosBol1
     }
     else if (input$var9==2){
       Predicciones_PreciosBol<-Predicciones_PreciosBol2
     }
     else if (input$var9==3){
       Predicciones_PreciosBol<-Predicciones_PreciosBol3
     }
     else if (input$var9==4){
       Predicciones_PreciosBol<-Predicciones_PreciosBol4
     }
     else if (input$var9==5){
       Predicciones_PreciosBol<-Predicciones_PreciosBol5
     }
     else if (input$var9==6){
       Predicciones_PreciosBol<-Predicciones_PreciosBol6
     }
     else if (input$var9==7){
       Predicciones_PreciosBol<-Predicciones_PreciosBol7
     }
     else if (input$var9==8){
       Predicciones_PreciosBol<-Predicciones_PreciosBol8
     }
     else if (input$var9==9){
       Predicciones_PreciosBol<-Predicciones_PreciosBol9
     }
     else if (input$var9==10){
       Predicciones_PreciosBol<-Predicciones_PreciosBol10
     }
     else if (input$var9==11){
       Predicciones_PreciosBol<-Predicciones_PreciosBol11
     }
     else if (input$var9==12){
       Predicciones_PreciosBol<-Predicciones_PreciosBol12
     }
     else if (input$var9==13){
       Predicciones_PreciosBol<-Predicciones_PreciosBol13
     }
     else if (input$var9==14){
       Predicciones_PreciosBol<-Predicciones_PreciosBol14
     }
     else if (input$var9==15){
       Predicciones_PreciosBol<-Predicciones_PreciosBol15
     }
     else if (input$var9==16){
       Predicciones_PreciosBol<-Predicciones_PreciosBol16
     }
     else if (input$var9==17){
       Predicciones_PreciosBol<-Predicciones_PreciosBol17
     }
     else if (input$var9==18){
       Predicciones_PreciosBol<-Predicciones_PreciosBol18
     }
     else if (input$var9==19){
       Predicciones_PreciosBol<-Predicciones_PreciosBol19
     }
     else if (input$var9==20){
       Predicciones_PreciosBol<-Predicciones_PreciosBol20
     }
     else if (input$var9==21){
       Predicciones_PreciosBol<-Predicciones_PreciosBol21
     }
     else if (input$var9==22){
       Predicciones_PreciosBol<-Predicciones_PreciosBol22
     }
     else if (input$var9==23){
       Predicciones_PreciosBol<-Predicciones_PreciosBol23
     }
     Predicciones_PreciosBol
     
   
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
