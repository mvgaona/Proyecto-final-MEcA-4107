#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("dplyr")
library(dplyr)
library(shiny)
library(xgboost)
library(Matrix)
library(data.table)
library(lubridate)
#source("code_modelo.R")

# Define UI for application that draws a histogram

# DTEST_H<-data.frame(readRDS("data/DTESTHOUSE.rds"))  #Guardar las bases de datos
# DTRAIN_H<-data.frame(readRDS("data/DTRAINHOUSE.rds"))
# # 
# #Se elimina la columna de precio en la base Dtest_H
# DTEST_H<-DTEST_H%>% mutate(price = NULL)
# # #Se crean las variables Dummies para la ciudad (Bogotá o Medellin) y para el tipo de propiedad (Casa o Apartamento)
# # #Para la dummy de ciudad se escogió que Medellín fuese 1, Bogotá 0 y Apartamento es 1 y Casa es igual a cero
# # 
# DTEST_H<-DTEST_H%>% mutate(Medellin = ifelse(l3=="Medellín", 1,0))
# DTRAIN_H<-DTRAIN_H%>% mutate(Medellin = ifelse(l3=="Medellín", 1,0))
# # 
# DTEST_H<-DTEST_H%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))
# DTRAIN_H<-DTRAIN_H%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))
# 
# DTRAIN_H<-DTRAIN_H%>% mutate(l3=NULL)
# DTRAIN_H<-DTRAIN_H%>% mutate(property_type=NULL)
# DTRAIN_H<-DTRAIN_H%>% mutate(MANZ_CCNCT=NULL)
# DTRAIN_H<-DTRAIN_H%>% mutate(geometry=NULL)
# DTRAIN_H<-DTRAIN_H%>% mutate(property_type=NULL)
# DTRAIN_H<-DTRAIN_H%>% mutate(base=NULL)
# 
# 
# DTEST_H<-DTEST_H%>% mutate(l3=NULL)
# DTEST_H<-DTEST_H%>% mutate(property_type=NULL)
# DTEST_H<-DTEST_H%>% mutate(MANZ_CCNCT=NULL)
# DTEST_H<-DTEST_H%>% mutate(geometry=NULL)
# DTEST_H<-DTEST_H%>% mutate(property_type=NULL)
# DTEST_H<-DTEST_H%>% mutate(base=NULL)
# 
# #Modelo 3 - Random Forest
# 
# #Se entrena el modelo 3 con las 10 variables. Se realizaron varias iteraciones y se determinó que el número de árboles con el que se obtiene el menor RMSE es 1000, así como menor valor y mayor cantidad de viviendas compradas.
# 
# set.seed(10101)
# modelo3_forest1000 <- ranger(
#   price ~ Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park+surface_new_3,
#   data = DTRAIN_H,
#   num.trees = 1000,
#   write.forest = TRUE
# )

#BASEOF<- readRDS("data/Base_de_datos_oficial_2.rds")
# train <- BASEOF[1:5751, ] # initial data (70% de los datos desde el 2001)
# pred <- BASEOF[(5752:8217), ] # extended time index (30% restante)

model23_<- readRDS("data/model23.rds")

# 
# Var0=123456
# Var1=1
# Var2=1
# Var3=1
# Var4=0
# Var5=4
# Var6=5
# Var7=400
# Var8=700
# Var9=500
# Var10=50
# 

ui <- fluidPage(
  
  # Application title
  titlePanel("Predicción del precio de bolsa del Mercado de Energía Mayorista Colombiano"),
  sidebarLayout(
    
    sidebarPanel(dateInput("var8", "Fecha para predicción"),
                 numericInput("var0","TRM",value = 4519),
                 numericInput("var1","Generacion Eólica en kWh",value = 10000),
                 numericInput("var2","Generación Solar en kWh",value = 0),
                 numericInput("var3","Generación Térmica en kWh",value = 10000000),
                 numericInput("var4","Generación Hidráulica en kWh",value = 1000000000),
                 numericInput("var5","Cogeneración en kWh",value = 4),
                 numericInput("var6","ONI",value = -1.5),
                 numericInput("var7","Aportes hídricos en kWh",value = 40000000),
                 
                 sliderInput("var9", "Hora para predicción del precio de bolsa",min=0, max=23, value=0),
                 ),
   
    mainPanel(h1("Esta es la predicción del precio de bolsa a la hora seleccionada"),
              #textOutput("selected_var")
              verbatimTextOutput("selected_var")
    )
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Var0=input$var0
  # Var1=1
  # Var2=1
  # Var3=1
  # Var4=0
  # Var5=4
  # Var6=5
  # Var7=400
  # Var8=700
  # Var9=500
  # Var10=50
  
  # BASE_WEB<-cbind(input$var0, input$var10, input$var1, input$var2, input$var5, input$var6, input$var7, input$var8, input$var9, input$var3, input$var4)
  # BASE_WEB<-cbind( Var0 ,input$var10, input$var1, input$var2, input$var5, input$var6, input$var7, input$var8, input$var9, input$var3, input$var4)
  # 
  # 
  # BASE_WEB<-data.frame(BASE_WEB)
  # 
  # 
  # colnames(BASE_WEB)<-c('property_id','surface_new_3','parqueaderoT','ascensorT','bathrooms','habitaciones', 'min_dist_bar_','min_dist_transp_','min_dist_park','Medellin', 'Apto')
  # 
  # DTEST_H<-rbind(DTEST_H,BASE_WEB)
  # 
  # Predicciones_PreciosViv <- predict(modelo3_forest1000,  data = BASE_WEB)$predictions #Se realiza predicción sobre la base Test con el modelo RF de 1000 árboles
  
  
  
  #output$selected_var <- renderText({"You have selected this"})
  output$selected_var <- renderPrint({
    
    
    mes <- lubridate::month(input$var8)
    año <-lubridate::year(input$var8)
    dia<-lubridate::day(input$var8)
    BASE_WEB<-cbind(input$var5, input$var4, input$var3, input$var1,input$var2, input$var6, input$var0, input$var7, dia,mes,año)
    BASE_WEB<-data.frame(BASE_WEB)
    
    #Predicciones_Bolsa<-case_when(input$var9==0~)
    
    colnames(BASE_WEB)<-c('Gen_CoGenerador23','Gen_Hidraulica23','Gen_Termica23','Gen_Eolica23','Gen_Solar23','ONI','TRM','Aportes_total','dia','mes','año')

    #x_test <- model.matrix(~Gen_CoGenerador23+ Gen_Hidraulica23 + Gen_Termica23+ Gen_Eolica23 + Gen_Solar23+ ONI + TRM + Aportes_total+dia+mes+año, data =BASE_WEB)[, -1]
    x_test<-as.matrix(BASE_WEB)
    y_test<- as.numeric(c(0))
    xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
    
    Predicciones_PreciosBol23 <-predict(model23_, xgb_test)
     
    Predicciones_PreciosBol
    
    #input$var
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
