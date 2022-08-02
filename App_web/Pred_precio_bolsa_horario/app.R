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
library(pacman)
library(ranger)
library(xgboost)
#source("code_modelo.R")

# Define UI for application that draws a histogram

DTEST_H<-data.frame(readRDS("data/DTESTHOUSE.rds"))  #Guardar las bases de datos
DTRAIN_H<-data.frame(readRDS("data/DTRAINHOUSE.rds"))
# 
#Se elimina la columna de precio en la base Dtest_H
DTEST_H<-DTEST_H%>% mutate(price = NULL)
# #Se crean las variables Dummies para la ciudad (Bogotá o Medellin) y para el tipo de propiedad (Casa o Apartamento)
# #Para la dummy de ciudad se escogió que Medellín fuese 1, Bogotá 0 y Apartamento es 1 y Casa es igual a cero
# 
DTEST_H<-DTEST_H%>% mutate(Medellin = ifelse(l3=="Medellín", 1,0))
DTRAIN_H<-DTRAIN_H%>% mutate(Medellin = ifelse(l3=="Medellín", 1,0))
# 
DTEST_H<-DTEST_H%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))
DTRAIN_H<-DTRAIN_H%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))

DTRAIN_H<-DTRAIN_H%>% mutate(l3=NULL)
DTRAIN_H<-DTRAIN_H%>% mutate(property_type=NULL)
DTRAIN_H<-DTRAIN_H%>% mutate(MANZ_CCNCT=NULL)
DTRAIN_H<-DTRAIN_H%>% mutate(geometry=NULL)
DTRAIN_H<-DTRAIN_H%>% mutate(property_type=NULL)
DTRAIN_H<-DTRAIN_H%>% mutate(base=NULL)


DTEST_H<-DTEST_H%>% mutate(l3=NULL)
DTEST_H<-DTEST_H%>% mutate(property_type=NULL)
DTEST_H<-DTEST_H%>% mutate(MANZ_CCNCT=NULL)
DTEST_H<-DTEST_H%>% mutate(geometry=NULL)
DTEST_H<-DTEST_H%>% mutate(property_type=NULL)
DTEST_H<-DTEST_H%>% mutate(base=NULL)

#Modelo 3 - Random Forest

#Se entrena el modelo 3 con las 10 variables. Se realizaron varias iteraciones y se determinó que el número de árboles con el que se obtiene el menor RMSE es 1000, así como menor valor y mayor cantidad de viviendas compradas.

set.seed(10101)
modelo3_forest1000 <- ranger(
  price ~ Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park+surface_new_3,
  data = DTRAIN_H,
  num.trees = 1000,
  write.forest = TRUE
)


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
    #sidebarPanel("lalal"),
    sidebarPanel(numericInput("var0","Numeric input",value = 123456),
                 numericInput("var1","Numeric input",value = 1),
                 numericInput("var2","Numeric input",value = 1),
                 numericInput("var3","Numeric input",value = 1),
                 numericInput("var4","Numeric input",value = 0),
                 numericInput("var5","Numeric input",value = 4),
                 numericInput("var6","Numeric input",value = 5),
                 numericInput("var7","Numeric input",value = 400),
                 numericInput("var8","Numeric input",value = 700),
                 numericInput("var9","Numeric input",value = 500),
                 numericInput("var10","Numeric input",value = 50),
                 selectInput("var",
                             label = "Choose a variable to display",
                             choices = c("Percent White",
                                         "Percent Black",
                                         "Percent Hispanic",
                                         "Percent Asian"),
                             selected = "Percent White")),
    #mainPanel("jejej")
    mainPanel(h1("Esta es la predicción del precio de bolsa"),
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
    
    BASE_WEB<-cbind(input$var0, input$var10, input$var1, input$var2, input$var5, input$var6, input$var7, input$var8, input$var9, input$var3, input$var4)
    BASE_WEB<-data.frame(BASE_WEB)
    colnames(BASE_WEB)<-c('property_id','surface_new_3','parqueaderoT','ascensorT','bathrooms','habitaciones', 'min_dist_bar_','min_dist_transp_','min_dist_park','Medellin', 'Apto')
    DTEST_H<-rbind(DTEST_H,BASE_WEB)
    Predicciones_PreciosViv <- predict(modelo3_forest1000,  data = BASE_WEB)$predictions #Se realiza predicción sobre la base Test con el modelo RF de 1000 árboles
    
    Predicciones_PreciosViv
    
    #input$var
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
