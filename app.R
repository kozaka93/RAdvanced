#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#setwd("C:/Users/Anna Kozak/Desktop/shiny_app_csv/app/")
slowa_funckje <- read.csv("funkcje_czestosc.csv", row.names = NULL)
slowa_pakiety <- read.csv("biblioteki_czestosc.csv", row.names = NULL)

library(shiny)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- navbarPage("Analiza",
                 tabPanel("Tab1",
                          sidebarLayout(
                            sidebarPanel(
                             sliderInput("ile", "Maksymalna liczba słów:", min=1, max=340, value=100)
                              
                            ),
                            mainPanel(plotOutput("plot",  width = "100%"))
                          )),
                 tabPanel("Tab2",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("ile1", "Maksymalna liczba słów:", min=1, max=244, value=100)
                              
                            ),
                            mainPanel(plotOutput("plot1",  width = "100%"))
                          )
                         
                          ),
                 tabPanel("Tab3"
                          
                 ),
                 tabPanel("Tab4"
                          
                 ),
                 tabPanel("Tab5"
                          
                 ),
                 
                 tabPanel("Tab6"
                          ))
                 
   
  

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
      # generate bins based on input$bins from ui.R
     
    
     ile_slow <- input$ile #parametr podawany przez uzytkownika
     slowa <- slowa_funckje[1:ile_slow,]
     wordcloud::wordcloud(words = slowa[,1], freq = slowa[,3], random.order = FALSE, colors=brewer.pal(8, "Accent"))
     },
     
     height = 700, width = 800
     )
   
   output$plot1 <- renderPlot({
     # generate bins based on input$bins from ui.R
     
     
     ile_slow1 <- input$ile1 #parametr podawany przez uzytkownika
     slowa1 <- slowa_pakiety[1:ile_slow1,]
     wordcloud::wordcloud(words = slowa1[,1], freq = slowa1[,3], random.order = FALSE, colors=brewer.pal(8, "Accent"))
   },
   
   height = 700, width = 800
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

