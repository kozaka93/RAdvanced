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
autor_pakiet <- read.csv('autor_pakiet.csv')
autor_funkcja <- read.csv('autor_funkcja.csv')
library(shiny)
library(RColorBrewer)
library(igraph)

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
                 tabPanel("Tab3",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("autor1", "Wybierz autora 1:", unique(autor_pakiet$autor)),
                              #selectInput("autor2", "wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
                              uiOutput("secondSelection")
                            ),
                            mainPanel(plotOutput("igraf",  width = "100%"))
                          )
                          
                 ),
                 tabPanel("Tab4",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("autor_1", "Wybierz autora 1:", unique(autor_funkcja$autor)),
                              #selectInput("autor2", "wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
                              uiOutput("secondSelection1")
                            ),
                            mainPanel(plotOutput("igraf1",  width = "100%"))
                          )
                          
                          
                 ),
                 tabPanel("Tab5"
                          
                 ),
                 
                 tabPanel("Tab6"
                          ),
                 navbarMenu("więcej",
                            tabPanel("Dane",
                                     includeMarkdown("dane.Rmd")
                            ),
                            
                            tabPanel("Autorzy",
                                     includeMarkdown("autorzy.Rmd")
                            ))
)
                            
   
  

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
   
   output$secondSelection <- renderUI({
     selectInput("autor2", "wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
   })
   
   output$igraf <- renderPlot({
     df <- autor_pakiet[autor_pakiet$autor==input$autor1 | autor_pakiet$autor==input$autor2,]
     igraf1 <- graph_from_edgelist(as.matrix(df), directed = TRUE) 
     plot(igraf1, edge.arrow.size=.5, vertex.color="gold", vertex.size=10, 
          
          vertex.frame.color="gray88", vertex.label.color="black", 
          
          vertex.label.cex=1, vertex.label.dist=2, edge.curved=0.2)
   },
   height = 700, width = 800)
   
   
   
   output$secondSelection1 <- renderUI({
     selectInput("autor_2", "wybierz autora 2:", unique(autor_funkcja[autor_funkcja$autor!=input$autor_1,1]))
   })
   
   output$igraf1 <- renderPlot({
     df1 <- autor_funkcja[autor_funkcja$autor==input$autor_1 | autor_funkcja$autor==input$autor_2,]
     igraf_1 <- graph_from_edgelist(as.matrix(df1), directed = TRUE) 
     plot(igraf_1, edge.arrow.size=.5, vertex.color="steelblue1", vertex.size=10, 
          
          vertex.frame.color="gray88", vertex.label.color="black", 
          
          vertex.label.cex=1, vertex.label.dist=2, edge.curved=0.2)
   },
   height = 700, width = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)


