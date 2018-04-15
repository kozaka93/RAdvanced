#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
slowa <- read.csv("C:/Users/Anna Kozak/Desktop/shiny_app_csv/funkcje_czestosc.csv", row.names = NULL)



library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Analiza",
                 tabPanel("Tab1",
                          sidebarLayout(
                            sidebarPanel(
                             sliderInput("ile", "Maksymalna liczba słów:", min=1, max=340, value=100)
                              
                            ),
                            mainPanel(plotOutput("plot",  width = "100%"))
                          )),
                 tabPanel("Tab2"
                         
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
     slowa <- slowa[1:ile_slow,]
     wordcloud::wordcloud(words = slowa[,1], freq = slowa[,3], random.order = FALSE, colors=brewer.pal(8, "Accent"))
     },
     
     height = 700, width = 800
     )
}

# Run the application 
shinyApp(ui = ui, server = server)

