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
autor_pakiet_B <- read.csv('autor_pakiet_B.csv')
autor_funkcja_B <- read.csv('autor_funkcja_B.csv')
slowa_funckje_B <- read.csv('B_funkcje_czestosc.csv')
slowa_pakiety_B <- read.csv('B_biblioteki_czestosc.csv')
razem_b <- read.csv('razem_b.csv')
razem_f <- read.csv('razem_f.csv')

library(shiny)
library(RColorBrewer)
library(igraph)
library(dplyr)
library(ggplot2)
library(gridExtra)
# Define UI for application that draws a histogram
ui <- navbarPage("Analiza kodów źródłowych R", header = tags$head(tags$style(HTML("
                            .col-sm-8 {
                              width: 100%;
                            }

                            "))),
                 tabPanel("Wstęp",
                          includeMarkdown("wstep.Rmd")
                          #includeMarkdown("wstep.Rmd")
                 ),
                
                 tabPanel("A - Graf pakietów",
                          headerPanel("Graf wykorzystywanych pakietów przez autorów"),
                          column(3,
                              selectInput("autor1", "Wybierz autora 1:", unique(autor_pakiet$autor))
                              ),
                              #selectInput("autor2", "wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
                          column(3,  
                                 uiOutput("secondSelection")
                            ),
                            mainPanel(plotOutput("igraf",  width = "100%"))
    
                          
                 ),
                 tabPanel("A - Graf funkcji",
                          headerPanel("Graf wykorzystywanych funkcji przez autorów"),
                            column(3,
                              selectInput("autor_1", "Wybierz autora 1:", unique(autor_funkcja$autor))
                              #selectInput("autor2", "wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
                            ),
                              column(3,
                                uiOutput("secondSelection1")
                            ),
                            mainPanel(plotOutput("igraf1",  width = "100%"))
                          
                 ),
                 
                 
                 tabPanel("B - Graf pakietów",
                          headerPanel("Graf wykorzystywanych pakietów przez autorów"),
                          column(3,
                              selectInput("autor1B", "Wybierz autora 1:", unique(autor_pakiet_B$Author))
                          ),
                          column(3,
                              #selectInput("autor2", "wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
                              uiOutput("secondSelectionB")
                            ),
                            mainPanel(plotOutput("igrafB",  width = "100%"))
                 ),
                 tabPanel("B - Graf funkcji",
                          headerPanel("Graf wykorzystywanych funkcji przez autorów"),
                          column(3,
                              selectInput("autor_1B", "Wybierz autora 1:", unique(autor_funkcja_B$Author))
                          ),
                          column(3,
                              #selectInput("autor2", "wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
                              uiOutput("secondSelection1B")
                            ),
                            mainPanel(plotOutput("igraf1B",  width = "100%"))
                          
                 ),
                 tabPanel("AB - Pakiety",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("top_p", "Top pakiety:", min=1, max=50, value=10)
                              
                            ),
                            mainPanel(plotOutput("plot_p_top",  width = "100%",height = "600px"))
                          )
                 ),
                 tabPanel("AB - Funkcje",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("top_f", "Top funkcje:", min=1, max=50, value=10)
                              
                            ),
                            mainPanel(plotOutput("plot_f_top",  width = "100%", height = "600px"))
                          )
                 ),
                 
                 
                
                 ####
                 tabPanel("Pakiety - chmury słów",
                          splitLayout(
                            fluidRow(
                              headerPanel("Kategoria A"),
                              column(width = 12, sliderInput("ile1",
                                                             "Maksymalna liczba słów:",
                                                             min=1, max=200, value=100)),  ### MIN I MAX ZMIENIC
                              column(width = 12,plotOutput("plot1",  width = "80%", height = "600px"))),
                            fluidRow(
                              headerPanel("Kategoria B"),
                              column(width = 12, sliderInput("ile1B", 
                                                             "Maksymalna liczba pakietów:",
                                                             min=1, max=200, value=100)),
                              column(width = 12,plotOutput("plot1B", width = "80%", height = "600px")))
                            
                          )
                 ),
                 
                 tabPanel("Funkcje - chmury słów",
                          splitLayout(
                            fluidRow(
                              headerPanel("Kategoria A"),
                              column(width = 12, sliderInput("ile",
                                                             "Maksymalna liczba funkcji:",
                                                             min=1, max=300, value=100)),
                              column(width = 12,plotOutput("plot",  width = "80%", height = "600px"))),
                            fluidRow(
                              headerPanel("Kategoria B"),
                              column(width = 12, sliderInput("ileB", 
                                                             "Maksymalna liczba słów:",
                                                             min=1, max=300, value=100)), ## MIN I MAX ZMIENIC
                              column(width = 12,plotOutput("plotB", width = "80%", height = "600px")))
                            
                          )
                          
                          
                          
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
    wordcloud::wordcloud(words = slowa[,1], freq = slowa[,2], random.order = FALSE, colors=brewer.pal(8, "Accent"))
  }#,
  
  #height = 700, width = 800
  )
  
  output$plot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    
    ile_slow1 <- input$ile1 #parametr podawany przez uzytkownika
    slowa1 <- slowa_pakiety[1:ile_slow1,]
    wordcloud::wordcloud(words = slowa1[,1], freq = slowa1[,2], random.order = FALSE, colors=brewer.pal(8, "Accent"))
  }#,
  
  #height = 700, width = 800
  )
  
  output$secondSelection <- renderUI({
    selectInput("autor2", "Wybierz autora 2:", unique(autor_pakiet[autor_pakiet$autor!=input$autor1,1]))
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
    selectInput("autor_2", "Wybierz autora 2:", unique(autor_funkcja[autor_funkcja$autor!=input$autor_1,1]))
  })
  
  output$igraf1 <- renderPlot({
    df1 <- autor_funkcja[autor_funkcja$autor==input$autor_1 | autor_funkcja$autor==input$autor_2,]
    igraf_1 <- graph_from_edgelist(as.matrix(df1), directed = TRUE) 
    plot(igraf_1, edge.width =1 ,edge.arrow.size=.5, vertex.color="steelblue1", vertex.size=5, 
         
         edge.arrow.size=0.5, vertex.frame.color="gray88", vertex.label.color="black", 
         
         vertex.label.cex=0.75, vertex.label.dist=1, edge.curved=0.2)
  },
  height = 1000, width = 1000)
  
  ###################### kat B
  
  output$plotB <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    
    ile_slow <- input$ileB #parametr podawany przez uzytkownika
    #tu trzeba csv wygenerowac
    slowa <- slowa_funckje_B[1:ile_slow,]
    wordcloud::wordcloud(words = slowa[,1], freq = slowa[,2], random.order = FALSE, colors=brewer.pal(8, "Accent"))
  }#,
  
  # height = 700, width = 800
  )
  
  output$plot1B <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    
    ile_slow1 <- input$ile1B #parametr podawany przez uzytkownika
    
    #tu trzeba csv wygenerowac
    slowa1 <- slowa_pakiety_B[1:ile_slow1,]
    wordcloud::wordcloud(words = slowa1[,1], freq = slowa1[,2], random.order = FALSE, colors=brewer.pal(8, "Accent"))
  }#,
  
  #height = 700, width = 800
  )
  
  output$secondSelectionB <- renderUI({
    selectInput("autor2B", "Wybierz autora 2:", unique(autor_pakiet_B[autor_pakiet_B$Author!=input$autor1B,2]))
  })
  
  output$igrafB <- renderPlot({
    df <- autor_pakiet_B[autor_pakiet_B$Author==input$autor1B | autor_pakiet_B$Author==input$autor2B,2:3]
    igraf1B <- graph_from_edgelist(as.matrix(df), directed = TRUE) 
    plot(igraf1B, edge.arrow.size=.5, vertex.color="gold", vertex.size=10, 
         
         vertex.frame.color="gray88", vertex.label.color="black", 
         
         vertex.label.cex=1, vertex.label.dist=2, edge.curved=0.2)
  },
  height = 700, width = 800
  )
  
  
  
  output$secondSelection1B <- renderUI({
    selectInput("autor_2B", "Wybierz autora 2:", unique(autor_funkcja_B[autor_funkcja_B$Author!=input$autor_1B,2]))
  })
  
  output$igraf1B <- renderPlot({
    df1 <- autor_funkcja_B[autor_funkcja_B$Author==input$autor_1B | autor_funkcja_B$Author==input$autor_2B,2:3]
    igraf_1B <- graph_from_edgelist(as.matrix(df1), directed = TRUE) 
    plot(igraf_1B, edge.width =1 ,edge.arrow.size=.5, vertex.color="steelblue1", vertex.size=5, 
         
         edge.arrow.size=0.5, vertex.frame.color="gray88", vertex.label.color="black", 
         
         vertex.label.cex=0.75, vertex.label.dist=1, edge.curved=0.2)
  },
  height = 1000, width = 1000
  )
  
  output$plot_f_top <- renderPlot({
    
    razem_top_f_a <- razem_f %>%
      filter(kategoria == "A") %>%
      arrange(kategoria, -czestosc) %>%
      top_n(input$top_f, czestosc)
    
    razem_top_f_b <- razem_f %>%
      filter(kategoria == "B") %>%
      arrange(kategoria, -czestosc) %>%
      top_n(input$top_f, czestosc)
      
    print(razem_top_f_b[1:20,])
    par(mfrow=c(2,1))
    razem_top_f_a %>%
      mutate(funkcja = reorder(funkcja, czestosc)) %>%
      ggplot(aes(funkcja, czestosc, fill = factor(kategoria))) +
      geom_bar(stat='identity', show.legend = FALSE) +
      facet_wrap(~ kategoria, scales = "free") +
      coord_flip() +
      theme(text = element_text(size=15)) -> plot_f_a
    razem_top_f_b %>%
      mutate(funkcja = reorder(funkcja, czestosc)) %>%
      ggplot(aes(funkcja, czestosc, fill = as.numeric(factor(kategoria)) )) +
      geom_bar(stat='identity', show.legend = FALSE) +
      facet_wrap(~ kategoria, scales = "free") +
      coord_flip() +
      theme(text = element_text(size=15))-> plot_f_b
    
    grid.arrange(plot_f_a, plot_f_b, ncol=2)
    
  })
  
  
  
  output$plot_p_top <- renderPlot({
    
    razem_top_b_a <- razem_b %>%
      filter(kategoria == "A") %>%
      arrange(kategoria, -czestosc) %>%
      top_n(input$top_p, czestosc)
    
    razem_top_b_b <- razem_b %>%
      filter(kategoria == "B") %>%
      arrange(kategoria, -czestosc) %>%
      top_n(input$top_p, czestosc)
    
    razem_top_b_a %>%
      mutate(pakiet = reorder(biblioteka, czestosc)) %>%
      ggplot(aes(pakiet, czestosc, fill = factor(kategoria))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      theme(text = element_text(size=15)) -> plot_b_a
    
    razem_top_b_b %>%
      mutate(pakiet = reorder(biblioteka, czestosc)) %>%
      ggplot(aes(pakiet, czestosc, fill = as.numeric(factor(kategoria)))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      theme(text = element_text(size=15)) -> plot_b_b
    grid.arrange(plot_b_a, plot_b_b, ncol=2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


