
library(tidyverse)
library(shiny)
library(ggridges)


### Source[Introduction to Shiny App] (https://bios2.github.io/posts/2021-06-22-introduction-to-shiny-apps/)
# User Interface

ui <- fluidPage(
    titlePanel("Exploring volcano explosivity"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        label = h3("Years"),
                        min = 1900, max = 2020,
                        value = c(2010, 2020)
            )
        ),
        
        mainPanel(
            plotOutput("ridgePlot"),
            tableOutput("erupt_table")
        )
    )
)

# server

server <- function(input, output) {
    eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
    
    eruptions <- eruptions %>% 
        group_by(volcano_name) %>% 
        mutate(count = n()) %>% 
        filter(!is.na(vei),
               count > 70) 
    
    eruptions_filtered <- reactive({
        eruptions %>% 
            filter(start_year >= input$years[1],
                   end_year <= input$years[2])
    })
    
    output$erupt_table <- renderTable({
        head(eruptions_filtered())
    })
    
    output$ridgePlot <- renderPlot({
        ggplot(data = eruptions_filtered(), 
               aes(x = vei,
                   y = volcano_name,
                   fill = volcano_name)) +
            geom_density_ridges(size = 0.2) +
            labs(x = "Volcano Explosivity Index",
                 y = "") +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text = element_text(size = 11, 
                                           face = "bold"),
                  axis.title.x = element_text(size = 13, 
                                              face = "bold"),
                  axis.ticks = element_blank())
        
    })
    
}

shinyApp(ui = ui, server = server)
