library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

penguins <- read.csv("C:\\Users\\2000h\\Documents\\Penguins-Shiny-App\\Penguins Shiny App\\penguins.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Penguins Shiny App", windowTitle = "Penguins Shiny App"), 
  sidebarLayout(
#This feature is allowing the user to filter the data by the Body Mass and the Species of the penguins. For the species, they can search for multiple entries simultaneously to look at data for any single or combination of penguin species.    
    sidebarPanel(sliderInput("body_mass_g", "Body Mass", min = 0, max = 7000, value = c(25, 40), pre = "g"),
                 checkboxGroupInput("species", "Species", choices = c("Adelie", "Chinstrap", "Gentoo"))
                 ), 
    mainPanel(
#This feature is creating separate tabs for the plot and the table, allowing the user to select how they would like to view the data. 
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")), 
#This feature is using the DT package to turn a static table into an interactive table. This allows the user to search for results within the table. 
        tabPanel("Table", DT::dataTableOutput("results"))
    )
)
)
)

server <- function(input, output) {
  output$plot <- renderPlot({
    filtered <- 
      penguins %>% 
      filter(body_mass_g >= input$body_mass_g[1],
             body_mass_g<= input$body_mass_g[2],
             species == input$species
             )
    ggplot(filtered, aes(body_mass_g)) + 
      geom_histogram()
  }
  )
  output$results <- DT::renderDataTable({
    filtered <- 
      penguins %>%
      filter(body_mass_g >= input$body_mass_g[1],
             body_mass_g <= input$body_mass_g[2],
             species == input$species
      )
    filtered
  })
}

shinyApp(ui = ui, server = server)


