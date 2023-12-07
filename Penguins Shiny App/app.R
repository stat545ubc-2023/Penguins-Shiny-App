library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

penguins <- read.csv("penguins.csv", stringsAsFactors = FALSE)

ui <- fluidPage(

#This is setting the theme for the app 
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  br(),
  
#This is writing the title for the app 
  titlePanel("Penguins Shiny App", windowTitle = "Penguins Shiny App"), 
  br(),

#This is writing the informative welcome message
  p("Welcome to the Penguins Shiny App! Here you can explore data on 344 penguins, including 3 species across 3 islands in the Palmer Archipelago, Antarctica. Using the sliding bar for Body Weight (g) and the check boxes for Species, you can customize how you would like to see the penguins data in the graph and the table. You can also use the search function in the table to find anything that interests you."),
  br(),

  sidebarLayout(
    
#This feature is allowing the user to filter the data by the Body Mass and the Species of the penguins. For the species, they can search for multiple entries simultaneously to look at data for any single or combination of penguin species.    
    sidebarPanel(sliderInput("body_mass_g", "Body Mass", min = 0, max = 7000, value = c(2000, 5000), pre = "g"),
                 checkboxGroupInput("species", "Species", choices = c("Adelie", "Chinstrap", "Gentoo"), selected = "Adelie")), 
    
    mainPanel(
      
      
#This feature is creating separate tabs for the plot and the table, allowing the user to select how they would like to view the data. 
      tabsetPanel(
        tabPanel("Plot", br(), plotOutput("plot")), 
        
#This feature is using the DT package to turn a static table into an interactive table. This allows the user to search for results within the table. 
        tabPanel("Table", br(), DT::dataTableOutput("results"))))))

server <- function(input, output) {
  
#This is creating a reactive variable to reduce code duplication
  filtered <- reactive({
    penguins %>% 
      filter(body_mass_g >= input$body_mass_g[1],
             body_mass_g<= input$body_mass_g[2],
             species == input$species)})
  
#This is creating a histogram of the distribution of body mass which can be filtered by the body mass of the penguins, and the species of penguin
  output$plot <- renderPlot({
    ggplot(filtered(), aes(body_mass_g)) + 
      geom_histogram(mapping = aes(fill = species)) +
      theme_bw() +
      labs(x = "Penguin Body Mass (g)", y = "# of Individuals", title = "Penguin Body Mass in the Palmer Archipelago, Antarctica") +
      theme(title = element_text(size = 16, face = "bold")) +
      scale_fill_brewer(palette = "Blues")})
  
#This is creating a table of the penguins data, which can also be filtered by body mass and species, and is interactive and has a search function for the users 
  output$results <- DT::renderDataTable({
    filtered()})}

shinyApp(ui = ui, server = server)


