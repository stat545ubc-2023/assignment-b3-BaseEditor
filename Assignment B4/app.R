

library(shiny)
library(ggplot2)
library(palmerpenguins)
library(tidyverse)
library(bslib)


# UI for application that draws two histograms and two tables
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("Penguin Body Mass App"),
  h4("Use this app to investigate penguin body mass (g) and bill length (mm) in relation to flipper length
  (mm), across three different species"),
  sidebarLayout(
    sidebarPanel(
      img(src = "palmerpenguins.png", style="display: block; margin-left: auto; margin-right: auto;",
          height = 200, width = 200),
      sliderInput("Flipper_Slider", "Select penguin flipper length range (mm)", min = 172, max = 231,
              value = c(190, 210)),
      checkboxGroupInput(inputId = "Species_Select", "Select penguin species", 
                         choices = levels(factor(penguins$species)),
                         selected = levels(factor(penguins$species))),
      img(src = "penguinspecies.png", style="display: block; margin-left: auto; margin-right: auto;",
          height = 210, width = 350),
      img(src = "culmen_depth.png", style="display: block; margin-left: auto; margin-right: auto;",
          height = 224, width = 350)
      ),
    mainPanel(
      plotOutput("BodyMass"),
      tableOutput("BodyMassTable"),
      plotOutput("BillLength"),
      tableOutput("BillLengthTable")
    )
  ),
  h6("Artwork by @allison_horst")
)

# Code for filtering slider and selection check boxes, two histograms, and two tables
server <- function(input, output) 
  {
  observe(print(input$Flipper_Slider))
  observe(print(input$Species_Select))
  
#Filter by species
  penguinsSpecies <- reactive({
    penguins %>%
      dplyr::filter(
        species %in% input$Species_Select
      )
  })
  
#Filter by flipper length
  penguinsFiltered <- reactive({
    penguinsSpecies() %>%
      filter(
        flipper_length_mm < input$Flipper_Slider[2],
        flipper_length_mm > input$Flipper_Slider[1]
        )
  })
  

#Body mass histogram
  output$BodyMass <- renderPlot({
    penguinsFiltered() %>%
    ggplot(aes(body_mass_g)) +
      geom_histogram(aes(fill = species)) +
      ggtitle("Body Mass Distribution") +
      xlab("Penguin Body Mass (g)") +
      theme_minimal()
  })

#Body mass table
  output$BodyMassTable <- renderTable({
    penguinsFiltered() %>%
      summarise(Mean = mean(body_mass_g, na.rm = TRUE), 
      Min = min(body_mass_g, na.rm = TRUE), Max = max(body_mass_g, na.rm = TRUE))
  })
  
#Bill length histogram
  output$BillLength <- renderPlot({
    penguinsFiltered() %>%
      ggplot(aes(bill_length_mm)) +
      geom_histogram(aes(fill = species)) +
      ggtitle("Bill Length Distribution") +
      xlab("Penguin Bill Length (mm)") +
      theme_minimal()
  })
  
#Bill length table
  output$BillLengthTable <- renderTable({
    penguinsFiltered() %>%
      summarise(Mean = mean(bill_length_mm, na.rm = TRUE), 
                Min = min(bill_length_mm, na.rm = TRUE), Max = max(bill_length_mm, na.rm = TRUE))
  })
}


shinyApp(ui = ui, server = server)
