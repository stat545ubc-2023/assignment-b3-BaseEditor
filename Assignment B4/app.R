

library(shiny)
library(ggplot2)
library(palmerpenguins)
library(tidyverse)
library(bslib)


# UI for application that draws a histogram and table
# Artwork was added for assignment b4 in order to improve the aesthetics of the page, as well as give a visual on the
#different species and what the bill measurements refer to
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("Palmer Penguins Analysis"),
  h4("Use this app to investigate physical penguin measurements in relation to flipper length
  (mm), across three different species"),
  sidebarLayout(
    sidebarPanel(
      img(src = "palmerpenguins.png", style="display: block; margin-left: auto; margin-right: auto;",
          height = 200, width = 200),
      selectInput("variable", "Graphed Variable",
                  c("Body mass (g)" = "body_mass_g",
                    "Bill length (mm)" = "bill_length_mm",
                    "Bill Depth (mm)" = "bill_depth_mm"
                  )
        
      ),
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
      h6("Body Mass (g) Summary Statistic Table"),
      tableOutput("BodyMassTable"),
    )
  ),
  h6("Artwork by @allison_horst")
)

# Code for variable selection, filtering slider, and selection check boxes, histogram, and table
server <- function(input, output) 
  {
#A variable selection tool was added for assignment B4, to allow the user to specify which variable they
#would like plotted on the histogram
  observe(print(input$variable))
  observe(print(input$Flipper_Slider))
  observe(print(input$Species_Select))
  
#Filter by species
#The feature of filtering by species, to allow users to select which species they would like to plot data
#from has been added for assignment B4
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
  

#Histogram
  output$BodyMass <- renderPlot({
    penguinsFiltered() %>%
    ggplot(aes_string(input$variable)) +
      geom_histogram(aes(fill = species)) +
      ggtitle("Distribution of Selected Variable") +
      xlab("Selected Variable") +
      theme_minimal()
  })

#Body Mass Table
  output$BodyMassTable <- renderTable({
    penguinsFiltered() %>%
      summarise(Mean = mean(body_mass_g, na.rm = TRUE), 
      Min = min(body_mass_g, na.rm = TRUE), Max = max(body_mass_g, na.rm = TRUE))
  })
}


shinyApp(ui = ui, server = server)
