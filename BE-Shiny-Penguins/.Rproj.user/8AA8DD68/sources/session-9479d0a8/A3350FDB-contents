

library(shiny)
library(ggplot2)
library(palmerpenguins)
library(tidyverse)
library(bslib)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("Penguin Body Mass App"),
  h4("Use this app to investigate penguin body mass (g) in relation to flipper length (mm)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Flipper_Slider", "Select penguin flipper length range (mm)", min = 172, max = 231,
              value = c(190, 210))
    ),
    mainPanel(
      plotOutput("BodyMass"),
  tableOutput("BodyMassTable")
    )
  )
)


server <- function(input, output) {
  observe(print(input$Flipper_Slider))
  penguinsFiltered <- reactive({
    penguins %>%
      filter(
        flipper_length_mm < input$Flipper_Slider[2],
        flipper_length_mm > input$Flipper_Slider[1])
  })
  output$BodyMass <- renderPlot({
    penguinsFiltered() %>%
    ggplot(aes(body_mass_g)) +
      geom_histogram(fill = "darkorange") +
      xlab("Penguin Body Mass (g)") +
      theme_minimal()
  })
  output$BodyMassTable <- renderTable({
    penguinsFiltered() %>%
      summarise(Mean = mean(body_mass_g, na.rm = TRUE), 
      Min = min(body_mass_g, na.rm = TRUE), Max = max(body_mass_g, na.rm = TRUE))
  })
}


shinyApp(ui = ui, server = server)
