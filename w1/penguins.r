# install.packages("shiny")
library(shiny)
library(palmerpenguins)
names(penguins)
library(tidyverse)
continuous_vars = c("bill_length_mm",
                    "bill_depth_mm",
                    "flipper_length_mm",
                    "body_mass_g")
categorical_vars = c("species",
                     "island"  ,
                     "sex")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Penguins plot"),





    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "var_xaxis",
                        label = "X axis variable",
                        choices = continuous_vars),
            selectInput(inputId = "var_yaxis",
                        label = "Y axis variable",
                        choices = continuous_vars),
            selectInput(inputId = "var_col",
                        label = "Colouring variable",
                        choices = categorical_vars),
            selectInput(inputId = "var_facet",
                        label = "Faceting variable",
                        choices = categorical_vars)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        penguins %>%
            ggplot()+
            aes(x = .data[[input$var_xaxis]],
                y = .data[[input$var_yaxis]],
                colour = .data[[input$var_col]]) +
            geom_point() +
            facet_grid (cols = vars(.data[[input$var_facet]]))

    })
}

# Run the application
shinyApp(ui = ui, server = server)
