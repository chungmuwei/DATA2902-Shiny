library(shiny)
library(tidyverse)

fdata = readxl::read_excel("ardd_fatalities_jun2022.xlsx", 
                           sheet = "BITRE_Fatality", 
                           skip = 4, 
                           na = c("","-9"), 
                           guess_max = 1e6) %>% 
  janitor::clean_names() |> 
  mutate(
    month_name = factor(month.abb[month], levels = month.abb)
  )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fatalities app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "year",
                           label = "Select year",
                           choices = 1989:2022,
                           selected = 2021,
                           multiple = TRUE
                           )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("chisq_test")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
      final_data() |> 
        ggplot() + 
        aes(x = month_name, fill = factor(year)) + 
        geom_bar(position = "fill")
      
    })
    
    final_data = reactive({
      fdata |> 
        dplyr::filter(year %in% input$year)
    })
    
    output$chisq_test = renderPrint({
      
      obs_counts = final_data() |> 
        pull(month_name) |> 
        table()
      
      chisq.test(obs_counts)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
