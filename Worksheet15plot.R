#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gapminder Data"),
    
    sidebarLayout(
      sidebarPanel(
        
    # Choose which continent
        checkboxGroupInput(inputId = "Continent",
                           label = "Choose which continent",
                           choices = levels(gapminder$continent),
                           selected = c("Asia")),

  

    # Sidebar with a slider input for years 
            sliderInput(inputId = "years",
                        label = "select years",
                        min = min(gapminder$year),
                        max = max(gapminder$year),
                        value = c(min(gapminder$year),max(gapminder$year))),

    
    # selecting country for comparison
        selectInput(
          inputId = "Country",
          label = "select country for comparison",
          choices = levels(gapminder$country),
          selected = "France")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
     

        p <- ggplot(
          gapminder, 
          aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)
        ) +
          geom_point(show.legend = TRUE, alpha = 0.7) +
          scale_color_viridis_d() +
          scale_size(range = c(2, 12)) +
          scale_x_log10() +
          labs(x = "GDP per capita", y = "Life expectancy")
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
