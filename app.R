library(shiny)
library(plotly)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MTCars Dataset Exploratory Graph"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("ft",
                        "Facet:",
                        c("Number of Cylinders",
                          "Transmission",
                          "Number of Gears"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- mtcars
    df$gear <- as.factor(df$gear)
    df$cyl <- as.factor(df$cyl)
    df$am <- as.factor(df$am)
    df$am <- recode(df$am, "0" = "Automatic", "1" = "Manual")
    
    output$plot <- renderPlotly({
        if( input$ft == "Number of Cylinders") {
            ggplotly(
            ggplot(df, aes(x = hp, y = mpg)) + 
                geom_point() + 
                facet_wrap(~cyl, nrow = 1, ncol = 3) +
                geom_smooth(method = "lm", se = F) +
                ggtitle("MPG vs Horsepower by Number of Cylinders") +
                theme(plot.title = element_text(hjust = 0.5)) +
                xlab("Horsepower") + 
                ylab("Miles per Gallon"))
        }
        else if (input$ft == "Transmission") {
            ggplotly(
            ggplot(df, aes(x = hp, y = mpg)) + 
                geom_point() + 
                facet_wrap(~am, nrow = 1, ncol = 2) +
                geom_smooth(method = "lm", se = F) +
                ggtitle("MPG vs Horsepower by Type Of Transmission") +
                theme(plot.title = element_text(hjust = 0.5)) +
                xlab("Horsepower") + 
                ylab("Miles per Gallon"))
        }
        else if (input$ft == "Number of Gears") {
            ggplotly(
            ggplot(df, aes(x = hp, y = mpg)) + 
                geom_point() + 
                facet_wrap(~gear, nrow = 1, ncol = 3) +
                geom_smooth(method = "lm", se = F) +
                ggtitle("MPG vs Horsepower by Number of Gears") +
                theme(plot.title = element_text(hjust = 0.5)) +
                xlab("Horsepower") + 
                ylab("Miles per Gallon"))
            
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
