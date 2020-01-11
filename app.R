 #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(datasets)
theme_set(theme_classic())
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Machine Learning Algorithm Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("type",
                        "Type:",
                        c("Regression", "Classification")),
            selectInput("dataset",
                        "Dataset:",
                        choices = c("mtcars", "iris")),
            selectInput("algo",
                        "Algorithm:",
                        choices = c("Linear Regression", "KNN", "Random Forest Regression")),
            selectInput("dependent",
                        "Predictor:",
                        choices = "mpg"),
            selectInput("independent",
                        "Predictors:",
                        choices = "cyl")
            
        ),
        # mainPanel(
        #     verbatimTextOutput("summary")
        #     #tableOutput("view")
        # ),

        # Show a plot of the generated distribution
        splitLayout(
           plotOutput("method_1")
           # ,
           # plotOutput("method_2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    datasetInput <- reactive({
        switch(input$dataset,
               "mtcars" = mtcars,
               "iris" = iris)
    })
    variables <- reactive({
        dataset <- datasetInput()
        variables <- colnames(dataset)
    })
    observe({
        dataset <- datasetInput()
        variables <- colnames(dataset)
        # Can also set the label and select items
        updateSelectInput(session, "dependent",
                          label = "Select variable",
                          choices = variables,
                          selected = variables[1]
        )
        updateSelectInput(session, "independent",
                          label = "Select variable",
                          choices = variables,
                          selected = variables[2]
        )
    })
    
    
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    output$method_1 <- renderPlot({
        dat <- datasetInput()
        y_var <- input$dependent
        x_var <- input$independent
        formula = paste(y_var, x_var, sep="~")
        model <- lm(formula, data= dat)

        dat %>%
            ggplot() +
            geom_point(aes(x=dat %>% pull(x_var), y= dat %>% pull(y_var))) +
            geom_abline(intercept = model$coefficients[1], slope=model$coefficients[2]) +
            labs(x=input$independent, y= input$dependent) + 
            NULL
       })
    output$method_2 <- renderPlot({
        dat <- datasetInput()
        y_var <- input$dependent
        x_var <- input$independent
        formula = paste(y_var, x_var, sep="~")
        model <- lm(formula, data= dat)
        
        dat %>%
            ggplot() +
            geom_point(aes(x=dat %>% pull(x_var), y= dat %>% pull(y_var))) +
            geom_abline(intercept = model$coefficients[1], slope=model$coefficients[2]) +
            labs(x=input$independent, y= input$dependent) + 
            NULL
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
