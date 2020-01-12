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
library(randomForest)

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
            selectInput("algo1",
                        "Algorithm 1:",
                        choices = c("Linear Regression", "Random Forest Regression")),
            selectInput("algo2",
                        "Algorithm 2:",
                        choices = c("Linear Regression", "Random Forest Regression")),
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
           plotOutput("method_1"),
           plotOutput("method_2")
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
    
    observe({
        if(input$type=="Regression"){
            methods = c("Linear Regression", "Random Forest Regression")
        } else {
            methods = c("Logistic Regression", "Random Forest Classification")
        }
        updateSelectInput(session, "algo1",
                          label = "Select method",
                          choices = methods,
                          selected = methods[1]
        )
        updateSelectInput(session, "algo2",
                          label = "Select method",
                          choices = methods,
                          selected = methods[1]
        )
    })
    output$method_1 <- renderPlot({
        dat <- datasetInput()
        y_var <- input$dependent
        x_var <- input$independent
        formula_paste <-  paste(y_var, x_var, sep="~")
        if(input$algo1 == "Linear Regression"){
          source("lin_reg.R")
          lin_reg(formula = formula_paste, dat = dat, x_var = x_var, y_var = y_var)  
        }else if(input$algo1 == "Random Forest Regression"){
          source("random_forest.R")
          random_forest(formula = formula_paste, dat = dat, x_var = x_var, y_var = y_var)
        }
       })
    output$method_2 <- renderPlot({
      dat <- datasetInput()
      y_var <- input$dependent
      x_var <- input$independent
      formula_paste <-  paste(y_var, x_var, sep="~")
      if(input$algo2 == "Linear Regression"){
        source("lin_reg.R")
        lin_reg(formula = formula_paste, dat = dat, x_var = x_var, y_var = y_var)  
      }else if(input$algo2 == "Random Forest Regression"){
        source("random_forest.R")
        random_forest(formula = formula_paste, dat = dat, x_var = x_var, y_var = y_var)
      }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
