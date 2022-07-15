#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            #Created a interactive button for generation of linear model
             actionButton("go", "Generate Linear Regression"),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
      
    model_In <- eventReactive(input$go, {
    lm(formula = y ~ x,
               data = dataInput())
  })
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        #plot(dataInput()$x,dataInput()$y)
        
        ggplot() +
  geom_point(aes(x = dataInput()$x, y = dataInput()$y),
             colour = 'blue') + 
           #Changed titles of x, y and Title of graph
        ggtitle('Data from Uploaded Files') + 
        xlab('x values') + 
        ylab('y values')
    })
    
    output$lmPlot <- renderPlot({
      #  plot(dataInput()$x,dataInput()$y)
        
        
        model_sum <- summary(model_In())
    # Creating variables for intercept,slope and R^2 & Outputing intercept,slope, and R^2 value
        slope_val <- model_sum$coefficients[1,1] 
        Label = paste("m =", slope_val) 
        
        intercept_val <- model_sum$coefficients[2,1] 
        Label2 = paste("y =", intercept_val)
        
        R_val <- model_sum$r.squared
        Label3 = paste("R^2 =", R_val)
        
ggplot() +
  geom_point(aes(x = dataInput()$x, y = dataInput()$y),
             colour = 'blue') +
  geom_line(aes(x = dataInput()$x, y = predict(model_In(), newdata = dataInput())),
            colour = 'red') +
  ggtitle('Linear regression') +
  xlab('x values') +
  ylab('y values') +
   
annotate("text", x = 17, y = 5, label = Label) +
annotate("text", x = 17, y = 4, label = Label2) +
annotate("text", x = 17, y = 3, label = Label3)
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)

