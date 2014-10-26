library(shiny)

shinyUI(fluidPage(
        
        sidebarLayout(
                inputPanel(
                        selectInput("var", 
                                    label = "Choose a variable to display",
                                    choices = c("Percent White", "Percent Black",
                                                "Percent Hispanic", "Percent Asian"),
                                    selected = "Percent White"),
                        br(),
                        
                        sliderInput("range", 
                                    label = "Range of interest:",
                                    min = 0, max = 100, value = c(0, 100))
                ),
                
                # Show a tabset that includes a plot, and summary
                mainPanel(
                        titlePanel("2010 US Census Visualization"),
                        tabsetPanel(type = "tabs", 
                                    tabPanel("Summary", verbatimTextOutput("summary"),
                                             verbatimTextOutput("view")),
                                    tabPanel("Map", plotOutput("map"))
                        )
                )
        ),
        
        h5("Creates a Summary, and a Demographic Map using the 2010 US Census."),
        helpText("You can choose which Demographic to show, and what percent you're looking for."), br(),
        h5("Summary: "),
        helpText("This will show the Top 10 Counties for the selected Demographic, and a Five-number summary."),
        h5("Map: "),
        helpText("Based on the grouping, it will show a Heatmap of the Percentage in each Counties."), br(),
        helpText("Location of files: http://github.com/JJconde/Data_Product_Project")
        
        
))