# Load the necessary libraries
library(shiny)
library(maps)
library(mapproj)

# Use the helper function
source("helpers.R")

# Load the data
counties <- readRDS("data/counties.rds")
# Remove NA's
counties <- counties[!is.na(counties$white), ]
# Change the significant figures to 3
counties$white <- signif(counties$white, digits=3)
counties$black <- signif(counties$black, digits=3)
counties$hispanic <- signif(counties$hispanic, digits=3)
counties$asian <- signif(counties$asian, digits=3)
# Change the name to upper case
counties$name <- toupper(counties$name)
# Format the name of the columns
names(counties) = c("Name", "Total Population", "White", "Black", "Hispanic", "Asian")

shinyServer(
        function(input, output) {
                
                # Generate the requested data
                data <- reactive({
                        args <- switch(input$var,
                                       "Percent White" = list(counties$White, "darkgreen", "% White"),
                                       "Percent Black" = list(counties$Black, "black", "% Black"),
                                       "Percent Hispanic" = list(counties$Hispanic, "darkorange", "% Hispanic"),
                                       "Percent Asian" = list(counties$Asian, "darkviolet", "% Asian"))
                        
                        args$min <- input$range[1]
                        args$max <- input$range[2]
                        
                        args
                })
                
                # Generate a map of the data
                output$map <- renderPlot({
                        do.call(percent_map, data()) # args)
                })
                
                # Generate a summary of the data
                output$summary <- renderPrint({
                        summary(data()[[1]])
                })
                
                # Generate a table of the data
                 output$view <- renderPrint({
                         head(counties[order(data()[[1]], decreasing=TRUE), ], 10)
                })
        }
)