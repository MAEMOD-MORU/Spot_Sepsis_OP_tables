#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#   Install TinyTeX
#   install.packages('tinytex')
#   tinytex::install_tinytex()
#   https://yihui.org/tinytex/
#
# setwd("D:/Work/Spot_Sepsis_OP_tables")
# develop shinyapp by tanaphum wichaita

library(shiny)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(dplyr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Spot Sepsis random number generator for outpatient screening"),
    
    
    # Show a plot of the generated distribution
    mainPanel(
        textInput("sitename", "Sitename", "Name"),
        fileInput("datafile", "Choose CSV File",
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
        ),
        downloadButton('downloadCSV', 'Download as .csv'),
        downloadButton('downloadPDF', 'Download as .pdf'),
        h2("Output"),
        tableOutput('table')
    )
    
)

)
