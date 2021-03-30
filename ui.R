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
library(dplyr)
library(shinyjs)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Spot Sepsis random number generator for outpatient screening"),
    
    
    # Show a plot of the generated distribution
    sidebarPanel(width = 4,
        shinyjs::useShinyjs(),
        selectInput("sitename", "Sitename : ",
                    c("Bangladesh" = "Bangladesh",
                      "Cambodia" = "Cambodia",
                      "Indonesia" = "Indonesia",
                      "Laos - Salavan" = "Laos - Salavan",
                      "Laos - Savannakhet" = "Laos - Savannakhet")
        ),
        selectInput("opdrecruitment", "OPD recruitment:",
                    c("Mon - Thu" = 1,
                      "Mon - Fri" = 2,
                      "Mon - Sat" = 3,
                      "Sun - Thu" = 4)
        ),
        checkboxGroupInput("nonrecruitday", "Non recruitment Day :",
                           c("Monday" = "Monday",
                             "Tuesday" = "Tuesday",
                             "Wednesday" = "Wednesday",
                             "Thursday" = "Thursday"
                           ),
                           
        ),
        fileInput(
            'datafile',
            h4('Upload File'),
            accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv',
                '.xlsx'
            )
        ),
        downloadButton('downloadPDF', 'Download as .pdf'),
    ),
        mainPanel(width = 8,
            h2("Output"),
            dataTableOutput('table'),
            conditionalPanel("output.show",h2("OPD Daily Patient Selection")),

            # tableOutput('table2')
            uiOutput("Clinical_Area")
        )

   
    
)

)
