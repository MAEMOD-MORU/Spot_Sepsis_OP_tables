#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(tinytex)
library(rmarkdown)
library(rdrop2)
library(readxl)
library(shinyjs)
library(dplyr)
library(flextable)
library(officer)
library(DT)
library(stringr)
library(shinybusy)
library(shinycssloaders)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    #disable download button
    shinyjs::useShinyjs()
    shinyjs::disable("downloadPDF")
    values <- reactiveValues(show = FALSE,
                             clinicalAreas = 1,
                             opdRecruitment = 0,
                             poolOfDays = "",
                             NumberRandomDays = 0)
    

    data<-reactive({
        if (is.null(input$datafile))
            return(NULL)
        if(input$datafile$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
            read_excel(input$datafile$datapath)
        }else{
            read.csv(input$datafile$datapath)
        }
        
    })
    
    #download CSV file
    output$downloadCSV <- downloadHandler(
        filename = function() {
            paste0(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".csv") 
            },
        content = function(file) {
            write.csv(table_out(), file, row.names = FALSE)
            }
        )
    
    #download DOCX file
    output$downloadDOC <- downloadHandler(
        filename = function() {
            paste0(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".docx")
        },
        
        content = function(file) {
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd')
            
            out <- render(input = 'report.Rmd',
                          output_format = word_document() 
            )
            file.rename(out, file)
        }
    )
    
    #download PDF file
    output$downloadPDF <- downloadHandler(
        filename = function() {
            paste0(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".pdf")
        },
        
        content = function(file) {
            show_modal_spinner()
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd')
            
            out <- render(input = 'report.Rmd',
                          output_format = pdf_document(latex_engine = "xelatex") 
            )
            remove_modal_spinner()
            file.rename(out, file)
        }
    )
    
    observe({
        show_modal_spinner()
        Sys.sleep(0.5)
        ######site name Input######
        if(input$sitename == "Bangladesh"){
            values$clinicalAreas <- 2
            values$opdRecruitment <- 3
            values$poolOfDays <- "Sun - Thu"
            values$NumberRandomDays <- 5
        }else if(input$sitename == "Cambodia"){
            values$clinicalAreas <- 3
            values$opdRecruitment <- 1
            values$poolOfDays <- "Mon - Fri"
            values$NumberRandomDays <- 3
        }else if(input$sitename == "Indonesia"){
            values$clinicalAreas <- 1
            values$opdRecruitment <- 2
            values$poolOfDays <- "Mon - Sat"
            values$NumberRandomDays <- 5
        }else if(input$sitename == "Laos - Salavan"){
            values$clinicalAreas <- 1
            values$opdRecruitment <- 1
            values$poolOfDays <- "Mon - Fri"
            values$NumberRandomDays <- 1
        }else if(input$sitename == "Laos - Savannakhet"){
            values$clinicalAreas <- 1
            values$opdRecruitment <- 1
            values$poolOfDays <- "Mon - Fri"
            values$NumberRandomDays <-1
        }
        
        values$show <- FALSE
        
        remove_modal_spinner()
    })
        
    
        table_out <- reactive({
        req (!is.null(input$datafile))

        data <- data()
        week=2 # Week ##
        cut_raw=numeric(week)
        cut=numeric(week)
        for (i in 1:2 )# Week #
        {
            di2=data[which(data[,2]==i),]
            cut_raw[i]=round((mean(di2[,5])))
            if (cut_raw[i]<=10) {
                cut[i]=round(0.5*cut_raw[i])
            } else {
                cut[i]=10
            }
        }
        nd=cut[2]
        ran_day=sample(seq(1,4,1),1,replace=F)
        ran_matrix=matrix(0,ran_day,nd)# No of working days for next week and nd= No-of patients ( no-of patients should be greater than 10)

        for (i in ran_day:ran_day) # no-of working days in the next week
        {


            ran_matrix[i,]= sort(sample(seq(1,cut_raw[week],1),nd,replace=F))
            ran_matrix[i,]=sapply(ran_matrix[i,],function(x)paste("OPD_DOC",x,sep="_"))
        }
        day=c("Monday", "Tuesday","Wednesday","Thursday","Friday")
        t(ran_matrix)
        out=cbind(rep(day[ran_day],nd),ran_matrix[ran_day,])

        colnames(out) <- c("Random Day","Patients")

        #enable download button
        shinyjs::useShinyjs()
        shinyjs::enable("downloadPDF")
        
#########       Local file system     #########
        # set your working directory
        # setwd("D:/Work/Spot_Sepsis_OP_tables")
        # path_out = 'backup/'
        # name = paste(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".csv",sep = "")
        # write.csv(out, paste(path_out,name,sep = ''), row.names = FALSE)
        
#########       Dropbox     #########
            # drop_auth()
            # drop_auth(new_user = T) #for renew Authentication
            # name = paste(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".csv",sep = "")
            # filePath <- file.path(tempdir(), name)
            # write.csv(out, filePath, row.names = FALSE, quote = TRUE)
            # drop_upload(filePath)

        
        data.frame(out)
        # data.frame(data())
    })
    
        opd_daily_patient_selection <- reactive({
            
            req (!is.null(input$datafile))
            #Define opd patient groups
            shinyjs::useShinyjs()
            shinyjs::enable("downloadPDF")
            values$show <- T
            opd_doc<-1:29
            tri_doc<-1:57
            tri_nurse<-1:1
            size_pat <- 10
            withProgress(message = 'Calculation in progress',{
            if(values$opdRecruitment == 1){
                day <- 5
                date <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
            }else if(values$opdRecruitment == 2){
                day <- 6
                date <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
            }else{
                day <- 5
                date <- c("Sunday","Monday","Tuesday","Wednesday","Thursday")
            }
            
            if(input$sitename == "Cambodia") size_pat <- 15
            
            #Apply vector-specific prefix to each item
            opd_doc<-sapply(opd_doc,function(x)paste("OPD_DOC",x,sep="_"))
            tri_doc<-sapply(tri_doc,function(x)paste("TRI_DOC",x,sep="_"))
            tri_nurse<-sapply(tri_nurse,function(x)paste("TRI_NUR",x,sep="_"))
            
            #Concatenate three opd groups into single vector
            all_opd<-c(opd_doc, tri_doc, tri_nurse)
            
            #Create function that samples 15 patients from concatenated vector without replacement
            patient_numbers<-function(all_opd){
                select_opd_patients<-sample(all_opd,size=size_pat,replace=F)
                select_opd_patients[str_order(select_opd_patients,numeric = T)]
                #x[str_order(x,  numeric = T)]
            }
            
            list_patient_numbers_group <- list()
            
            nonrecruitment<-1:day
            #create function to randomly select one value from sample
            #NB output of function (last line of code) must be a value

                select_opd_day<-sort(sample(nonrecruitment,size=values$NumberRandomDays))
            
            

            #Repeat function 3 times to get random selection of 15 patients for each day of the week
            for (i in 1:values$clinicalAreas) {
                patient_numbers_group <- replicate(values$NumberRandomDays,patient_numbers(all_opd))

                    # date <- date[!date %in% input$nonrecruitday]
                
                colnames(patient_numbers_group) <- date[select_opd_day]
                list_patient_numbers_group[[i]] <-patient_numbers_group
            }
            })
                list_patient_numbers_group

        })
        
        output$poolOfDay <- renderText(
            paste0("Pool Of Days : ", values$poolOfDays ) 
        ) 
        
        output$clinicalArea_text <- renderText(
            paste0("Number of Clinical Area : ", values$clinicalAreas ) 
        ) 
        
 
        
        output$Clinical_Area <- renderUI({
            req (!is.null(input$datafile))
            withProgress(message = 'Calculation in progress',{
            Clinical_Area_list <- NULL
            shinyjs::disable("downloadPDF")
            Clinical_Area_list <- lapply(1:values$clinicalAreas, function(i) {
                tableName <- paste("Clinical Area", i, sep="")
                outputName <- paste("t", i, sep="")
                shinyjs::enable("downloadPDF")
                output[[outputName]] <- renderTable(opd_daily_patient_selection()[[i]],bordered = T)
                tagList(
                    h2(tableName),
                    tableOutput(outputName)
                )
            })
            })
        })
    

})
