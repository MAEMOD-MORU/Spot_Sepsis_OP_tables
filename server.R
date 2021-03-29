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
library(aweek)
library(lubridate)
library(shinyjs)
library(dplyr)
library(flextable)
library(officer)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    #disable download button
    shinyjs::useShinyjs()
    shinyjs::disable("downloadCSV")
    shinyjs::disable("downloadDOC")
    shinyjs::disable("downloadPDF")
    
    

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
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd')
            
            out <- render(input = 'report.Rmd',
                          output_format = pdf_document(latex_engine = "xelatex") 
            )
            file.rename(out, file)
        }
    )
    
    observe({
        if(input$opdrecruitment == 1){
            updateCheckboxGroupInput(session, "nonrecruitday","Non recruitment Day :",
                                     c("Monday" = "Monday",
                                       "Tuesday" = "Tuesday",
                                       "Wednesday" = "Wednesday",
                                       "Thursday" = "Thursday"
                                     ))
            
        }else{
            updateCheckboxGroupInput(session, "nonrecruitday","Non recruitment Day :",
                                     c("Monday" = "Monday",
                                       "Tuesday" = "Tuesday",
                                       "Wednesday" = "Wednesday",
                                       "Thursday" = "Thursday",
                                       "Friday" = "Friday"
                                     ))
        }
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
        shinyjs::enable("downloadCSV")
        shinyjs::enable("downloadDOC")
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
            
            #Define opd patient groups
            opd_doc<-1:29
            tri_doc<-1:57
            tri_nurse<-1:1
            nonRecruitDay <- length(input$nonrecruitday)
            if(input$opdrecruitment == 1)  {
                day <- 4 - nonRecruitDay
                date <- c("Monday","Tuesday","Wednesday","Thursday")
                }
            else {
                day <- 5 - nonRecruitDay
                date <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
            }
            #break if day == 0 
            if(day == 0) return(NULL) 
            
            #Apply vector-specific prefix to each item
            opd_doc<-sapply(opd_doc,function(x)paste("OPD_DOC",x,sep="_"))
            tri_doc<-sapply(tri_doc,function(x)paste("TRI_DOC",x,sep="_"))
            tri_nurse<-sapply(tri_nurse,function(x)paste("TRI_NUR",x,sep="_"))
            
            #Concatenate three opd groups into single vector
            all_opd<-c(opd_doc, tri_doc, tri_nurse)
            
            #Create function that samples 15 patients from concatenated vector without replacement
            patient_numbers<-function(all_opd){
                select_opd_patients_15<-sample(all_opd,size=15,replace=F)
                select_opd_patients_15
            }
            
            
            #Repeat function 3 times to get random selection of 15 patients for each day of the week
            patient_numbers_group <- replicate(day,patient_numbers(all_opd))
            if(!is.null(input$nonrecruitday)){
                date <- date[!date %in% input$nonrecruitday]
            }
            colnames(patient_numbers_group) <- date
            
            data.frame(patient_numbers_group)
        })
        
        table_out2 <- reactive({
            req (!is.null(input$datafile))
            data <- na.omit(data())
            w <- week(Sys.Date()) #get weed
            fristWeedDay <- get_date(week= w+1) #start Monday
            
            shinyjs::useShinyjs()
            shinyjs::enable("downloadCSV")
            shinyjs::enable("downloadDOC")
            shinyjs::enable("downloadPDF")
            
            if(input$opdrecruitment == 1)  {
                day_num <- 4
                day <- c("Monday","Tuesday","Wednesday","Thursday")
                date <- c(as.Date(fristWeedDay),
                          as.Date(fristWeedDay+1),
                          as.Date(fristWeedDay+2),
                          as.Date(fristWeedDay+3))
                d <- "Monday - Thursday"
            }
            else {
                day_num <- 5
                day <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
                date <- c(as.Date(fristWeedDay),
                          as.Date(fristWeedDay+1),
                          as.Date(fristWeedDay+2),
                          as.Date(fristWeedDay+3),
                          as.Date(fristWeedDay+4)
                          )
                d <- "Monday - Friday"
            }
            
                clinic1 <- rpois(day_num,
                                 lambda = mean(
                                     tail(data$`Clinical Area 1`,5),na.rm = T
                                 )
                )
                
                clinic2 <- rpois(day_num,
                                 lambda = mean(
                                     tail(data$`Clinical Area 2`,5),na.rm = T
                                 )
                )
                
                clinic3 <- rpois(day_num,
                                 lambda = mean(
                                     tail(data$`Clinical Area 3`,5),na.rm = T
                                 )
                )
                
                table <- data.frame("OPD recruitment hours" = d,
                                    "OPD Day" = day,
                                    "OPD Date" = date,
                                    "Clinical Area 1" <- clinic1,
                                    "Clinical Area 2" <- clinic2,
                                    "Clinical Area 3" <- clinic3
                )
            
            colnames(table) <- c("OPD recruitment hours","OPD Day","OPD Date","Clinical Area 1","Clinical Area 2","Clinical Area 3")
            for (day in input$nonrecruitday) {
                
                table[table$`OPD Day` == day,]$`Clinical Area 1` <- 0
                table[table$`OPD Day` == day,]$`Clinical Area 2` <- 0
                table[table$`OPD Day` == day,]$`Clinical Area 3` <- 0
            }
            table
        })
        
    
    # output$table <- renderDT(table_out2())
    output$table <- renderDataTable(table_out2())
    output$table2 <- renderTable(opd_daily_patient_selection(),bordered = T)

})
