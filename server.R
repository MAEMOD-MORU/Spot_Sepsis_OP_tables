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
library(aweek)
library(summarytools)
library(lubridate)
options(lubridate.week.start = 7)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  set_week_start("Sunday")
  
    #use loading screen
    show_modal_spinner()
    #wait of 1 sec
    Sys.sleep(2)
    #stop loading screen
    remove_modal_spinner()
    #disable download button
    shinyjs::useShinyjs()
    shinyjs::disable("downloadPDF")
    
    #values have changing 
    values <- reactiveValues(show = FALSE,
                             clinicalAreas = 1,
                             opdRecruitment = 0,
                             poolOfDays = "",
                             NumberRandomDays = 0,
                             previousWeek= NULL)
    
    #input file 
    data<-reactive({
        #if input file don't upload return NULL
        if (is.null(input$datafile))
            return(NULL)
        #use loading screen
        show_modal_spinner()
        Sys.sleep(1)
        x <- unlist(str_split(input$datafile$name,"\\."))
        #if file is .xls/.xlsx use read excel ,else(.csv) use read csv
        if(tail(x,1)=="csv"){
            x <- read.csv(input$datafile$datapath) 
            print(x)
            x$OPD.Date <- as.POSIXct(as.character(as.factor(x$OPD.Date)),format="%m/%d/%Y")
            colnames(x) <- c("OPD recruitment hours","OPD Day","OPD Date","Clinical Area 1","Clinical Area 2","Clinical Area 3")
        }
        else{
            x <-read_excel(input$datafile$datapath)
            colnames(x) <- c("OPD recruitment hours","OPD Day","OPD Date","Clinical Area 1","Clinical Area 2","Clinical Area 3")
        }
        #stop loading screen
        remove_modal_spinner()
        return(x)
    })
    
    #download PDF file
    output$downloadPDF <- downloadHandler(
        
        #set name of download file by using input of site name , current time ,in format day-month(short text)-year and time hour.minute.second(Ex. 03-Mar-2021 16.01.00),
        #and file name .pdf
        filename = function() {
            paste0(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".pdf")
        },
        #content in pdf file look more details in report.Rmd
        content = function(file) {
            #use loading screen
            show_modal_spinner()
            #Use template form report.Rmd
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd')
            
            #render pdf doc by using report.Rmd template and latex engine is xelatex
            out <- render(input = 'report.Rmd',
                          output_format = pdf_document(latex_engine = "xelatex") 
            )
            #stop loading screen
            remove_modal_spinner()
            #rename file
            file.rename(out, file)
        }
    )
    
    observe({
        #use loading screen
        show_modal_spinner()
        #wait of 1 sec
        Sys.sleep(1)
        ######site name Input######
        #change values of clinicalAreas , opdRecruitment , poolOfDays and NumberRandomDays
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
            values$opdRecruitment <- 4
            values$poolOfDays <- "Mon - Thu"
            values$NumberRandomDays <- 1
        }else if(input$sitename == "Laos - Savannakhet"){
            values$clinicalAreas <- 1
            values$opdRecruitment <- 4
            values$poolOfDays <- "Mon - Thu"
            values$NumberRandomDays <-1
        }else if(input$sitename == "Vietnam"){
            values$clinicalAreas <- 2
            values$opdRecruitment <- 1
            values$poolOfDays <- "Mon - Fri"
            values$NumberRandomDays <-5
        }
        
        #stop loading screen
        remove_modal_spinner()
    })
        
 
    
        opd_daily_patient_selection <- reactive({
            
            req (!is.null(input$datafile))
            show_modal_spinner()
            Sys.sleep(5)
            #change to use data form easy then to use data() 
            data <- na.omit(data())
            values$show <- T
            size_pat <- 10
            # withProgress(message = 'Calculation in progress',{
            if(values$opdRecruitment == 1){
                day <- 5
                date <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
            }else if(values$opdRecruitment == 2){
                day <- 6
                date <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
            }else if(values$opdRecruitment == 4){
              day <- 4
              date <- c("Monday","Tuesday","Wednesday","Thursday")
            }else{
                day <- 5
                date <- c("Sunday","Monday","Tuesday","Wednesday","Thursday")
            }
  
            w <- (tail(data$`OPD Date`,1))%>% floor_date(unit='week') %>% epiweek()
            w <- strtoi(w)
            w <- paste0(w)

            values$previousWeek<- data %>% filter(as.aweek(data$`OPD Date`) %>% epiweek()  == w)

                
                clinic1 <- floor(mean(values$previousWeek$`Clinical Area 1`)/2)
                clinic2 <- floor(mean(values$previousWeek$`Clinical Area 2`)/2)
                clinic3 <- floor(mean(values$previousWeek$`Clinical Area 3`)/2)
            #Define opd patient groups
            #Apply vector-specific prefix to each item
            #if the numbers in a particular week are low and there are 20 or fewer patients to select from, 
            #then the patients should not be randomized and patients can be screened consecutively 
                if(input$sitename == "Cambodia"){
                    total_patient <- clinic1+clinic2+clinic3
                    if(total_patient > 15){
                        opd_doc<-1:clinic1
                        tri_doc <- 1:clinic2
                        tri_nurse <- 1:clinic3
                    }else{
                        area_ratio_patient <- round(c(clinic1,clinic2,clinic3)/total_patient*15)
                        if(sum(area_ratio_patient) > 15){
                            order_max <- which.max(area_ratio_patient)
                            area_ratio_patient[order_max] <- area_ratio_patient[order_max]-1
                        }
                        else if(sum(area_ratio_patient) < 15){
                            order_min <- which.min(area_ratio_patient)
                            area_ratio_patient[order_min] <- area_ratio_patient[order_min]+1
                        }
                        opd_doc <- 1:area_ratio_patient[1]
                        tri_doc <- 1:area_ratio_patient[2]
                        tri_nurse <- 1:area_ratio_patient[3]
                        
                    }
                }else if(input$sitename == "Vietnam"){
                  if(clinic1 > 15) opd_doc<-1:clinic1
                  else opd_doc <- 1:15
                  tri_doc <- 1:15
                  tri_nurse <- 1:15
                }else{
                    if(clinic1 > 10) opd_doc<-1:clinic1
                    else opd_doc <- 1:10
                    if(clinic2 > 10) tri_doc<-1:clinic2
                    else tri_doc <- 1:10
                    if(clinic3 > 10) tri_nurse<-1:clinic3
                    else tri_nurse <- 1:10
                }

            opd_doc<-sapply(opd_doc,function(x)paste("CLINAREA_A",x,sep="_"))
            tri_doc<-sapply(tri_doc,function(x)paste("CLINAREA_B",x,sep="_"))
            tri_nurse<-sapply(tri_nurse,function(x)paste("CLINAREA_C",x,sep="_"))
            all_opd<- opd_doc
            #Concatenate three opd groups into single vector
            if(clinic1 == 0){
                opd_doc <- NULL
            }
            if(clinic2 == 0){
                tri_doc <- NULL
            }
            if(clinic3 == 0){
                tri_nurse <- NULL
            }
            
            
            if(input$sitename == "Cambodia") {
                size_pat <- 15
                all_opd<- c(opd_doc,tri_doc,tri_nurse)
            }else if(input$sitename == "Indonesia") {
                all_opd <- opd_doc
            }else if(input$sitename == "Laos - Salavan") {
                all_opd <- opd_doc
            }else if(input$sitename == "Vietnam") {
              size_pat <- 15
              all_opd <- opd_doc
            }else{
                all_opd <- opd_doc
            }
            
           
            
            #Create function that samples 15 patients from concatenated vector without replacement
            patient_numbers<-function(all_opd){

                select_opd_patients<-sample(all_opd,size=size_pat,replace=F)
                select_opd_patients[str_order(select_opd_patients,numeric = T)]
                #x[str_order(x,  numeric = T)]
            }
            
            # list_patient_numbers_group <- list()
            
            nonrecruitment<-1:day
            #create function to randomly select one value from sample
            #NB output of function (last line of code) must be a value

            select_opd_day<-sort(sample(nonrecruitment,size=values$NumberRandomDays))
            
            

            #Repeat function 3 times to get random selection of 15 patients for each day of the week
            if(input$sitename == "Bangladesh"){
                randomNum1 <-0
                randomNum2 <-0
                if(clinic1 <= 10 || clinic2 <= 10)  {
                    if(clinic1 <= 10) randomNum1 <- 1:10
                    else randomNum1 <- 1:length(opd_doc)
                    if ( clinic2 <= 10) randomNum2 <- 1:10
                    else randomNum1 <- 1:length(opd_doc)

                    }
                else {
                    randomNum1 <- 1:length(opd_doc)
                    randomNum2 <- 1:length(tri_doc)
                }
                
                ca1 <- sum(values$previousWeek$`Clinical Area 1`)/2
                ca2 <- sum(values$previousWeek$`Clinical Area 2`)/2
                caAll <- ca1 + ca2
                opd_doc_loop_num <- round((ca1/caAll)*5)

                if(ca1 == ca2) opd_doc_loop_num <- opd_doc_loop_num+round(runif(1))
                tri_doc_loop_num <- values$NumberRandomDays - opd_doc_loop_num
                
                
                

                opd_doc <- sapply(randomNum1,function(x)paste("CLINAREA_A",x,sep="_"))
                all_opd <- opd_doc
                patient_numbers1 <- replicate(opd_doc_loop_num,patient_numbers(all_opd))
                

                tri_doc <- sapply(randomNum2,function(x)paste("CLINAREA_B",x,sep="_"))
                all_opd <- tri_doc
                patient_numbers2 <- replicate(tri_doc_loop_num,patient_numbers(all_opd))
                
                patient_numbers_group <- cbind(patient_numbers1,patient_numbers2)
                #x[,sample(ncol(x), 6)]
                patient_numbers_group <- patient_numbers_group[,sample(ncol(patient_numbers_group))]
                
            }else{
                patient_numbers_group <- replicate(values$NumberRandomDays,patient_numbers(all_opd))
            }
                    # date <- date[!date %in% input$nonrecruitday]
                
                colnames(patient_numbers_group) <- date[select_opd_day]
                # list_patient_numbers_group[[i]] <-patient_numbers_group
            
            # })
            shinyjs::useShinyjs()
            shinyjs::enable("downloadPDF")
            remove_modal_spinner()
            patient_numbers_group

        })
        
        output$poolOfDay <- renderText(
            paste0("Pool Of Days : ", values$poolOfDays ) 
        ) 
        
        output$clinicalArea_text <- renderText(
            paste0("Number of Clinical Area : ", values$clinicalAreas ) 
        ) 
        
        
        output$show <-reactive({ return(values$show) })
        
        #Send output$show to ui.R
        outputOptions(output,"show",suspendWhenHidden =F)
        
        output$previousWeek <-  renderDataTable({
            req (!is.null(input$datafile))
            req(!is.null(values$previousWeek))
            x <- values$previousWeek
            x$`OPD Date` <- format(x$`OPD Date`, " %d-%b-%Y")
            ca <- values$clinicalAreas
            col <- 3+ ca
            x[,1:col]
        })

        
        output$Clinical_Area <- renderTable({
            req (!is.null(input$datafile))
            #use loading screen
            show_modal_spinner()
            ###drop box
            # drop_auth(rdstoken = "D:/Work/Spot_Sepsis_OP_tables/tokenfile.RDS")
            # path <- paste(getwd(),"report.Rmd",sep = "/")
            # render(input = path,
            #               output_format = pdf_document(latex_engine = "xelatex")
            # )
            # name <-  paste0(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S UTC%Z"), ".pdf")
            # file.rename("report.pdf", name)
            # path <- paste(getwd(),name,sep = "/")
            # 
            # drop_upload(path, path = "drop_test")
            #stop loading screen
            remove_modal_spinner()
            opd_daily_patient_selection()
            },
            bordered = T)
        

    

})
