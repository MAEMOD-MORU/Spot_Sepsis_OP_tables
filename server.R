#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    data<-reactive({
        if (is.null(input$datafile))
            return(NULL)                
        read.csv(input$datafile$datapath)
        
    })
    
    output$downloadCSV <- downloadHandler(filename = function() {paste(input$sitename,format(Sys.time(), " %A-%B-%d-%Y-%T"), ".csv",sep = "") }
                                          ,content = function(file) {write.csv(table_out(), file, row.names = FALSE)})
    
    
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
        
        # Local file system
        path_out = 'backup/'
        name = paste(input$sitename,format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".csv",sep = "")
        write.csv(out, paste(path_out,name,sep = ''), row.names = FALSE)

        
        out
    })
    
    
    output$table <- renderTable(table_out(),bordered =T)

})
