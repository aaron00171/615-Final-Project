library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(shinyBS)
thedata <- read.csv('AMZN.csv')
thedata$Date <- as.Date(thedata$Date,format = "%Y/%m/%d")

the_origin_result <- data.frame(Trading_date=as.Date("2020-06-30"),
                               Current_Portfolio_value =0,
                               current_Spare_Cash=250000,
                               Current_Portfolio_Profit=0)
theresult <<- the_origin_result

# Apps can be run without a server.r and ui.r file
ui = fluidPage(
  column(6,
         column(12,uiOutput("date_input")),
         column(3,uiOutput('buy_UI')),
         column(3,uiOutput('Sell_UI')),
         column(3,h1(""),actionButton("Apply_label","Apply")),
         column(3,h1(""),actionButton("Clear_label","Clear")),
         dataTableOutput("result")),
  column(6,plotOutput("plot1"))

)


server = function(input, output,session) {
  
  observeEvent(input$Clear_label,
               theresult <<- the_origin_result       
  )
  
  output$date_input <- renderUI({
    input$Apply_label
    selectInput("Date1","Select the date",choices = unique(thedata$Date)[unique(thedata$Date)>max(resultdata()$Trading_date)])
    input$Clear_label
    selectInput("Date1","Select the date",choices = unique(thedata$Date)[unique(thedata$Date)>max(theresult$Trading_date)])
    
  })
  
  output$buy_UI <- renderUI({
    numericInput("buy_num","Buy in($)",value = 0,min = 0,
                 max = theresult$current_Spare_Cash[dim(theresult)[1]])
  })
  
  output$Sell_UI <- renderUI({
    numericInput("sell_num","Sell($)",value = 0,min = 0,
                 max = theresult$Current_Portfolio_value[dim(theresult)[1]])
  })
  
  resultdata <- reactive({
    input$Apply_label
    if (dim(theresult)[1]==1) {
      tempdata <- isolate(data.frame(
        Trading_date = isolate(input$Date1),
        Current_Portfolio_value = isolate(input$buy_num) - isolate(input$sell_num),
        current_Spare_Cash =  theresult$current_Spare_Cash[1] - isolate(input$buy_num),
        Current_Portfolio_Profit = isolate(input$sell_num)
      ))
    }else{
      pre_close <- thedata[thedata$Date==theresult$Trading_date[dim(theresult)[1]],]$Adj.Close
      tempdata <- isolate(data.frame(
        Trading_date = isolate(input$Date1),
        Current_Portfolio_value = 
          theresult$Current_Portfolio_value[dim(theresult)[1]]/pre_close*thedata[thedata$Date==isolate(input$Date1),]$Adj.Close + 
          isolate(input$buy_num) - isolate(input$sell_num) ,
        current_Spare_Cash = theresult$current_Spare_Cash[dim(theresult)[1]]- 
          isolate(input$buy_num) + isolate(input$sell_num),
        Current_Portfolio_Profit = theresult$Current_Portfolio_value[dim(theresult)[1]]/pre_close*thedata[thedata$Date==isolate(input$Date1),]$Adj.Close+
          theresult$current_Spare_Cash[dim(theresult)[1]]- 250000
      ))  
    }
    theresult <<- rbind(theresult,isolate(tempdata))
    theresult
    })
  
  output$result <- renderDataTable({
    resultdata()
  })
  
  output$plot1 <- renderPlot({
    input$Apply_label
    plotdata <- subset(thedata,thedata$Date <= isolate(input$Date1))
    ggplot(plotdata,aes(Date,Adj.Close))+geom_line()
  })
  
  
}

app <- shinyApp(ui,server)
