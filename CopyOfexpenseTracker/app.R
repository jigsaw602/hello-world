library(data.table)
library(shiny)
library(DT)
library(highcharter)
library(tidyverse)
source("exp.r")
library(shinydashboard)

ui <- dashboardPage(skin = "green",
  dashboardHeader(title  = "Personal Expense Report"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Timeline", tabName = "timeLine", icon = icon("clock")),
      menuItem("Account-Wise", tabName = "accounts", icon = icon("money-check")),
      menuItem("Month-Wise", tabName = "month", icon = icon("calendar")),
      menuItem("Transactions", tabName = "trans", icon = icon("list"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow()),
      tabItem(tabName = "trans",
              fluidRow(
                column(4, selectInput("metricInputTrans", "Metric", choices = c("Category", "Luxurious", "Account"))),
                column(4, conditionalPanel("input.metricInputTrans == 'Category'",
                                           selectInput("catInputTrans", "Categories", choices = unique(transactions_dt$Category), multiple = TRUE, selected = "Household")),
                       conditionalPanel("input.metricInputTrans == 'Luxurious'",
                                        selectInput("luxInputTrans", "Luxurious/Essential", choices = unique(transactions_dt$Luxurious)[unique(transactions_dt$Luxurious) != "NA"], multiple = TRUE, selected = "Essential")),
                       conditionalPanel("input.metricInputTrans == 'Account'",
                                        selectInput("accInputTrans", "Account", choices = unique(transactions_dt$Account), multiple = TRUE, selected = "Paytm"))),
                column(4, selectInput("monthInputTrans","Month(s)", choices = unique(transactions_dt$MonthYear), multiple = TRUE, selected = "202005"))
              ),
              fluidRow(
                dataTableOutput("trans_data")
              )),
      tabItem(tabName = "timeLine",
              fluidRow(
                column(3, selectInput("metricInput1", "Metric", choices = c("Category", "Luxurious", "Account"))),
                column(3, conditionalPanel("input.metricInput1 == 'Category'",
                                           selectInput("catInput", "Categories", choices = unique(transactions_dt$Category), multiple = TRUE, selected = "Household")),
                 conditionalPanel("input.metricInput1 == 'Luxurious'",
                                           selectInput("luxInput", "Luxurious/Essential", choices = unique(transactions_dt$Luxurious)[unique(transactions_dt$Luxurious) != "NA"], multiple = TRUE, selected = "Essential")),
                 conditionalPanel("input.metricInput1 == 'Account'",
                                            selectInput("accInput", "Account", choices = unique(transactions_dt$Account), multiple = TRUE, selected = "Paytm"))),
                column(3, selectInput("plotTypeInput","Plot Type", choices = c("spline", "line", "column")))
              ),
              fluidRow(
                highchartOutput("linePlot"),
                dataTableOutput("data_tl")
              )),
      tabItem(tabName = "accounts",
              fluidRow(
                selectInput("accountInput", "Account", choices = unique(transactions_dt$Account))
              ),
              fluidRow(
                highchartOutput("accPlot"),
                dataTableOutput("accData")
              )),
      tabItem(tabName = "month",
              fluidRow(
                column(3,box(selectInput("monthInput", "Month", choices = unique(transactions_dt$MonthYear)), selectInput("metricInput2", "Metric", choices = c("Category", "Luxurious", "Account")))),
                
                
              ),
              fluidRow(
                highchartOutput("pieplot"),
                dataTableOutput("data_mw")
              ))
    )
   
  )
)

server = function(session, input, output){
  output$trans_data = renderDataTable({
    if(input$metricInputTrans == "Category"){
      transaction_list(input$metricInputTrans,input$catInputTrans,input$monthInputTrans)
    }
    else if(input$metricInputTrans == "Luxurious"){
      transaction_list(input$metricInputTrans,input$luxInputTrans,input$monthInputTrans)
    }
    else{
      transaction_list(input$metricInputTrans,input$accInputTrans,input$monthInputTrans)
    }
  })
  
  output$linePlot = renderHighchart({
    if(input$metricInput1 == "Category"){
      timeline(input$metricInput1, input$catInput, input$plotTypeInput)$plot
    }
    else if(input$metricInput1 == "Luxurious"){
      timeline(input$metricInput1, input$luxInput, input$plotTypeInput)$plot
    }
    else{
      timeline(input$metricInput1, input$accInput, input$plotTypeInput)$plot
    }
    })
  
  output$data_tl = renderDataTable({
    if(input$metricInput1 == "Category"){
      timeline(input$metricInput1, input$catInput, input$plotTypeInput)$data
    }
    else if(input$metricInput1 == "Luxurious"){
      timeline(input$metricInput1, input$luxInput, input$plotTypeInput)$data
    }
    else{
      timeline(input$metricInput1, input$accInput, input$plotTypeInput)$data
    }
  })
  
  output$pieplot = renderHighchart(month_pie(input$monthInput, input$metricInput2)$plot)
  output$data_mw = renderDataTable(month_pie(input$monthInput, input$metricInput2)$data)
  
  output$accPlot = renderHighchart(saving_report(input$accountInput)$plot)
  output$accData = renderDataTable(saving_report(input$accountInput)$data)
}

shinyApp(ui =ui, server = server)