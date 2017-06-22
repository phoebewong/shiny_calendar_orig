# Shiny 2.2
library(shiny)
library(plotly)
library(shinythemes)
library(networkD3)

shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
  
  titlePanel("Statistics of My Meetings"),
  
  sidebarLayout(
    sidebarPanel(
      textOutput("instruction"),
      # File input 
      fileInput('meet.df', 'Choose CSV File',
                accept=c(#'text/csv', 
                         #'text/comma-separated-values,text/plain', 
                         '.csv','.CSV')),
      # Add a horizontal line
      tags$hr(), 
      # Columns to show
      selectInput('show_vars','Columns to show:', choices = NULL, #placeholder for columns
                  multiple = TRUE, selectize = TRUE),
      
      # Date input
      uiOutput("dates"),
      checkboxInput("showallday", "Exclude All Day Events", value = TRUE)#,
      #checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Table", dataTableOutput("table"),
                                    verbatimTextOutput("date") ),
                  tabPanel("Summary", 
                           h1(textOutput("summary1")),
                           h2(textOutput("summary2")),
                           h3(textOutput("summary3")),
                           checkboxInput("sortbyFreq", "Sort by Frequency", value = FALSE),
                           plotOutput("monthplot"),
                           downloadButton("downloadPlot1", "Download the Plot"),
                           plotOutput("monthtimeplot")),
                  
                  tabPanel("Plot: People", 
                           # Number of people to show
                           sliderInput("n", "Number of People to Show:",min=1, max=40, value=20),
                           # h2(textOutput("plot1title")),
                           plotOutput("plot1"),
                           # h2(textOutput("plot2title")),
                           plotOutput("plot2")),
                  tabPanel("Plot: Time", plotlyOutput("timeplot1"),
                           checkboxInput("showname", "Show Event Name", value = TRUE)),
                  tabPanel("Plot: Network", plotOutput("netplot1"),
                           checkboxInput("random_net", "Rearrange the plot after changes", value = FALSE),
                           sliderInput("net_n", "Number of People in the Network:",min=1, max=40, value=5),
                           sliderInput("net_width", "Link Width:",min=1, max=100, value=20),
                           sliderInput("node_size", "Node Size:",min=1, max=100, value=50),
                           forceNetworkOutput("netplot2", width = "100%", height = "500px")) #
                  
                  )
                  
      # h3("Statistics"),
      # textOutput("scene"),
      # plotOutput("plot1")#,
      # h3("Predicted Horsepower from Model 2:"),
      # textOutput("pred2"),
      # textOutput("name")
    )  
  )
))