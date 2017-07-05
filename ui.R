# Shiny 2.2
library(shiny)
library(plotly)
library(shinythemes)
library(networkD3)
library(markdown)
shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
  
  titlePanel("Statistics of My Meetings"),
  
  sidebarLayout(
    sidebarPanel(
      # File input 
      fileInput('meet.df', 'Choose CSV File',
                accept=c(#'text/csv', 
                         #'text/comma-separated-values,text/plain', 
                         '.csv','.CSV')),
      # Add a horizontal line
      tags$hr(), 
      
      selectInput('myname','My name is', choices = NULL, #placeholder for names
                  multiple = FALSE, selectize = TRUE),
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
                  tabPanel("Introduction", 
                           includeMarkdown("intro.md")),
                  tabPanel("Table", dataTableOutput("table"),
                                    verbatimTextOutput("date") ),
                  tabPanel("Summary", 
                           h2(textOutput("summary1")),
                           sliderInput("workhours", "Work Hours per Day:",min=1, max=24, value=8),
                           # numericInput("bizdaycount", "Number of Business Days:", value = NULL,
                           #              min=1, max=100, step=1),
                           h3(textOutput("summary2")),
                           checkboxInput("sortbyFreq", "Sort by Frequency", value = FALSE),
                           plotOutput("monthplot"),
                           downloadButton("downloadmonthplot", "Download the Plot Above"),
                           plotOutput("monthtimeplot"),
                           downloadButton("downloadmonthtimeplot", "Download the Plot Above")),
                  
                  tabPanel("Plot: People",
                           h3(textOutput("summary3")),
                           tags$hr(),
                           # Number of people to show
                           sliderInput("n", "Number of People to Show:",min=1, max=40, value=20),
                           plotOutput("plot1"),
                           downloadButton("downloadplot1", "Download the Plot Above"),
                           
                           plotOutput("plot2"),
                           downloadButton("downloadplot2", "Download the Plot Above")),
                  
                  tabPanel("Interactive Heatmap: Time", plotlyOutput("timeplot1"),
                           checkboxInput("showname", "Show Event Name", value = TRUE)),
                  tabPanel("Network Plot", plotOutput("netplot1"),
                           checkboxInput("random_net", "Rearrange the plot after changes", value = FALSE),
                           sliderInput("net_n", "Number of People in the Network:",min=1, max=40, value=5),
                           sliderInput("net_width", "Link Width:",min=1, max=100, value=20),
                           sliderInput("node_size", "Node Size:",min=1, max=100, value=50),
                           forceNetworkOutput("netplot2", width = "100%", height = "500px")),
                  tabPanel("Fun Fact", 
                           h4(textOutput("summarydateplot")),
                           plotOutput("dateplot"),
                           downloadButton("downloaddateplot", "Download the Plot Above")),
                  tabPanel("Contact", 
                           includeMarkdown("contact.md"))
                  )
    )  
  )
))