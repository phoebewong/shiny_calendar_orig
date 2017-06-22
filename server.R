# Shiny 2.2
library(dplyr)
library(ggplot2)
library("ggthemes")
library(scales)
library(shiny)
library(data.table)
library(plotly)
library(igraph)
library(networkD3)
# To do:
# 1. Remove "Canceled:"
# 2. Provide a text entry box to remove certain events
# 3. Show numbers in % (checkbox option)
# 4. Calculate percentage of work time/given count of work days
# 5. barplot of events by months
# 6. Remove Appointment
# template <- read.csv("./data/df_template.csv", header = TRUE, sep = ",", comment.char="",stringsAsFactors = FALSE)

# To-do:
# .	Exclude meeting invites from specific person (from Quant Event?)
# .	Exclude certain events?
# .	Add "Download Plot" buttons [ggplot]
# . Change plot 1 to refelct input$n


shinyServer(function(input, output, session) {
  #output$instruction <- renderText({return( paste("Instruction to download csv file of your calendar","http://tinnes.co.uk/desktopcalendar/support/7/faq_importexport_exportoutlook2010.php"))})
  
  meet.df <- reactive({
    infile <- input$meet.df
    # if (is.null(infile)) {return(NULL)} # User has not uploaded a file yet
    if (is.null(infile)) {
      df <- read.csv("./data/df_template.csv", header = TRUE, sep = ",", comment.char="",stringsAsFactors = FALSE)
    } else{
      df <- read.csv(infile$datapath, header = TRUE, sep = ",", comment.char="",stringsAsFactors = FALSE) 
      }
    return (df)
  })

  display.df <- reactive({
    #validate(need(input$meet.df != "", "Please upload a data set"))
    display.df <- meet.df()
    # Changing All day event
    display.df$All.day.event <- ifelse(display.df$All.day.event == "True", TRUE, FALSE)
    
    if(input$showallday == TRUE){
      display.df <- subset(display.df, display.df$All.day.event == FALSE & display.df$Required.Attendees != "" & display.df$Meeting.Organizer != "")
      }
    else{display.df <- display.df}
    
    # Cleaning date
    display.df$start <- as.POSIXct(paste(display.df$Start.Date, display.df$Start.Time), format = "%m/%d/%Y %I:%M:%S %p")
    display.df$end   <- as.POSIXct(paste(display.df$End.Date, display.df$End.Time), format = "%m/%d/%Y %I:%M:%S %p")
    display.df$Start.Date <- as.Date.character(display.df$Start.Date, format = "%m/%d/%Y")
    display.df$End.Date <- as.Date.character(display.df$End.Date, format = "%m/%d/%Y")
    min.date <- input$dates[1]
    max.date <- input$dates[2]
    display.df <- subset(display.df, display.df$Start.Date >= min.date & display.df$End.Date <= max.date)
    
    # Cleaning time
    display.df$duration <- as.numeric(display.df$end-display.df$start)
    display.df$hour <- as.numeric(format(display.df$start, "%H")) # unclass((as.POSIXlt(display.df$start)))$hour
    display.df$wday <- lubridate::wday(display.df$start) - 1 # unclass((as.POSIXlt(display.df$start)))$wday #wday(display.df$start)
    
    display.df$Required.Attendees[display.df$Required.Attendees == ""] <- NA
    display.df$Optional.Attendees[display.df$Optional.Attendees == ""] <- NA
    
    display.df$startyr   <- format(display.df$start, "%Y")
    display.df$startmon  <- format(display.df$start, "%m")
    display.df$startyrmon <- format(display.df$start, "%Y-%m")
    
    return(display.df)
  })
  # Columns to show (reactive)
  observe({ 
    x <- colnames(meet.df())
    # Can use character(0) to remove all choices
    if (is.null(x)) x <- character(0)
    updateSelectizeInput(session, inputId = "show_vars",
                         label = "Columns to show:",
                         choices = x,
                         selected = x[c(1:6, 10:12, 14)])
  })
  
  # Date of data to show (reactive datarangeinput) ####
  output$dates <- renderUI({
    df <- meet.df()
    # print(df)
    min <- min(as.Date.character(df$Start.Date, format = "%m/%d/%Y"))
    max <- max(as.Date.character(df$Start.Date, format = "%m/%d/%Y"))
    dateRangeInput('dates', label = "Date input:",
                   start = min, end = max,
                   min = min, max = max)
  })

  output$table <- renderDataTable({
    #validate(need(input$meet.df != "", "Please upload a data set"))
    display.df()[, input$show_vars, drop = FALSE]
  })
  
  #output$date <- renderText({format(input$dateRange[1])})
  # Number of people to show (reactive sliderbar)
  observe({
    df <- display.df()
    n <- length(unique(df$Meeting.Organizer))
    updateSliderInput(session, "n", value = 20, min = 1, max = n)
  })
  
  # output$plot1title <- renderText({
  #   #validate(need(input$meet.df != "", "Please upload a data set"))
  #   paste("Top", input$n, "people who initaited a meeting with me from", input$dates[1], "to", input$dates[2])})
  
  plot1 <- reactive({
    meet_clean.df <- display.df()
    n <- input$n
    people.freq.df <- as.data.frame(table(meet_clean.df$Meeting.Organizer)) %>%
      top_n(n, Freq) %>% #show n rows, will show >n rows if there're ties
      arrange(desc(Freq)) #%>% #show by descending order of Freq
    # rename(Name=Var1) #-> people.freq.df
    rot <- 45
    # sort <- "Freq"
    
    # if (sort == "Freq"){
    people.freq.df$Var1 <- factor(people.freq.df$Var1)
    people.freq.df$Var1 <- reorder(people.freq.df$Var1, -(people.freq.df$Freq))
    # } else {
    #   people.freq.df$Var1 <- people.freq.df$Var1
    # }
    # 
    ggplot(data=people.freq.df, aes(x=Var1, y=Freq)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5) +
      labs(title = paste("Top", input$n, "people who initaited a meeting with me from", input$dates[1], "to", input$dates[2]),
            x = "Name") +
      
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = rot, hjust = 1)) +
      scale_fill_fivethirtyeight() #doesn't 
  })
  output$plot1 <- renderPlot({
    #validate(need(input$meet.df != "", ""))
    return(plot1())
  })
  
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste('plot', '.png', sep='')
    },
    content=function(file){
      png(file)
      print(plot1())
      dev.off()
    },
    contentType='image/png')

  # output$plot2title <- renderText({
  #   #validate(need(input$meet.df != "", ""))
  #   paste("Top", input$n, "people who attended a meeting with me from", input$dates[1], "to", input$dates[2])})
  
  output$plot2 <- renderPlot({
    #validate(need(input$meet.df != "", ""))
    meet.df <- display.df()
    n <- input$n
    split.func <- function(x){ return (unlist(strsplit(x, split = ";")))}
    split2.func <- function(x){ return (strsplit(x, split = ";"))}
    # To avoid NA as character
    meeting.org <- as.character(na.omit(unlist(sapply(meet.df$Meeting.Organizer, split.func))))
    required.attendee <- as.character(na.omit(unlist(sapply(meet.df$Required.Attendees, split.func))))
    optional.attendee <- as.character(na.omit(unlist(sapply(meet.df$Optional.Attendees, split.func))))

    all.attendee <- c(meeting.org, required.attendee, optional.attendee)
      
    people.freq.df <- as.data.frame(table(all.attendee)) %>%
      top_n(n, Freq) %>% #show n rows, will show >n rows if there're ties
      arrange(desc(Freq)) #%>% #show by descending order of Freq
      #rename(Name=all.attendee2) -> people.freq.df
    rot <- 45
    # sort <- "Freq" #or by "Alphabet"

    # if (sort == "Freq"){
      people.freq.df$all.attendee <- factor(people.freq.df$all.attendee)
      people.freq.df$all.attendee <- reorder(people.freq.df$all.attendee, -(people.freq.df$Freq))
    # } else {
    #   people.freq.df$Name <- people.freq.df$Name
    # 

    ggplot(data=people.freq.df, aes(x=all.attendee, y=Freq)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5) +
      labs(title = paste("Top", input$n, "people who attended a meeting with me from", input$dates[1], "to", input$dates[2]),
           x = "Name") + 
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = rot, hjust = 1)) +
      scale_fill_fivethirtyeight()
  })
  
  output$monthplot <- renderPlot({
    meet.df <- display.df()
    ## Number of meetings per month (plot) ####
    if(input$sortbyFreq == TRUE){ #sorting by Frequency
      month.df <- as.data.frame(table(meet.df$startyrmon)) %>%
        arrange(desc(Freq)) #sort by Frequency
      month.df[,1] <- factor(month.df[,1])
      month.df[,1] <- reorder(month.df[,1], -(month.df[,2]))
    }
    else{month.df <- as.data.frame(table(meet.df$startyrmon))}
    
    colnames(month.df) <- c("Month", "Freq")
    rot = 45
    
    g <- ggplot(month.df, aes(x=Month, y=Freq))+
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5) +
      labs(x = "Year-Month", title = "Number of Meetings per Month") +
      geom_hline(yintercept=mean(month.df$Freq), col = "dark grey", linetype = "dashed")+
      geom_text(x=Inf, y=mean(month.df$Freq), label = round(mean(month.df$Freq)), hjust=1, vjust=-0.5)+
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = rot, hjust = 1)) +
      scale_fill_fivethirtyeight()
    return (g)
  })
  
  output$monthtimeplot <- renderPlot({
    meet.df <- display.df()
    # Duration of meetings per month (plot) ####
    if(input$sortbyFreq == TRUE){
    time.month.df <- as.data.frame(aggregate(meet.df$duration, by = list(meet.df$startyrmon), mean)) %>%
        arrange(desc(x)) #sort by Frequency
    colnames(time.month.df) <- c("Month", "Duration")
    time.month.df$Month <- factor(time.month.df$Month)
    time.month.df$Month <- reorder(time.month.df$Month, -(time.month.df$Duration))   
    } else{
    time.month.df <- as.data.frame(aggregate(meet.df$duration, by = list(meet.df$startyrmon), mean))
    colnames(time.month.df) <- c("Month", "Duration") 
    }
    rot <- 45
    
    g <- ggplot(time.month.df, aes(x=Month, y=Duration)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Duration)), vjust = -0.5) +
      labs(x = "Year-Month", y = "Duration (Minutes)", title = "Average Duration (Minutes) Per Month") +
      geom_hline(yintercept=mean(time.month.df$Duration), col = "dark grey", linetype = "dashed")+
      geom_text(x=Inf, y=mean(time.month.df$Duration), label = round(mean(time.month.df$Duration)), hjust=1, vjust=-0.5)+
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = rot, hjust = 1)) +
      scale_fill_fivethirtyeight()
    
    return(g)
  })
  
  output$avgtimeplot <- renderPlot({
    
  })
  output$timeplot1 <- renderPlotly({
    temp <- display.df()
    temp <- subset(temp, temp$wday >= 1 & temp$wday <= 5)
    time.df <- as.data.frame(table(temp$wday, temp$hour))
    colnames(time.df) <- c("wday", "hour", "Freq")
    time.df$hour <- factor(time.df$hour, levels = levels(time.df$hour)[nlevels(time.df$hour):1])
    time.df$desc <- character(nrow(time.df))
    
    for (i in 1:nlevels(time.df$wday)){
      for (j in 1:nlevels(time.df$hour)){
        day <- unique(time.df$wday)[i]
        hour <- unique(time.df$hour)[j]
        
        index <- which(temp$wday == day & temp$hour == hour)
        desc <- as.character(names(sort(table(temp$Subject[index]), decreasing  = TRUE)))
        
        index2 <- as.numeric(which(time.df$wday == day & time.df$hour == hour))
        time.df$desc[index2] <- desc[1] #show top 3 meeting subject
      }
    }
    if (input$showname == TRUE){
      g <- ggplot(time.df, aes(x = wday, y = hour, fill = Freq, text = paste("Event:",desc)))
    }else{ g <- ggplot(time.df, aes(x = wday, y = hour, fill = Freq))}
    # g <- ggplot(time.df, aes(x = wday, y = hour, fill = Freq, text = paste("Event:",desc)))
      g <- g + 
         geom_tile(color="white", size=0.1) +
        scale_fill_gradient(low = "white", high = "red") + 
        labs(x="Day", y="Hour", title="Meetings per weekday & time of day")
        # theme_tufte(base_family="Helvetica")

    return(ggplotly(g))
    
  })
  
  output$netplot1 <- renderPlot({
    netplot.df <- display.df()
    netplot.df$all <- paste(netplot.df$Meeting.Organizer, netplot.df$Required.Attendees, sep = ";")
    split2.func <- function(x){ return (strsplit(x, split = ";"))}
    netplot.df$all2 <- sapply(netplot.df$all, split2.func)
    temp <- unlist(netplot.df$all2) #everyone's name
    temp <- temp[temp != ""] #manual clean up
    n <- as.numeric(length(unique(temp)))
    
    people.df <- data.frame(matrix(0, ncol = n, nrow = 1))
    colnames(people.df) <- unique(temp)
    for (i in 1:ncol(people.df)){
      for (j in 1:nrow(netplot.df)){
        people.df[j,i] <- grepl(colnames(people.df)[i], netplot.df$all2[j]) 
      }
    }
    data <- people.df
    
    total_occurrences <- colSums(data)
    index <- total_occurrences >= sort(total_occurrences, decreasing = TRUE)[input$net_n] #median(total_occurrences)
    data <- subset(data, select = index == TRUE)
    total_occurrences <- colSums(data)
    
    data_matrix <- as.matrix(data)
    co_occurrence <- t(data_matrix) %*% data_matrix
    graph <- graph.adjacency(co_occurrence,
                             weighted=TRUE,
                             mode="undirected",
                             diag=FALSE)
    if(input$random_net == FALSE) {set.seed(33)}
    return(plot(graph,
         #vertex.label=names(data),
         vertex.size=total_occurrences*input$node_size/500,
         edge.width=E(graph)$weight*input$net_width/1000))
  })
  
  output$netplot2 <- renderForceNetwork({
    netplot.df <- display.df()
    netplot.df$all <- paste(netplot.df$Meeting.Organizer, netplot.df$Required.Attendees, sep = ";")
    split2.func <- function(x){ return (strsplit(x, split = ";"))}
    netplot.df$all2 <- sapply(netplot.df$all, split2.func)
    temp <- unlist(netplot.df$all2) #everyone's name
    temp <- temp[temp != ""] #manual clean up
    n <- as.numeric(length(unique(temp)))
    
    people.df <- data.frame(matrix(0, ncol = n, nrow = 1))
    colnames(people.df) <- unique(temp)
    for (i in 1:ncol(people.df)){
      for (j in 1:nrow(netplot.df)){
        people.df[j,i] <- grepl(colnames(people.df)[i], netplot.df$all2[j]) 
      }
    }
    data <- people.df
    
    total_occurrences <- colSums(data)
    index <- total_occurrences >= sort(total_occurrences, decreasing = TRUE)[input$net_n] #median(total_occurrences)
    data <- subset(data, select = index == TRUE)
    total_occurrences <- colSums(data)
    
    data_matrix <- as.matrix(data)
    co_occurrence <- t(data_matrix) %*% data_matrix
    graph <- graph.adjacency(co_occurrence,
                             weighted=TRUE,
                             mode="undirected",
                             diag=FALSE)
    wc <- cluster_walktrap(graph)
    members <- membership(wc)
    graph_in <- igraph_to_networkD3(graph, group = members)
    
    return(forceNetwork(Links = graph_in$links, Nodes = graph_in$nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group', zoom = TRUE))
  })
  
  output$summary1 <- renderText({
    paste("From", input$dates[1], "to", input$dates[2],  ", I had", nrow(display.df()), "meetings")})
  output$summary2 <- renderText({
    meet.df <- display.df()
    n <- input$n
    split.func <- function(x){ return (unlist(strsplit(x, split = ";")))}
    split2.func <- function(x){ return (strsplit(x, split = ";"))}
    # To avoid NA as character
    meeting.org <- as.character(na.omit(unlist(sapply(meet.df$Meeting.Organizer, split.func))))
    required.attendee <- as.character(na.omit(unlist(sapply(meet.df$Required.Attendees, split.func))))
    optional.attendee <- as.character(na.omit(unlist(sapply(meet.df$Optional.Attendees, split.func))))
    
    all.attendee <- c(meeting.org, required.attendee)
    
    people.freq.df <- as.data.frame(table(all.attendee)) %>%
      top_n(n, Freq) %>% #show n rows, will show >n rows if there're ties
      arrange(desc(Freq)) #%>% #show by descending order of Freq
    #rename(Name=all.attendee2) -> people.freq.df
    rot <- 45
    # sort <- "Freq" #or by "Alphabet"
    
    # if (sort == "Freq"){
    people.freq.df$all.attendee <- factor(people.freq.df$all.attendee)
    people.freq.df$all.attendee <- reorder(people.freq.df$all.attendee, -(people.freq.df$Freq))
    return(paste0("Most of them are with ", people.freq.df$all.attendee[2], "."))
    })
  
  output$summary3 <- renderText({
    meet.df <- display.df()
    n <- input$n
    split.func <- function(x){ return (unlist(strsplit(x, split = ";")))}
    split2.func <- function(x){ return (strsplit(x, split = ";"))}
    # To avoid NA as character
    meeting.org <- as.character(na.omit(unlist(sapply(meet.df$Meeting.Organizer, split.func))))
    required.attendee <- as.character(na.omit(unlist(sapply(meet.df$Required.Attendees, split.func))))
    optional.attendee <- as.character(na.omit(unlist(sapply(meet.df$Optional.Attendees, split.func))))
    
    all.attendee <- c(meeting.org, required.attendee)
    
    people.freq.df <- as.data.frame(table(all.attendee)) %>%
      top_n(n, Freq) %>% #show n rows, will show >n rows if there're ties
      arrange(desc(Freq)) #%>% #show by descending order of Freq
    #rename(Name=all.attendee2) -> people.freq.df
    rot <- 45
    # sort <- "Freq" #or by "Alphabet"
    
    # if (sort == "Freq"){
    people.freq.df$all.attendee <- factor(people.freq.df$all.attendee)
    return(paste0("To be exact, ", round(people.freq.df$Freq[2]/nrow(display.df())* 100), "% of my meetings are with ", people.freq.df$all.attendee[2]))
  })
  
  
})