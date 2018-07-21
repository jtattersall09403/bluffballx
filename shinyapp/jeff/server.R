
library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(betScrapeR)
library(abettor)
library(dplyr)
library(ggplot2)
library(data.table)

options(shiny.usecairo = TRUE)

# ---- Get data ----

# Get login details
key.file <- readLines('Key')

# Log in
loginBF(key.file[1],key.file[2],key.file[3])

st <- list()
st[[1]] <-   abettor::getAccountStatement(fromRecordValue = 0) %>%
  select(refId, itemDate, balance, amount)

warning(st[[1]])

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # ---- Get data with progress bar ----
  
  # Get all statements
  withProgress(message = "Getting statements", value = 1, {
    i <<- 1
    while(nrow(st[[i]]) %% 100 == 0) {
      i <<- i + 1
      print(i)
      st[[i]] <<- abettor::getAccountStatement(fromRecordValue = (i-1) * 100) %>%
        select(refId, itemDate, balance, amount)
    }
  })
  
  st <- rbindlist(st) %>% unique
  
  # Format
  st.2 <- st %>%
    select(refId, itemDate, balance, amount) %>%
    mutate(itemDate = as.POSIXct(itemDate, format = "%Y-%m-%dT%H:%M:%S")) %>%
    arrange(itemDate) %>%
    mutate(index = row_number())
  
  
  # Smooth data
  st.3 <- st.2 %>%
    filter(refId != 0) %>%
    mutate(date = as.Date(itemDate)) %>%
    group_by(date) %>%
    summarise(amount = sum(amount)) %>%
    ungroup %>%
    mutate(balance = cumsum(amount),
           index = row_number(date))
  
  
  # ---- Plots ----
  
  output$perBet <- renderPlotly({
    
    # Value so far
    p = st.2 %>%
      filter(refId != 0) %>%
      mutate(balance = cumsum(amount)) %>%
      ggplot(aes(x = index, y = balance, text = paste('Index:', index, 
                                                      '<br>Balance:', round(balance, 2)),
                 group = 1)) +
      geom_line(colour = "dodgerblue4") +
      theme_minimal() +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = c("text"))
    
  })

  output$perDay <- renderPlotly({
    
    # By day
    p = st.2 %>%
      filter(refId != 0) %>%
      mutate(date = as.Date(itemDate)) %>%
      group_by(date) %>%
      summarise(amount = sum(amount)) %>%
      ungroup %>%
      mutate(balance = cumsum(amount),
             index = as.factor(row_number(date))) %>%
      ggplot(aes(x = index, y = balance, text = paste('Index:', index, 
                                                      '<br>Closing balance:', round(balance, 2)))) +
      geom_bar(colour = "dodgerblue4", fill = "dodgerblue", stat = "identity") +
      theme_minimal() +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = "text")
    
  })
  
  output$perDay2 <- renderPlotly({
    
    # Line
    p = st.2 %>%
      filter(refId != 0) %>%
      mutate(date = as.Date(itemDate)) %>%
      group_by(date) %>%
      summarise(amount = sum(amount)) %>%
      ungroup %>%
      mutate(balance = cumsum(amount),
             index = row_number(date)) %>%
      ggplot(aes(x = index, y = balance, group = 1,
                 text = paste('Index:', index, 
                              '<br>Closing balance:', round(balance, 2)))) +
      geom_line(colour = "dodgerblue4") +
      theme_minimal() +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$smooth <- renderPlotly({
    
    # Smooth plot
    p = st.3 %>%
      ggplot(aes(x = index, y = balance, group = 1,
                 text = paste('Day:', index, 
                              '<br>Closing balance:', round(balance, 2)))) +
      geom_point(colour = "dodgerblue4") +
      stat_smooth(method="lm", fullrange=TRUE) +
      theme_minimal() +
      expand_limits(y = 0, x = c(0, nrow(st.3) + input$slider))
    
    ggplotly(p)
    
  })

  
  output$perMonth <- renderPlotly({
    
    # Profits per month
    p = st.2 %>%
      filter(refId != 0) %>%
      mutate(date = as.Date(itemDate)) %>%
      group_by(date) %>%
      summarise(amount = sum(amount)) %>%
      ungroup %>%
      mutate(index = as.factor(floor(row_number(date)/31) + 1)) %>%
      group_by(index) %>%
      summarise(amount = sum(amount)) %>%
      ggplot(aes(x = index, y = amount)) +
      geom_bar(colour = "dodgerblue4", fill = "dodgerblue", stat = "identity") +
      theme_minimal() +
      expand_limits(y = 0)
    
    ggplotly(p)
    
  })
  
  
})
