
library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(betScrapeR)
library(abettor)
library(dplyr)
library(ggplot2)

options(shiny.usecairo = TRUE)

# Define UI for application
shinyUI(dashboardPage(
  dashboardHeader(title = "Jeff"),
  dashboardSidebar(),
  dashboardBody(
    
    # Title
    h1("Unbelievable Jeff!"),
    em(h3("BluffBallX returns to date")),
    
    # Per bet
    fluidRow(
      box(title = "Full results to date",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("perBet"))
      
      ),
    
    # Per day
    fluidRow(
      h3("Closing balances per day"),
      box(width = 6, plotlyOutput("perDay")),
      box(width = 6, plotlyOutput("perDay2"))
      
    ),
    
    # Forecasts
    fluidRow(
      h3("Forecasts"),
      box(widtg = 12,
        title = "Controls",
        sliderInput("slider", "Number of days to forecast:", 1, 60, 14)
    ),
    
    fluidRow(
      box(width = 12,
          status = "info",
          solidHeader = TRUE,
          plotlyOutput('smooth'))
    )
    
    
    )
  )
))
