# Collect data

library(rvest)
library(dplyr)

# Data from research paper
url <- 'http://184.73.28.182/'
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
data <- html_nodes(webpage,'td')
text <- html_text(data[[2]])

# Betfair
url <- 'https://www.betfair.com/exchange/plus/football'
webpage <- read_html(url)
buttons <- html_nodes(webpage, '.bet-button-price')
