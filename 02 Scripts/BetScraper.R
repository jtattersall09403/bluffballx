#devtools::install_github("dashee87/betScrapeR")
#devtools::install_github("phillc73/abettor")
library("betScrapeR")
library("abettor")
library(XML)
library(dplyr)
library(rvest)
library(data.table)
library(fastLink)

# Log in
loginBF("jacktattersall92@gmail.com","Jack1992","aBiNJgPrYSh3u5Iw")

# Get football matches
matches <- listMarketCatalogue(eventTypeIds = c("1"),
                               fromDate = (format(Sys.time()+60*20, "%Y-%m-%dT%TZ")),
                               toDate = (format(Sys.time()+60*60*24, "%Y-%m-%dT%TZ")),
                               maxResults = "200",
                               marketTypeCodes = c("MATCH_ODDS"))

# some ugly stuff to correct for British Summer Time (well, I do enjoy later sunsets...)
matches$marketStartTimeUTC <- as.POSIXct(matches$marketStartTime,format="%Y-%m-%dT%H:%M","Europe/London")
matches$marketStartTimeUTC <- matches$marketStartTimeUTC+ 60*60*(format(matches$marketStartTimeUTC,format="%Z")=="BST")
matches$marketStartDate <- as.Date(matches$marketStartTimeUTC)
matches$venuetime <- paste(matches$event$venue,substring(matches$marketStartTimeUTC,12,16))

# View(matches)

# ---- Scrape oddsportal ----
# Data from research paper
url <- 'http://184.73.28.182/'
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
data <- html_nodes(webpage,'td')
result <- lapply(seq(1, length(data), by = 10), function(i) {
  tmp = sapply(i:(i+9), function(x) html_text(data[[x]]))[1:9]
  data.frame(t(tmp))
}) %>% rbindlist

# Add names
names(result) <- c("sport", "match", "league", "bet", "date", "time.remaining",
                   "best.bookie", "best.odds", "mean.median")

# Remove invalid characters
result$match <- gsub("[[:punct:]]", "", result$match)

View(result)  
View(matches$event$name)

# Get team to back
result.teams <- strsplit(result$match, " vs ") %>% do.call(rbind, .) %>% 
  trimws %>% as.data.frame()
result.teams

# Match to odds
result.2 <- cbind(result, result.teams) %>%
  mutate(back = ifelse(bet == 1, V1,
                       ifelse(bet == 2, V2, 'X'))) %>%
  mutate(mean.odds = as.numeric(substr(mean.median, 1, 4))) %>%
  select(-V1, -V2) %>%
  mutate(match = gsub(" vs ", " v ", 
                      gsub("FC", " ", 
                           gsub("IF", " ", 
                                gsub("\\s+", " ", match)))))

result.2$match <- trimws(gsub("\\s+", " ", result.2$match))

# ---- Match to betfair by event name ----

# Get relevant betfair variables
matches.2 <- data.frame(cbind(matches[, c("marketId",
                                          "marketStartTime",
                                          "marketStartTimeUTC",
                                          "marketStartDate",
                                          "venuetime",
                                          "totalMatched")],
                              comp.name = matches$competition$name,
                              event.name = matches$event$name,
                              matches$event$timezone)
                        ) %>%
  mutate(event.name = gsub(" vs ", " v ", 
                      gsub("FC", " ", 
                           gsub("IF", " ", 
                                gsub("\\s+", " ", event.name)))))
matches.2$event.name <- trimws(gsub("\\s+", " ", matches.2$event.name))

# Get left and right tables
right <- matches.2
left <- result.2 %>% rename(event.name = match,
                            comp.name = league)

matched.games <- left %>% inner_join(right, by = "event.name")

# ---- Get matches ----

match.prices <- lapply(1:nrow(matched.games), function(i) {
  
  # Get row
  match <- matched.games[i,]
  
  # Get teams
  #betfair.teams <- match$runners[[1]]$runnerName
  
  
  # Get betfair exchange info about that match
  betfair.info <- abettor::listMarketBook(marketIds=match$marketId, 
                                          priceData = "EX_BEST_OFFERS")
  
  # Check if valid
  if(length(betfair.info)==0)
    print("No market data returned. Invalid market ID and/or session token expired?")
  if(betfair.info$status=="CLOSED"){
    print("That market is closed")
  }
  
  # Back
  betfair.back <- unlist(lapply(betfair.info$runners[[1]]$ex$availableToBack[runners],
                                function(x){
                                  if(length(x)==0){
                                    data.frame(price=NA,size=NA)
                                  } else {as.data.frame(x)[1,]
                                  }
                                }),
                         use.names = FALSE)
  
  # Get lay prices
  betfair.lay <- unlist(lapply(betfair.info$runners[[1]]$ex$availableToLay[runners],
                               function(x){
                                 if(length(x)==0){
                                   data.frame(price=NA,size=NA)
                                 } else {
                                   as.data.frame(x)[1,]
                                 }
                               }),
                        use.names = FALSE)
  
  # Combine prices
  betfair.prices <- rbind(betfair.info$runners[[1]]$selectionId[runners],
                          as.data.frame(matrix(betfair.back,2,3)),
                          as.data.frame(matrix(betfair.lay,2,3)))
  
  colnames(betfair.prices) <- betfair.teams
  row.names(betfair.prices) <- c("Selection ID","Back Price","Back Size","Lay Price","Lay Size")
  betfair.prices$var = row.names(betfair.prices)
  
  # Get clean rows
  prices <- melt(betfair.prices, id.var = "var") %>% 
    filter(var == "Back Price") %>%
    select(-var) %>%
    rename(back = value,
           team = variable) %>% 
    mutate(team = ifelse(team=="The Draw", "X", team)) %>%
    mutate(dum = 1)
  
  # Record progress
  print(paste("Processed", i))
  
  # Return dataframe
  data.frame(unlist(match)) %>% t %>%
    as.data.frame() %>%
    select(marketId,
           marketStartTime,
           competition.name,
           event.id,
           event.name,
           event.countryCode) %>%
    mutate(dum = 1) %>%
    inner_join(prices, by = "dum")
})

# Combine 
match.prices.2 <- rbindlist(match.prices) 

