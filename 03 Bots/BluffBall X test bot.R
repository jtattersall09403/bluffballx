# Test bot - paper trading #

#devtools::install_github("dashee87/betScrapeR")
#devtools::install_github("phillc73/abettor")
library("betScrapeR")
library("abettor")
library(XML)
library(dplyr)
library(rvest)
library(data.table)
library(fastLink)

# Get login details
key.file <- readLines('Key')

# Log in
loginBF(key.file[1],key.file[2],key.file[3])

# ---- Start bot ----

# Dummy variable for while loop
dummy <- TRUE

# Let's go!
while(dummy == "TRUE") {
  
  # If no funds available, wait for 1 hour
  if (balance < 10) {
    print("Low balance, sleeping for 1 hour")
    Sys.sleep(60*60)
  }
  
  # Refresh session
  keepAlive()
  
  # Check if it's a new day
  date <- readRDS('./03 Bots/admin/date.rds')
  
  # If it is, get the days matches
  if(date < Sys.Date()) {
    
    
    
  }
  
  # Mark initialisation time
  init.time <- Sys.time()
  
  # ----- Get current bets ----
  
  # Completed bets
  comp <- list.files('./03 Bots/Completed_bets')
  completed <- fread(paste0('./03 Bots/Completed_bets/',
                              comp[[length(comp)]]))
  
  if(file.exists('./03 Bots/Current_bets/bets.rds')) {
    
    # Read file
    current_bets <- readRDS('./03 Bots/Current_bets/bets.rds')
    
    # Get outcomes
    statuses <- lapply(1:nrow(current_bets), function(i) {
      
      # Get row
      match <- current_bets[i,]
      
      # Get betfair exchange info about that match
      betfair.info <- abettor::listMarketBook(marketIds=match$marketId, 
                                              priceData = "SP_AVAILABLE")
      
      # Check if valid
      if(length(betfair.info)==0) {
        print("No market data returned. Invalid market ID and/or session token expired?")
      } else {
        
        # Get all prices
        status <- betfair.info$runners[[1]]$status
        ids <- betfair.info$runners[[1]]$selectionId
        
        # Record progress
        # print(paste("Status processed", i))
        
        # Return value
        data.frame(marketId=betfair.info$marketId,
                   id=ids,
                   status=status)
        
      }
      
      
    }) %>% rbindlist %>% unique
    
    # Match to current bets
    current_bets.2 <- current_bets %>%
      inner_join(statuses, by = c("marketId",
                                  "back.id"="id")) %>%
      unique
    
    # Record results history
    completed.2 <- current_bets.2 %>%
      filter(status %in% c("WINNER", "LOSER"))
    
    if(nrow(completed.2) > nrow(completed)) {
      completed <- completed.2
      fwrite(completed, paste0('./03 Bots/Completed_bets/',
                    gsub(":", "-", as.character(Sys.time())),
                    '_completed.csv'))
    }
    
    # If any are complete, calculate returns and remove from current bets
    wins <- current_bets.2 %>%
      filter(status == "WINNER") %>%
      summarise(profit = sum(bf-1)) %>% unlist %>% as.numeric()
    
    losses <- current_bets.2 %>%
      filter(status == "LOSER") %>%
      summarise(profit = n()) %>% unlist %>% as.numeric()
    
    balance <<- balance + wins
    
    current_bets <- current_bets.2 %>% filter(status == "ACTIVE") %>%
      select(-status)
    
    print(Sys.time())
    print(paste("Current bets checked. Wins:", wins, "Losses:", losses,
                "Balance:", balance))
  }
  
  # ---------- Get matches from Betfair --------
  
  # Get football matches
  matches <- tryCatch({
    listMarketCatalogue(eventTypeIds = c("1"),
                        fromDate = (format(Sys.time()+60*10, "%Y-%m-%dT%TZ")),
                        toDate = (format(as.POSIXct(paste(Sys.Date(), "23:59"),
                                                    format="%Y-%m-%d %H:%M"), "%Y-%m-%dT%TZ")),
                        maxResults = "200",
                        marketTypeCodes = c("MATCH_ODDS"))
  }, error = function(e) {
    
    # Log back in
    loginBF("jacktattersall92@gmail.com","Jack1992","aBiNJgPrYSh3u5Iw")
    
    return(
      listMarketCatalogue(eventTypeIds = c("1"),
                          fromDate = (format(Sys.time()+60*10, "%Y-%m-%dT%TZ")),
                          toDate = (format(as.POSIXct(paste(Sys.Date(), "23:59"),
                                                      format="%Y-%m-%d %H:%M"), "%Y-%m-%dT%TZ")),
                          maxResults = "200",
                          marketTypeCodes = c("MATCH_ODDS"))
    )}
  )
  
  # some ugly stuff to correct for British Summer Time (well, I do enjoy later sunsets...)
  matches$marketStartTimeUTC <- as.POSIXct(matches$marketStartTime,format="%Y-%m-%dT%H:%M","Europe/London")
  matches$marketStartDate <- as.Date(matches$marketStartTimeUTC)
  
  # View(matches)
  
  # ---- Scrape from oddsportal ----
  
  # Record start time - i.e. time between scraping odds and placing bet
  start.time <- Sys.time()
  
  ## change Phantom.js scrape file
  url <- 'http://www.oddsportal.com/matches/soccer/'
  
  lines <- lines <- readLines("scrape_final.js")
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, "scrape_final.js")
  
  ## Download website
  system("phantomjs scrape_final.js")
  
  ### use Rvest to scrape the downloaded website.
  web <- read_html("1.html")
  
  # Get kickoff times
  times <- html_nodes(web, '.datet')
  times.2 <- html_text(times)
  
  # Get matches
  games <- html_nodes(web, '.table-participant') %>% html_text
  
  # Scores (for completed and in-play games)
  scores <- html_nodes(web, '.table-score') %>% html_text
  
  # Odds
  odds <- html_nodes(web, '.odds-nowrp') %>% html_text %>%
    matrix(nrow = 3) %>%
    t %>%
    as.data.frame %>%
    rename(home=V1,
           draw=V2,
           away=V3)
  
  # Number of bookies
  bookies <- html_nodes(web, '.info-value') %>% html_text
  
  # Combine
  oddsportal <- data.frame(game=games,
                           time=times.2,
                           bs=bookies) %>%
    cbind(odds)
  
  print("Scraped oddsportal data")
  
  # ---- Clean oddsportal data ----
  
  # Filter to those more than an hour in the future,
  # with at least 3 bookies
  oddsportal.2 <- oddsportal %>%
    mutate(date = Sys.Date()) %>%
    mutate(kickoff = paste(date, time)) %>%
    mutate(kickoff = as.POSIXct(kickoff, format="%Y-%m-%d %H:%M","Europe/London")) %>%
    filter(kickoff >= Sys.time() + 60*10,
           bs >= 3)
  
  # Convert odds to decimal
  odds.h <- strsplit(oddsportal.2$home, "/") %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    mutate_all(as.numeric) %>%
    mutate(dec = (V1+V2)/V2)
  
  odds.d <- strsplit(oddsportal.2$draw, "/") %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    mutate_all(as.numeric) %>%
    mutate(dec = (V1+V2)/V2)
  
  odds.a <- strsplit(oddsportal.2$away, "/") %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    mutate_all(as.numeric) %>%
    mutate(dec = (V1+V2)/V2)
  
  # Split out teams
  teams <- strsplit(oddsportal.2$game, " - ") %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    mutate_all(trimws)
  
  # Add to data
  oddsportal.3 <- oddsportal.2 %>%
    mutate(h = odds.h$dec,
           d = odds.d$dec,
           a = odds.a$dec) %>%
    mutate(h.team = teams$V1,
           a.team = teams$V2) %>%
    select(game, kickoff, h.team, a.team, h, d, a, bs)
  
  # ---- Match to betfair by event name ----
  
  # Get relevant betfair variables
  matches.2 <- data.frame(cbind(matches[, c("marketId",
                                            "marketStartTime",
                                            "marketStartTimeUTC",
                                            "marketStartDate",
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
  
  # Add home and away team names
  matches.3 <- lapply(matches$runners, function(runner) {
    data.frame(team.h.id = runner$selectionId[1],
               team.h = runner$runnerName[1],
               team.a.id = runner$selectionId[2],
               team.a = runner$runnerName[2],
               draw.id = runner$selectionId[3])
  }) %>% rbindlist %>%
    cbind(matches.2)
  
  
  # ---- Match oddsportal to betfair ----
  
  # Simple join
  odds.matched <- oddsportal.3 %>%
    inner_join(matches.3, by = c("h.team"="team.h",
                                 "a.team"="team.a"))
  
  
  print("Matched to betfair games")
  
  # ---- Get betfair odds ----
  
  match.prices <- lapply(1:nrow(odds.matched), function(i) {
    
    # Get row
    match <- odds.matched[i,]
    
    # Get betfair exchange info about that match
    betfair.info <- tryCatch({
      abettor::listMarketBook(marketIds=match$marketId, 
                              priceData = "EX_BEST_OFFERS",
                              virtualise = TRUE)
    }, error = function(e) {
      
      # Log in
      loginBF("jacktattersall92@gmail.com","Jack1992","aBiNJgPrYSh3u5Iw")
      
      return(
        abettor::listMarketBook(marketIds=match$marketId, 
                                priceData = "EX_BEST_OFFERS",
                                virtualise = TRUE)
      )
    })
    
    # Check if valid
    if(length(betfair.info)==0) {
      print("No market data returned. Invalid market ID and/or session token expired?")
      
    } else if(betfair.info$status=="CLOSED"){
      print("That market is closed")
    } else {
      
      # Get all prices
      prices <- betfair.info$runners[[1]]$ex$availableToBack
      ids <- betfair.info$runners[[1]]$selectionId
      
      # Record progress
      # print(paste("Processed", i))
      
      # For each result, get maximum odds with at least £10 available
      lapply(prices, function(p) {
        p[1,]
      }) %>% do.call(rbind, .) %>% cbind(ids) %>%
        mutate(marketId = betfair.info$marketId)
      
    }
    
    
  })
  
  # Combine 
  match.prices.2 <- rbindlist(match.prices)
  
  # Join to matched odds
  odds.matched.2 <- odds.matched %>%
    inner_join(match.prices.2, (by = c("marketId",
                                       "team.h.id"="ids"))) %>%
    rename(h.b=price,
           h.b.s=size) %>%
    inner_join(match.prices.2, (by = c("marketId",
                                       "draw.id"="ids"))) %>%
    rename(d.b=price,
           d.b.s=size) %>%
    inner_join(match.prices.2, (by = c("marketId",
                                       "team.a.id"="ids"))) %>%
    rename(a.b=price,
           a.b.s=size)
  
  # ------- Betting strategy -------
  
  # Melt oddsportal odds
  op <- odds.matched.2 %>%
    select(marketId,
           comp.name,
           game,
           team.h.id,
           draw.id,
           team.a.id,
           h,
           d,
           a) %>%
    melt(id.vars = c("marketId",
                     "comp.name",
                     "game",
                     "team.h.id",
                     "draw.id",
                     "team.a.id"))
  
  # Melt betfair odds
  bf <- odds.matched.2 %>%
    select(marketId,
           game,
           team.h.id,
           draw.id,
           team.a.id,
           h.b,
           d.b,
           a.b) %>%
    melt(id.vars = c("marketId",
                     "game",
                     "team.h.id",
                     "draw.id",
                     "team.a.id"))
  
  # Melt sizes
  bf.s <- odds.matched.2 %>%
    select(marketId,
           game,
           team.h.id,
           draw.id,
           team.a.id,
           h.b.s,
           d.b.s,
           a.b.s) %>%
    melt(id.vars = c("marketId",
                     "game",
                     "team.h.id",
                     "draw.id",
                     "team.a.id"))
  
  # Merge
  odds.matched.4 <- cbind(op, "bf"=bf$value, "size"=bf.s$value)
  
  # Get opportunities
  opps <- odds.matched.4 %>%
    mutate(dif = bf - value) %>%
    mutate(xr = (1/value) * bf,
           xv = (1/value) * (bf - 1) - (1-1/value),
           xv = 0.95 * xv) %>%
    filter(xv > 1.1) %>%
    arrange(desc(dif)) %>%
    mutate(back.id = case_when(variable == 'a' ~ team.a.id,
                               variable == 'd' ~ draw.id,
                               variable == 'h' ~ team.h.id))
  
  # Filter to new bets
  newopps <- opps %>%
    filter(!marketId %in% current_bets$marketId |
             (marketId %in% current_bets$marketId &
                !back.id %in% current_bets$back.id)) %>%
    arrange(desc(xv)) %>%
    mutate(cum.cost = row_number()) %>%
    filter(cum.cost < balance)
  
  # Add to current bets if you can afford them
  cost <- ifelse(nrow(newopps) > 0, 
                 max(newopps$cum.cost, na.rm = TRUE),
                 0)
  balance <<- balance - cost
  
  # Update current bets
  newopps <- newopps %>% select(-cum.cost)
  current_bets <- rbind(current_bets, newopps)
  
  # Save as current bets
  saveRDS(current_bets, './03 Bots/Current_bets/bets.rds')
  
  # ------ Results -------
  
  # Save data every 30 minutes
  if (Sys.time() - save.time > 1800) {
    save.time <- Sys.time()
    time <- as.character(save.time) %>%
      gsub(":", "-", .)
    file <- paste(time, "matched_odds.rds", sep = "_")
    saveRDS(odds.matched, paste0('./03 Bots/Matched_odds/', file))
    saveRDS(oddsportal.3, paste0('./03 Bots/Oddsportal/', time, "_odds.rds"))
  }
  
  
  # Record end time
  end.time <- Sys.time()
  print(paste("Balance:", balance))
  print(paste("New bets:", nrow(newopps)))
  print(paste("Current bets:", nrow(current_bets)))
  print(paste0("Expected profit from current bets: ", 
               round(100*sum(current_bets$xv)/nrow(current_bets), 1),
               "%"))
  print(paste("Initiation to result time:", 
              round(end.time - init.time, 1),
              "seconds"))
  print(paste("Scrape to result time:", 
              round(end.time - start.time, 1),
              "seconds"))
  print("---------------------------------------")
  print("---------------------------------------")
  
  # Log progress
  fileConn <- file("BluffBall X Log.txt")
  writeLines(c(paste(Sys.time()),
               (paste("Balance:", balance)),
               (paste("New bets:", nrow(newopps))),
               (paste("Current bets:", nrow(current_bets))),
               (paste0("Expected profit from current bets: ", 
                            round(100*sum(current_bets$xv)/nrow(current_bets), 1),
                            "%")),
               (paste("Initiation to result time:", 
                           round(end.time - init.time, 1),
                           "seconds")),
               (paste("Scrape to result time:", 
                           round(end.time - start.time, 1),
                           "seconds")),
               ("---------------------------------------"),
               ("---------------------------------------")), fileConn)
  close(fileConn)
  
  # Log current bets
  tryCatch(fwrite(current_bets, "Current_bets.csv"),
           error = function(e) fwrite(current_bets, "Current_bets_1.csv"))
}


