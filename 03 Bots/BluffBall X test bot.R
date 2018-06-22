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
library(aws.s3)

# Set up your keys and your region here.
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAJZMWCIS25K3PAIJQ",
           "AWS_SECRET_ACCESS_KEY" = "1oxq+8ug+dn9zMibwQwZlOadgqzRG4XJxJjjCMQO",
           "AWS_DEFAULT_REGION" = "us-east-2")

# Get login details
key.file <- readLines('Key')

# Log in
loginBF(key.file[1],key.file[2],key.file[3])

# Variables not to clear out
keepvars <- c('keepvars',
              'balance',
              'dummy',
              'key.file',
              'save.time',
              'stake',
              'all.bet',
              'first.time',
              'get_optimal',
              'hour5')

# ---- Start bot ----

# Dummy variable for while loop
dummy <- TRUE

# Let's go!
while(dummy == "TRUE") {
  
  # Get stake and balance
  stake <- readRDS('./03 Bots/stake.rds')
  balance <- abettor::checkBalance()$availableToBetBalance
  
  # Try to run bot. On error, pause for 1 hour.
  tryCatch({
    # If no funds available, wait for 1 hour
    if (balance <= stake) {
      print("Low balance, sleeping for 1 hour")
      Sys.sleep(60*60)
      
      # Log back in
      loginBF(key.file[1],key.file[2],key.file[3])
      
    }
    
    # Refresh session
    keepAlive()
    
    # Check if it's a new day
    date <- readRDS('./03 Bots/admin/date.rds')
    
    # If it is, get the days matches and daily liability
    if(date < Sys.Date()) {
      
      # Get balance from Betfair
      start.balance <- abettor::checkBalance()$availableToBetBalance
      
      # Set liability per bet. Between 2 and 50.
      stake <- max(start.balance/20, 2)
      stake <- min(stake, 50)
      saveRDS(stake, '03 Bots/stake.rds')
      
      source('./03 Bots/Functions/new_day.R')
      
    }
    
    # Update date variable
    date <- Sys.Date()
    saveRDS(date, './03 Bots/admin/date.rds')
    
    # Mark initialisation time
    init.time <- Sys.time()
    
    # ----- Get current bets ----
    
    # Completed bets
    # comp <- list.files('./03 Bots/Completed_bets')
    # completed <- fread(paste0('./03 Bots/Completed_bets/',
    #                             comp[[length(comp)]]))
    
    if(file.exists('./03 Bots/Current_bets/bets.rds')) {
      
      # Read file
      current_bets <- readRDS('./03 Bots/Current_bets/bets.rds')
      
      # Check if any current bets
      if(nrow(current_bets) > 0) {
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
                                      "id")) %>%
          unique
        
        # Record results history
        completed <- current_bets.2 %>%
          filter(status %in% c("WINNER", "LOSER")) %>%
          mutate(kickoff = as.character(kickoff),
                 marketId = as.numeric(marketId))
        
        # Need to sort out variable formats here
        if(nrow(completed) > 0) {
          
          # Save completed bets
          # fwrite(completed, paste0('./03 Bots/Completed_bets/',
          #                          gsub(":", "-", as.character(Sys.time())),
          #                          '_completed.csv'))
          
          # write to an in-memory raw connection
          zz <- rawConnection(raw(0), "r+")
          write.csv(completed, zz)
          
          # upload the object to S3
          aws.s3::put_object(file = rawConnectionValue(zz), bucket = "bluffball-x", object = "completed.csv")
          
          # close the connection
          close(zz)
          
        }
        
        # Filter current bets to active
        current_bets <- current_bets.2 %>% filter(status == "ACTIVE") %>%
          select(-status)
        
        # Save
        saveRDS(current_bets, './03 Bots/Current_bets/bets.rds')
        
        print(Sys.time())
        print(paste("Current bets checked.",
                    "Balance:", balance))
      } else {
        print("No current bets")
      }
      
      
    }
    
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
    
    # Odds - some sort of bug occasionally
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
                             bs=as.numeric(bookies)) %>%
      cbind(odds)
    
    print("Scraped oddsportal data")
    
    # ---- Clean oddsportal data ----
    
    # Filter to those more than 10 mins in the future,
    # with at least 3 bookies
    oddsportal.2 <- oddsportal %>%
      mutate(date = Sys.Date()) %>%
      mutate(kickoff = paste(date, time)) %>%
      mutate(kickoff = as.POSIXct(kickoff, format="%Y-%m-%d %H:%M","Europe/London"))
    
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
    
    # Get lookup
    lookup <- readRDS('./03 Bots/admin/oddsportal-betfair-lookup.rds')
    
    # Add to data, join to betfair lookup
    oddsportal.3 <- oddsportal.2 %>%
      mutate(h = odds.h$dec,
             d = odds.d$dec,
             a = odds.a$dec) %>%
      mutate(h.team = teams$V1,
             a.team = teams$V2) %>%
      select(game, kickoff, h.team, a.team, h, d, a, bs) %>%
      inner_join(lookup, by = c("h.team",
                                "a.team")) %>%
      mutate(kickoff = kickoff + 60*60) # Correct for BST
    
    # Filter out matches you've already bet on,
    # and to those less than 5 hours in the future. 
    # For carrying forward.
    odds.matched.1 <- oddsportal.3 %>%
      filter(kickoff >= Sys.time() + 60,
             kickoff <= Sys.time() + 60 * 60 * 5,
             bs >= 3) %>%
      filter(!marketId %in% current_bets$marketId)
    
    # Get games that are more than 5 hours in advance but meet other criteria
    # (for sleep calculations)
    oddsportal.4 <- filter(oddsportal.3,
                           kickoff > Sys.time() + 60 * 60 * 5,
                           bs >= 3,
                           !marketId %in% current_bets$marketId)
    
    
    # ---- Check if any bets remaining today ----
    if (nrow(odds.matched.1) > 0) {
      
      # If not, continue
      
      # Randomly select up to 10 bets.
      # This is to minimise the time between scraping and placing the bet,
      # and thus to minimise price movements
      
      odds.matched <- odds.matched.1 %>%
        sample_n(size = ifelse(nrow(.) < 10, nrow(.), 10))
      
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
          back.prices <- betfair.info$runners[[1]]$ex$availableToBack
          lay.prices <- betfair.info$runners[[1]]$ex$availableToLay
          id <- betfair.info$runners[[1]]$selectionId
          
          # Record progress
          # print(paste("Processed", i))
          
          #  Get back prices
          back <- lapply(back.prices, function(p) {
            p[1,]
          }) %>% do.call(rbind, .) %>% cbind(id) %>%
            mutate(marketId = betfair.info$marketId) %>%
            rename(back.price = price, back.size = size)
          
          # Get lay prices
          lay <- lapply(lay.prices, function(p) {
            p[1,]
          }) %>% do.call(rbind, .) %>% cbind(id) %>%
            mutate(marketId = betfair.info$marketId) %>%
            rename(lay.price = price, lay.size = size)
          
          # Return data frame
          inner_join(back, lay, by = c("marketId", "id")) %>%
            select(marketId, id, everything())
          
        }
        
        
      })
      
      # Combine 
      match.prices.2 <- rbindlist(match.prices)
      
      # Join to matched odds
      odds.matched.2 <- odds.matched %>%
        mutate(draw = "draw") %>%
        rename(odds_1 = h,
               odds_2 = a,
               odds_3 = d,
               team_1 = h.team,
               team_2 = a.team,
               team_3 = draw,
               id_1 = team.h.id,
               id_2 = team.a.id,
               id_3 = draw.id) %>%
        reshape(varying = c("odds_1",
                            "odds_2",
                            "odds_3",
                            "team_1",
                            "team_2",
                            "team_3",
                            "id_1",
                            "id_2",
                            "id_3"),
                direction="long", idvar="marketId", sep="_",
                "timevar" = "home.away") %>%
        inner_join(match.prices.2, by = c("marketId", "id")) %>%
        mutate(betId = row_number(game)) %>%
        rename(price_1 = back.price,
               price_2 = lay.price,
               size_1 = back.size,
               size_2 = lay.size) %>%
        reshape(varying = c("price_1", "price_2", "size_1", "size_2"),
                direction="long", idvar="betId", sep="_",
                "timevar" = "back.lay") %>%
        mutate(back.lay = ifelse(back.lay == 1, "back", "lay"))
      
      print("Matched to betfair odds")
      
      # ------- Betting strategy -------
      
      # backup object
      odds.matched.4 <- odds.matched.2
      
      # Get opportunities. This bit is important!
      opps <- odds.matched.4 %>%
        mutate(alpha = case_when(home.away == 1 ~ 0.034,  # Intercepts from kaunitz et al
                                 home.away == 2 ~ 0.037,
                                 home.away == 3 ~ 0.057),
               true.p = (1/odds) - alpha,
               true.p.not = 1 - true.p) %>%
        filter(true.p > 0) %>% # Remove any very long shots
        mutate(liability = stake,
               stake = ifelse(back.lay == "back",
                              liability,
                              liability/(price - 1)),
               stake = ifelse(size < stake, size, stake), # Only bet up to what's available
               xv = ifelse(back.lay == "back",
                           (0.95 * true.p * (price - 1)) - (true.p.not),
                           ((0.95 * stake * true.p.not) - (true.p * liability))/liability)
        )
      
      # # Current probability of losing every bet
      # prod <- prod(1-(1/(current_bets$value) - 0.05))
      # prod <- ifelse(prod == 0, 1, prod)
      
      # Function below gets as many bets as you can afford such that
      # xv is maximised without raising the chance of losing every bet
      # above a certain threshold.
      # Currently not active: selects as many as possible, in order of xv
      # Might need to edit this in future to deal with case when stake is greater than size available
      newopps <- opps %>%
        filter(row_number(desc(xv)) * stake < balance) %>% # Add to current bets if you can afford them
        mutate(bet.time = Sys.time()) %>%
        filter(stake > 2, # Need to be above betfair's minimum
               xv > 0.03) %>% # targeting at least 3% profit
        arrange(desc(xv)) %>%
        group_by(marketId) %>%
        filter(row_number(desc(xv)) == 1) %>% # Get highest value bet per game
        ungroup
      
      cost <- sum(newopps$liability)
      
      # Update current bets
      current_bets <- rbind(current_bets, newopps)
      
      # Save as current bets
      saveRDS(current_bets, './03 Bots/Current_bets/bets.rds')
      
      # ------ Results -------
      
      # Save data every 30 minutes
      save.time <- ifelse(!exists("save.time"), 0, save.time)
      if (Sys.time() - save.time > 1800) {
        save.time <- Sys.time()
        time <- as.character(save.time) %>%
          gsub(":", "-", .)
        file <- paste(time, "matched_odds.rds", sep = "_")
        saveRDS(opps, paste0('./03 Bots/Matched_odds/', file))
        saveRDS(oddsportal.3, paste0('./03 Bots/Oddsportal/', time, "_odds.rds"))
      }
      
      # Check if you've already bet on all the matched odds
      all.bet <- all(odds.matched.1$marketId %in% current_bets$marketId)
      
      # Get kickoff time of first game you've bet on
      first.time <- min(as.POSIXct(current_bets$kickoff))
      
      # Check total wins and losses
      # wins.td <- round(summarise(filter(completed,
      #                                   status == "WINNER"),
      #                            profit = sum((bf - 1) * stake)), 2)
      # loss.td <- round(summarise(filter(completed,
      #                                   status == "LOSER"),
      #                            profit = sum(stake)), 2)
      # ret.td <- round(100*(wins.td-loss.td)/nrow(completed), 2)
      
      # Record end time
      end.time <- Sys.time()
      print(paste("Balance:", balance))
      #print(paste("Winnings to date:", wins.td))
      #print(paste("Losses to date:", loss.td))
      #print(paste0("Returns to date: ", ret.td, "%"))
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
      
      
      
      # Sleep if all bet
      if(all.bet) {
        print(paste("All matched odds have been bet on. Sleeping until",
                    first.time))
        
        # Clear out memory
        rm(list= ls()[!(ls() %in% keepvars)])
        gc()
        
        Sys.sleep(60*60*(hour(first.time) - hour(as.POSIXct(Sys.time()))))
        
        # Log back in
        loginBF(key.file[1],key.file[2],key.file[3])
      }
    } else if(nrow(oddsportal.4) > 0){
      
      # Matches available, but in more than 5 hours
      
      # Get time 4 hours before next match
      hour5 <- min(oddsportal.4$kickoff - 60 * 60 * 4)
      
      # Clear memory
      rm(list= ls()[!(ls() %in% keepvars)])
      gc()
      
      # Sleep
      print("More matches to bet on, but none in the next 5 hours.")
      print(paste("Sleeping until", hour5))
      Sys.sleep(as.integer(hour5) - as.integer(as.POSIXct(Sys.time())))
      print("Waking up...")
      
      # Log back in
      loginBF(key.file[1],key.file[2],key.file[3])
      
    } else {
      
      # No more matches, sleep til tomorrow
      rm(list= ls()[!(ls() %in% keepvars)])
      gc()
      print("No more new matches today. See you tomorrow!")
      Sys.sleep(as.integer(as.POSIXct(paste(Sys.Date() + 1, "00:30"))) - as.integer(as.POSIXct(Sys.time())))
      print("Waking up...")
      
      # Log back in
      loginBF(key.file[1],key.file[2],key.file[3])
    }
    
    # Pause between each iteration, for disk and memory management
    Sys.sleep(60)
  }, error = function(e) {
    
    # Code error. Log and wait
    fileConn <- file("BluffBall X error Log.txt")
    text <- readLines(fileConn)
    writeLines(c(text,
                 paste("Error at", Sys.time()),
                 paste(e),
                 "Sleeping for 1 hour"), 
               fileConn)
    close(fileConn)
    
    # Clear memory
    rm(list= ls()[!(ls() %in% keepvars)])
    gc()
    print("No more new matches today. See you tomorrow!")
    Sys.sleep(60*60)
    print("Waking up...")
    
    # Log back in
    loginBF(key.file[1],key.file[2],key.file[3])
    
  })
  
}


