# Test bot - paper trading #

#devtools::install_github("dashee87/betScrapeR")
#devtools::install_github("phillc73/abettor")
library(betScrapeR)
library(abettor)
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
  #stake <- s3readRDS('misc/stake.rds', bucket = "bluffball-x")
  stake <- readRDS('./03 Bots/data/stake.rds')
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
    
    # ---- Utilities: new day etc ----
    # Check if it's a new day
    #date <- s3readRDS('misc/date.rds', bucket = "bluffball-x")
    date <- readRDS('./03 Bots/data/date.rds')
    
    # If it is, get the day's liability
    if(date < Sys.Date()) {
      
      # Get balance from Betfair
      start.balance <- abettor::checkBalance()$availableToBetBalance
      
      # Set liability per bet. Between 3 and 50.
      stake <- max(start.balance/20, 4)
      stake <- min(stake, 50)
      
      # Upload to S3
      # s3saveRDS(stake,
      #           bucket = "bluffball-x", 
      #           object = "misc/stake.rds")
      
      saveRDS(stake, './03 Bots/data/stake.rds')
      
    }
    
    # Every 10 minutes, match to oddsportal again. Otherwise it seems to miss some
    time <- readRDS('./03 Bots/data/match_time.rds')
    
    if (time < Sys.time() - 10 * 60) {
      
      source('./03 Bots/Functions/new_day.R')
      
      time <- Sys.time()
      
      saveRDS(time, './03 Bots/data/match_time.rds')
    }
    
    # Get lookup
    #lookup <- s3readRDS('misc/oddsportal-betfair-lookup.rds', bucket = "bluffball-x")
    lookup <- readRDS('./03 Bots/data/lookup.rds')
    
    # Update date variable
    date <- Sys.Date()
    
    # Save to S3
    # s3saveRDS(date, 
    #           bucket = "bluffball-x", 
    #           object = "misc/date.rds")
    
    saveRDS(date, './03 Bots/data/date.rds')
    
    # Mark initialisation time
    init.time <- Sys.time()
    
    # ----- Get current bets ----
      
    # Read file
    #current_bets <- s3readRDS('current/current_bets.rds', bucket = "bluffball-x")
    current_bets <- readRDS('./03 Bots/data/current_bets.rds')
    
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
        
        # Upload to s3
        file <- paste("./03 Bots/Completed_bets/", Sys.time(), " completed.rds", sep = "")
        file <- gsub(":", "-", file)
        # s3saveRDS(completed, 
        #           bucket = "bluffball-x", 
        #           object = file)
        saveRDS(completed, file)
        
      }
      
      # Filter current bets to active
      current_bets <- current_bets.2 %>% filter(status == "ACTIVE") %>%
        select(-status)
      
      # Log progress
      print(Sys.time())
      print(paste("Current bets checked.",
                  "Balance:", balance))
    } else {
      print(Sys.time())
      print("No current bets")
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
    
    # Add to data, join to betfair lookup
    oddsportal.3 <- oddsportal.2 %>%
      mutate(h = odds.h$dec,
             d = odds.d$dec,
             a = odds.a$dec) %>%
      mutate(h.team = trimws(teams$V1),
             a.team = trimws(teams$V2)) %>%
      mutate(h.team = trimws(gsub("\\s+", " ", h.team)),
             a.team = trimws(gsub("\\s+", " ", a.team)),
             h.team = trimws(gsub("\\([^\\]]*\\)", "", h.team, perl=TRUE)),
             a.team = trimws(gsub("\\([^\\]]*\\)", "", a.team, perl=TRUE))) %>%
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
            if(length(p) > 0) {
              p[1,]
            } else {
              data.frame(price = 0, size = 0)
            }
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
      print(paste("Matched", nrow(oddsportal.3), "games out of", nrow(oddsportal.2)))
      
      # ------- Betting strategy -------
      
      # backup object
      odds.matched.4 <- odds.matched.2
      
      # Get opportunities. This bit is important!
      opps <- odds.matched.4 %>%
        filter(price != 0) %>%
        mutate(alpha = case_when(home.away == 1 & back.lay == "back" ~ 0.05,  # Intercepts from kaunitz et al
                                 home.away == 2 & back.lay == "back" ~ 0.05,
                                 home.away == 3 & back.lay == "back" ~ 0.05,
                                 home.away == 1 & back.lay == "lay" ~ 0.025,  # Intercepts from analysis of kaunitz et al data
                                 home.away == 2 & back.lay == "lay" ~ 0.025,
                                 home.away == 3 & back.lay == "lay" ~ 0.04),
               slope = case_when(home.away == 1 ~ 1.003,  # Slopes from kaunitz et al
                                 home.away == 2 ~ 1.012,
                                 home.away == 3 ~ 1.081),
               true.p = round(slope * (1/odds) - alpha, 2), # Probabilities are accurate when rounded to nearest 1%, historically
               true.p.not = 1 - true.p) %>%
        select(-slope) %>%
        filter(true.p > 0) %>% # Remove any very long shots
        mutate(liability = stake,
               stake = ifelse(back.lay == "back",
                              liability,
                              liability/(price - 1)),
               stake = ifelse(size < stake, size, stake), # Only bet up to what's available
               xv = ifelse(back.lay == "back",
                           (0.95 * true.p * (price - 1) * stake) - (true.p.not * stake),
                           (0.95 * stake * true.p.not) - (true.p * liability))
        )
      
      # Save opportunities
      fwrite(opps, './03 Bots/data/opps.csv')
      
      # Get current bets from betfair
      current_ids <- listCurrentOrders()$marketId
      
      # Function below gets as many bets as you can afford such that
      # xv is maximised without raising the chance of losing every bet
      # above a certain threshold.
      # Currently not active: selects as many as possible, in order of xv
      # Might need to edit this in future to deal with case when stake is greater than size available
      newopps <- opps %>%
        filter(!marketId %in% current_ids) %>% # Double check now duplicates
        mutate(bet.time = Sys.time()) %>%
        filter(stake >= 2, # Need to be above betfair's minimum
               xv > 0.01) %>% # positive expected value
        arrange(desc(xv)) %>%
        group_by(marketId) %>%
        filter(row_number(desc(xv)) == 1) %>% # Get highest value bet per game
        ungroup %>%
        filter(row_number(desc(xv)) * liability < unlist(balance)) # Add to current bets if you can afford them
      
      # ---- Place bets ----
      
      if(nrow(newopps) > 0) {
        
        for (i in 1:nrow(newopps)) {
          
          # Get details
          size = as.character(round(newopps[i, 'stake'], 2))
          marketId = as.character(newopps[i, 'marketId'])
          selectionId = as.character(newopps[i, 'id'])
          betSide = toupper(as.character(newopps[i, 'back.lay']))
          price = as.character(newopps[i, 'price'])
          
          # Place bet!
          PlaceBetReturn <- placeOrders(marketId = marketId,
                                        selectionId = selectionId,
                                        betSide = betSide,
                                        betType = "LIMIT",
                                        betSize = size,
                                        reqPrice = price,
                                        persistenceType = "LAPSE"
          )
          
          print('Bet placed!')
          
        }
        
      }
      
      # Record end time
      end.time <- Sys.time()
      
      
      # Update current bets
      current_bets <- rbind(current_bets, newopps)
      
      # Save as current bets
      # s3saveRDS(current_bets,
      #           bucket = "bluffball-x", 
      #           object = "current/current_bets.rds")
      
      saveRDS(current_bets, './03 Bots/data/current_bets.rds')
      
      # ------ Results -------
      
      # Save data every 30 minutes
      save.time <- ifelse(!exists("save.time"), 0, save.time)
      if (Sys.time() - save.time > 1800) {
        save.time <- Sys.time()
        time <- as.character(save.time) %>%
          gsub(":", "-", .)
        file <- paste(time, "matched_odds.rds", sep = "_")
        
        # Save matched bets
        # upload the object to S3
        # s3saveRDS(opps,
        #           bucket = "bluffball-x",
        #           object = paste0("matched_odds/", file))
        
        # Save all oddsportal odds
        # upload the object to S3
        # s3saveRDS(oddsportal.3, 
        #            bucket = "bluffball-x",
        #            object = paste0("oddsportal/", time, "_odds.rds"))
        
      }
      
      # Check if you've already bet on all the matched odds
      all.bet <- all(odds.matched.1$marketId %in% current_bets$marketId)
      
      # Get kickoff time of first game you've bet on
      first.time <- min(as.POSIXct(current_bets$kickoff))
      
      # Check balance
      balance <- checkBalance()[1]
      print(paste("Balance:", balance))
      #print(paste("Winnings to date:", wins.td))
      #print(paste("Losses to date:", loss.td))
      #print(paste0("Returns to date: ", ret.td, "%"))
      print(paste("New bets:", nrow(newopps)))
      print(paste("Current bets:", nrow(current_bets)))
      print(paste0("Expected profit from current bets: £", 
                   round(sum(current_bets$xv), 2)))
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
      
      # Sleep if all bet
      if(all.bet) {
        print(paste("All matched odds have been bet on. Sleeping until",
                    first.time))
        
        # Clear out memory
        rm(list= ls()[!(ls() %in% keepvars)])
        gc()
        
        # Sys.sleep(60*60*(hour(first.time) - hour(as.POSIXct(Sys.time()))))
        Sys.sleep(60*10)
        
        # Log back in
        loginBF(key.file[1],key.file[2],key.file[3])
      }
    } else if(nrow(oddsportal.4) > 0){
      
      # Matches available, but in more than 5 hours
      
      # Get time 5 hours before next match
      hour5 <- min(oddsportal.4$kickoff) - 60 * 60 * 4.9
      
      # Clear memory
      rm(list= ls()[!(ls() %in% keepvars)])
      gc()
      
      # Sleep
      print("More matches to bet on, but none in the next 5 hours.")
      print(paste("Sleeping until", hour5))
      Sys.sleep(as.integer(hour5) - as.integer(as.POSIXct(Sys.time())))
      #Sys.sleep(60*10)
      print("Waking up...")
      
      # Log back in
      loginBF(key.file[1],key.file[2],key.file[3])
      
    } else {
      
      # No more matches, sleep til tomorrow
      rm(list= ls()[!(ls() %in% keepvars)])
      gc()
      print("No more new matches today. See you tomorrow!")
      # Sys.sleep(as.integer(as.POSIXct(paste(Sys.Date() + 1, "00:10"), tz = "")) - as.integer(as.POSIXct(Sys.time())))
      Sys.sleep(60*30)
      print("Waking up...")
      
      # Log back in
      loginBF(key.file[1],key.file[2],key.file[3])
    }
    
    # Pause between each iteration, for disk and memory management
    Sys.sleep(60)
  }, error = function(e) {
    
    # Code error. Log and wait
    fileConn <- file("BluffBall X error Log.txt")
    writeLines(c(paste("Error at", Sys.time()),
                 paste(e)), 
               fileConn)
    close(fileConn)
    
    # Clear memory
    print("Code encountered an error somewhere. Sleeping for 10 mins.")
    Sys.sleep(60*10)
    
    rm(list= ls()[!(ls() %in% keepvars)])
    gc()
    
    print("Waking up...")
    
    # Log back in
    loginBF(key.file[1],key.file[2],key.file[3])
    
  }, finally = {
    
    # Log back in
    loginBF(key.file[1],key.file[2],key.file[3])
    
    # Clear memory
    rm(list= ls()[!(ls() %in% keepvars)])
    gc()
    
  })
  
}

# ---- End ----


