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

# Record start time. Takes about 12 seconds.
start.time <- Sys.time()

# Get football matches
matches <- listMarketCatalogue(eventTypeIds = c("1"),
                               fromDate = (format(Sys.time()+60*20, "%Y-%m-%dT%TZ")),
                               toDate = (format(as.POSIXct(paste(Sys.Date(), "23:59"),
                                                           format="%Y-%m-%d %H:%M"), "%Y-%m-%dT%TZ")),
                               maxResults = "200",
                               marketTypeCodes = c("MATCH_ODDS"))

# some ugly stuff to correct for British Summer Time (well, I do enjoy later sunsets...)
matches$marketStartTimeUTC <- as.POSIXct(matches$marketStartTime,format="%Y-%m-%dT%H:%M","Europe/London")
matches$marketStartDate <- as.Date(matches$marketStartTimeUTC)

# View(matches)

# ---- Scrape from oddsportal ----

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

# ---- Clean oddsportal data ----

# Filter to those more than an hour in the future,
# with at least 3 bookies
oddsportal.2 <- oddsportal %>%
  mutate(date = Sys.Date()) %>%
  mutate(kickoff = paste(date, time)) %>%
  mutate(kickoff = as.POSIXct(kickoff, format="%Y-%m-%d %H:%M","Europe/London")) %>%
  filter(kickoff >= Sys.time() + 60*60,
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



# ---- Get betfair odds ----

match.prices <- lapply(1:nrow(odds.matched), function(i) {
  
  # Get row
  match <- odds.matched[i,]
  
  # Get betfair exchange info about that match
  betfair.info <- abettor::listMarketBook(marketIds=match$marketId, 
                                          priceData = "EX_BEST_OFFERS")
  
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
    print(paste("Processed", i))
    
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
  filter(dif > 0.1) %>%
  arrange(desc(dif)) %>%
  mutate(xr = (1/value) * bf,
         xv = (1/value) * (bf - 1) - (1-1/value))

# Record end time
end.time <- Sys.time()
end.time - start.time


# ------ Results -------

View(opps)

# Expected value of taking these bets
sum(opps$xv)

# Expected value if you bet the full amount available
sum(opps$xv * opps$size)
