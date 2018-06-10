# ----- Script to run at the start of a new day to get market id lookup ----

# Oddsportal to betfair lookup #

# -------- Oddsportal -------
## change Phantom.js scrape file
getTeams <- function(url) {
  lines <- readLines("scrape_final.js")
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
  
  # Combine
  oddsportal <- data.frame(game=games,
                           time=times.2,
                           stringsAsFactors = FALSE)
  
  # Split out teams
  teams <- strsplit(oddsportal$game, " - ") %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    mutate_all(trimws)
  
  # Add to data
  oddsportal.3 <- oddsportal %>%
    mutate(h.team = teams$V1,
           a.team = teams$V2) %>%
    select(game, time, h.team, a.team)
  
  return(oddsportal.3)
}

url <- 'http://www.oddsportal.com/matches/soccer/'

# Get teams
oddsportal <- getTeams(url)

# Append into single variable
oddsportal.teams <- oddsportal %>%
  select(-game) %>%
  mutate(date = Sys.Date()) %>%
  mutate(time = paste(date, time)) %>%
  mutate(h.team = trimws(gsub("\\s+", " ", h.team)),
         a.team = trimws(gsub("\\s+", " ", a.team)))


# Save
saveRDS(oddsportal.teams, './01 Data/oddsportal teams 10-06-2018.rds')

# ---------- Betfair ----------

matches <- listMarketCatalogue(eventTypeIds = c("1"),
                               fromDate = format(as.POSIXct(paste(Sys.Date(), "00:00"),
                                                            format="%Y-%m-%d %H:%M"), "%Y-%m-%dT%TZ"),
                               toDate = format(as.POSIXct(paste(Sys.Date(), "23:59"),
                                                          format="%Y-%m-%d %H:%M"), "%Y-%m-%dT%TZ"),
                               maxResults = "200",
                               marketTypeCodes = c("MATCH_ODDS"))

# Get relevant betfair variables
matches.2 <- data.frame(cbind(matches[, c("marketId",
                                          "totalMatched")],
                              comp.name = matches$competition$name,
                              event.name = matches$event$name,
                              matches$event$timezone,
                              matches$marketStartTime)
) %>% mutate(event.name = trimws(gsub("\\s+", " ", event.name)))

# Add home and away team names
matches.3 <- lapply(matches$runners, function(runner) {
  data.frame(team.h.id = runner$selectionId[1],
             team.h = runner$runnerName[1],
             team.a.id = runner$selectionId[2],
             team.a = runner$runnerName[2],
             draw.id = runner$selectionId[3])
}) %>% rbindlist %>%
  cbind(matches.2)

betfair.teams <- matches.3 %>%
  mutate(time = substr(matches.marketStartTime, 1, 16)) %>%
  mutate(time = gsub("T", " ", time)) %>%
  select(time, marketId, h.team=team.h, a.team=team.a)

# Save
saveRDS(betfair.teams, './01 Data/betfair teams 10-06-2018.rds')


# ---- Fastlinking -----

betfair.teams <- readRDS('./01 Data/betfair teams 10-06-2018.rds')
oddsportal.teams <- readRDS('./01 Data/oddsportal teams 10-06-2018.rds')

# Make into data frames. Need two variables for fastlink to work
left <- oddsportal.teams
right <- betfair.teams

# Simple join as benchmark
result <- left %>% inner_join(right, by = c("h.team", "a.team"))

nrow(result)

# Link to FPL data
matches.out <- fastLink(
  dfA = left,
  dfB = right, 
  varnames = c("h.team", "a.team", "time"),
  stringdist.match = c("h.team", "a.team"), # Specifies the variables you want to treat as strings for fuzzy matching
  #partial.match = c("team.1", "team.2"), # Specifes variables where you want the algorithm to check for partial matches
  verbose = TRUE,
  #cut.a = 0.8,
  #cut.p = 0.6,
  return.all = TRUE
)

# Gives the match rate, estimated false positive rate (FDR) and estimated false negative rate (FNR)
summary(matches.out)

# Extracts the matched data
a <- matches.out$matches$inds.a
b <- matches.out$matches$inds.b

# Compile matched data
left.result <- left[a,] %>% mutate(index = b)
right.result <- right[b,] %>% mutate(index = b,
                                     "match"=matches.out$posterior)

matched.data <- inner_join(left.result,
                           right.result,
                           by="index") %>%
  select(marketId, time.x, h.team.x, a.team.x, h.team.y, a.team.y, match) %>%
  filter(match > 0.1)

matched.data.2 <- matched.data %>%
  select(h.team=h.team.x,
         a.team=a.team.x,
         marketId)

saveRDS(matched.data.2, './03 Bots/admin/oddsportal-betfair-lookup.rds')

