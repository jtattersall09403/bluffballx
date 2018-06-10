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
                           bs=bookies) %>%
    cbind(odds)
  
  # Split out teams
  teams <- strsplit(oddsportal$game, " - ") %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    mutate_all(trimws)
  
  # Add to data
  oddsportal.3 <- oddsportal %>%
    mutate(h.team = teams$V1,
           a.team = teams$V2) %>%
    select(game, h.team, a.team)
  
  return(oddsportal.3)
}

url <- 'http://www.oddsportal.com/matches/soccer/20180609/'

# Get teams
oddsportal <- getTeams(url)

# Append into single variable
oddsportal.teams <- append(oddsportal$h.team,
                           oddsportal$a.team) %>% unique

# Save
saveRDS(oddsportal.teams, './01 Data/oddsportal teams 09-06-2018.rds')

# ---------- Betfair ----------

matches <- listMarketCatalogue(eventTypeIds = c("1"),
                    fromDate = (format(Sys.time()-60*60*24, "%Y-%m-%dT%TZ")),
                    toDate = (format(Sys.time()+60*60*24, "%Y-%m-%dT%TZ")),
                    maxResults = "200",
                    marketTypeCodes = c("MATCH_ODDS"))

# Get relevant betfair variables
matches.2 <- data.frame(cbind(matches[, c("marketId",
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

betfair.teams <- append(matches.3$team.h, matches.3$team.a) %>% 
  cbind(append(matches.3$team.h.id, matches.3$team.a.id)) %>%
  unique

# Save
saveRDS(betfair.teams, './01 Data/betfair teams 09-06-2018.rds')
