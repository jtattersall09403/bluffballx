

horseScraper=function(race, suppress = FALSE, numAttempts = 5, sleepTime = 0){
  
  if(is.null(race$event$venue)|
     is.null(race$event$countryCode)|
     is.null(race$marketId)|
     is.null(race$marketStartTime)|
     is.null(race$runners)) return(data.frame(error="Insufficient race data"))
  
  race.time=as.POSIXct(race$marketStartTime,
                       format="%Y-%m-%dT%H:%M",
                       "Europe/London")
  
  race.time=race.time+ 60*60*(format(race.time,format="%Z")=="BST")
  
  if(difftime(race.time, format(Sys.time(), tz="Europe/London",usetz=TRUE))<0.25 & suppress == FALSE){
    warning("Race starts in less than 15 minutes. Be careful. Prices may fluctuate quickly!")
  }
  
  betfair.horses <- race$runners[[1]]$runnerName
  
  betfair.info <- abettor::listMarketBook(marketIds=race$marketId, priceData = "EX_BEST_OFFERS")
  
  if(length(betfair.info)==0)
    return(data.frame(error="No market data returned. Invalid market ID and/or session token expired?"))
  if(betfair.info$status=="CLOSED"){
    return(data.frame(error="That market is closed",marketId=race$marketId))
  }
  
  if(!is.null(betfair.info$message)){
    return(data.frame(data.frame(error="listMarketBook error"),betfair.info))}
  
  betfair.horses <- betfair.horses[match(betfair.info$runners[[1]]$selectionId,race$runners[[1]]$selectionId)]
  runners <- which(betfair.info$runners[[1]]$status=="ACTIVE")
  betfair.horses <- betfair.horses[which(betfair.info$runners[[1]]$status=="ACTIVE")]
  
  if(any(grepl("[0-9]",betfair.horses))){
    betfair.horses <- gsub("^[^ ]* ","",betfair.horses)
  }
  betfair.horses <- gsub("[[:punct:]]", "", betfair.horses)
  betfair.back <- unlist(lapply(betfair.info$runners[[1]]$ex$availableToBack[runners],function(x){if(length(x)==0){data.frame(price=NA,size=NA)}else{as.data.frame(x)[1,]}}), use.names = FALSE)
  betfair.lay <- unlist(lapply(betfair.info$runners[[1]]$ex$availableToLay[runners],function(x){if(length(x)==0){data.frame(price=NA,size=NA)}else{as.data.frame(x)[1,]}}), use.names = FALSE)
  betfair.prices <- rbind(betfair.info$runners[[1]]$selectionId[runners],as.data.frame(matrix(betfair.back,2,length(betfair.horses))),
                          as.data.frame(matrix(betfair.lay,2,length(betfair.horses))))
  colnames(betfair.prices) <- betfair.horses
  row.names(betfair.prices) <- c("Selection ID","Back Price","Back Size","Lay Price","Lay Size")
  
  if( race$event$countryCode=="GB" |  race$event$countryCode == "IE"){
    if(as.Date(race.time)==as.Date(format(as.POSIXct(Sys.time(),format="%Y-%m-%dT%H:%M","UTC"),tz="Europe/London"))){
      page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/",formatVenue(race$event$venue),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)
    }else{
      page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/",substring(race$marketStartTime,1,10),"-",gsub(" ","-",tolower(race$event$venue)),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)}
  }else if(race$event$countryCode=="FR"|race$event$countryCode=="DE"){
    if(as.Date(race.time)==as.Date(format(as.POSIXct(Sys.time(),format="%Y-%m-%dT%H:%M","UTC"),tz="Europe/London"))){
      page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/europe/",formatVenue(race$event$venue),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)
    }else{
      page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/europe/",substring(race$marketStartTime,1,10),"-",formatVenue(race$event$venue),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)}
  }else if(race$event$countryCode=="US"|race$event$countryCode=="CL"){
    if(as.Date(format(as.POSIXct(Sys.time(),format="%Y-%m-%dT%H:%M","UTC"),tz=race$event$timezone))==as.Date(format(as.POSIXct(Sys.time(),format="%Y-%m-%dT%H:%M","UTC"),tz="Europe/London"))){
      page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/americas/",formatVenue(race$event$venue),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)
    }else{page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/americas/",substring(race$marketStartTime,1,10),"-",formatVenue(race$event$venue),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)}
  }else if(race$event$countryCode=="ZA"|race$event$countryCode=="SG"){
    if(as.Date(race.time)==as.Date(format(as.POSIXct(Sys.time(),format="%Y-%m-%dT%H:%M","UTC"),tz="Europe/London"))){
      page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/world/",formatVenue(race$event$venue),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)
    }else{page <- scrapePage(paste0("http://www.oddschecker.com/horse-racing/world/",substring(race$marketStartTime,1,10),"-",formatVenue(race$event$venue),"/",substring(race.time,12,16),"/winner"),numAttempts,sleepTime)}
  }else{return(data.frame(error="Country not covered by OddsChecker"))}
  
  if(is.data.frame(page))
    return(page)
  bookies <- rvest::html_nodes(page,".eventTableHeader .bk-logo-click") %>%  rvest::html_attr(name = "title")
  if(length(bookies) == 0){
    return(data.frame(error="No racing data scraped- Is that race covered by OddsChecker?"))
  }
  horse <- rvest::html_nodes(page, ".bc .selTxt") %>% html_attr("data-name")
  if(length(horse) == 1){
    horse <- betfair.horses
    odds <- rep(0,length(betfair.horses)*length(bookies))
  }else{horse <- gsub("[[:punct:]]", "", horse)
  if(length(intersect(horse,betfair.horses))==0)
    return(data.frame(error="Events don't match up- No horses in common"))
  horse <- betfair.horses[partialMatch(tolower(horse[!grepl(" NR",horse)]),tolower(betfair.horses))]
  odds <- rvest::html_nodes(page,".bc .np , .bs") %>% rvest::html_text() %>% sapply(fracToDec)}
  
  checker <- as.data.frame(matrix(odds,length(bookies),length(horse)))
  colnames(checker) <- horse
  rownames(checker) <- bookies
  # Remove the exchanges
  checker <- checker[!bookies %in% c("Betfair", "Betdaq", "Matchbook", "Betfair Exchange"),]
  nr.horses=betfair.horses[!betfair.horses %in% horse]
  if(length(nr.horses)!=0){
    checker[,(length(checker)+1):(length(checker)+length(nr.horses))] <- -101
    colnames(checker)[(length(checker)-length(nr.horses)+1):length(checker)] <- nr.horses
  }
  return(rbind(betfair.prices, checker[,names(betfair.prices)]))
}