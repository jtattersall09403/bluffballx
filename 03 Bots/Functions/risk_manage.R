library(data.table)

get_optimal <- function(dat, balance, stake, min, prod) {
  
  # Calculate probability of losing all bets
  dat <- arrange(dat, desc(xv)) %>%
    mutate(ploss = 1 - 1/(value - 0.05))
  
  # Always pick as many as you can afford (as this minimises ploss)
  # When you can't afford all of them, pick as many as you can, such that ploss < arbitrary min, and xv is maximised
  n = balance/stake
  combos = combn(nrow(dat), n)
  res = apply(combos, 2, function(i) {
    dat[i, ]
  })
  
  # filter according to ploss
  prods = sapply(res, function(x) prod(x$ploss)) * prod
  res.2 = res[prods < min]
  
  if(length(res.2) > 0) {
    # Get result with maximum xv that satisfy the minimum ploss requirement
    xvs = sapply(res.2, function(x) sum(x$xv)/nrow(x))
    res.3 = res.2[[which(xvs == max(xvs))]]
    return(select(res.3, -ploss))
  } else {
    return(dat[dat$ploss < 0, ] %>% select(-ploss))
  }

}

