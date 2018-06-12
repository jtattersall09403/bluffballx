library(FLSSS)
library(data.table)
library(microbenchmark)

# Create toy data
p <- rbeta(10, 2, 5)
p <- sort(p, decreasing = TRUE)
op <- 1/p
dif <- seq(0.1, 1, by = 0.1)*op
bf <- op + dif

plot(sort(op))
  
hist(p)
hist(bf)

xv <- p*(bf - 1) - (1-p)
plot(p, bf)
plot(p, xv)

newopps <- data.frame(p, op, bf, xv, dif)
newopps


# Calculate probability of losing all bets
dat <- arrange(newopps, desc(xv)) %>%
  mutate(ploss = 1- p) %>%
  sample_n(9)

balance <- 10
stake <- 2
min <- 0.2
prod <- 1 # This should be 1 if no current bets, or the probability of losing every current bets otherwise (i.e. prod(1-current_bets$p))

get_optimal <- function(dat, balance, stake, min, prod) {
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
    return(res.3)
  } else {
    return(dat[dat$ploss < 0, ])
  }

}

result <- get_optimal(dat, balance, stake, min, prod)
result

sum(result$xv)/nrow(result)
prod(result$ploss)

# Check how fast this is. Appears to take a maximum of 0.06 seconds, so this is probably fine.
microbenchmark(get_optimal(dat, 10, 2, 0.1, 1), times = 100L)
