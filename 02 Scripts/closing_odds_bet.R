# Odds data

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

data.raw <- fread('./01 Data/closing_odds.csv')

names(data.raw) <- c("id", "comp", "date", "home", "home_score", "away", "away_score",
                     "home.odds.m", "draw.odds.m", "away.odds.m",
                     "home.odds.max", "draw.odds.max", "away.odds.max",
                     "bookie.h", "bookie.x", "bookie.a",
                     "bs.h", "bs.x", "bs.a")

# Calculate 'true' probabilities and expected value (taking commission into account)
#alpha <- 0.05
data.2 <- data.raw %>%
  mutate(result = case_when(home_score > away_score ~ "h",
                            away_score > home_score ~ "a",
                            TRUE ~ "x")) %>%
  filter(bs.a > 3, bs.h > 3, bs.x > 3) %>%
  select(id:away.odds.max, result) %>%
  melt(id.vars = c("id",
                   "comp",
                   "date",
                   "home",
                   "home_score",
                   "away",
                   "away_score",
                   "result",
                   "home.odds.m",
                   "draw.odds.m",
                   "away.odds.m")) %>%
  mutate(bet = case_when(variable == "home.odds.max" ~ "h",
                         variable == "draw.odds.max" ~ "x",
                         variable == "away.odds.max" ~ "a"),
         mean.odds = case_when(variable == "home.odds.max" ~ home.odds.m,
                               variable == "draw.odds.max" ~ draw.odds.m,
                               variable == "away.odds.max" ~ away.odds.m)) %>%
  select(id:result, bet, mean.odds, max.odds = value) %>%
  mutate(imp.p = 1/mean.odds,
         alpha = case_when(bet == "x" ~ 0.05,
                           TRUE ~ 0.05),
         slope = case_when(bet == "x" ~ 1.081,
                           bet == "a" ~ 1.012,
                           bet == "h" ~ 1.003)) %>%
  mutate(p = round((1/mean.odds) - alpha, 2)) %>%
  select(-slope) %>%
  mutate(xv = p*0.95*(max.odds-1) - (1-p)) %>%
  mutate(xv = round(xv, 4)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# How many of these bets are profitable?
target <- 0
100*sum(data.2$xv > target)/nrow(data.2)

# Less than 2% if alpha = 0.05
# This is going to be a maximum of 2 bets per day on betfair, and therefore isn't workable.
# At alpha = 0.01, this is 57%.
# At alpha = 0.02, 28%
# At alpha = 0.03, 11%
# At alpha = 0.04, 4.45%
# At alpha = intercepts from study, depending on bet, this is 6.8%

# How profitable is it if you set alpha to be a certain value and pick one result per game? (Assuming stake = 1)
# Also randomly select a subset of them, representing those you can match to betfair
matchrate <- 1
target <- 0
bets.data <- data.2 %>%
  filter(xv > target) %>%
  group_by(id) %>%
  mutate(rank = row_number(desc(xv))) %>%
  ungroup %>%
  filter(rank == 1) %>%
  sample_frac(matchrate)

bets.data %>%
  mutate(p = round(p, 2)) %>%
  group_by(p) %>%
  summarise(true.pos = sum(bet==result),
            num = n(),
            pct = true.pos/num) %>%
  ggplot(aes(x=p, y=pct)) +
  geom_point(colour = "dodgerblue4") +
  theme_minimal() +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1)

# How many bets per day is this?
bets.data %>% group_by(date) %>% summarise(num = n()) %>% ungroup %>% select(num) %>% unlist %>% mean

# Get profit and balance over time.
bets.data.2 <- bets.data %>%
  arrange(date) %>%
  mutate(profit = ifelse(result == bet, 0.95*(max.odds - 1), -1)) %>%
  mutate(balance = cumsum(profit))

# plot balance over time
bets.data.2 %>%
  ggplot(aes(x=date, y=balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal()

# alpha = 0.05 is highly profitable, over the time period. alpha = 0.02 loses you a lot of money!
monthly <- bets.data.2 %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarise(profit = sum(profit),
            bets = n()) %>%
  ungroup %>%
  mutate(date = paste(year, month, "01", sep = "-")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(balance = cumsum(profit))

monthly %>%
  ggplot(aes(x=date, y=balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal()


# ------ Changing stake simulation -----
startbalance <- 50
bets.data.3 <- bets.data %>%
  arrange(date) %>%
  mutate(profit = ifelse(result == bet, 0.95*(max.odds - 1), -1)) %>%
  select(-rank) %>%
  group_by(date) %>%
  summarise(profit = sum(profit),
            bets = n())

mean(bets.data.3$bets)
hist(bets.data.3$bets)

# Get changing stake and balance over time. Minimum of 2, maximum of 50, otherwise balance / 20.
# Withdraw 50% of profits each month.
prev <- NA
balance <- startbalance
totprofs <- 0
result <- lapply(1:nrow(bets.data.3), function(i) {
  
  if(is.na(prev)) {
    stake <- 2
    withdraw <- 0
  } else {
    stake <- balance/20
    stake = case_when(stake < 2 ~ 2,
                      stake > 50 ~ 50,
                      TRUE ~ stake)
    
  }
  
  # Get profit
  profit.2 <- stake * bets.data.3$profit[i]
  
  # If it's a new month, withdraw profits
  if(is.na(prev)) {
    
    # Profits in first day
    totprofs <<- profit.2
    
  } else if(month(bets.data.3$date[i]) != month(bets.data.3$date[i-1])) {
    
    # Profits in previous month
    withdraw <- ifelse(totprofs > 0, totprofs * 0.5, 0)
    totprofs <<- 0
    
  } else {
    withdraw <- 0
    totprofs <<- totprofs + profit.2
  }
  
  # Record previous
  prev <<- stake
  
  # Update balance
  balance <<- balance + profit.2 - withdraw
  
  #if(i %% 10 == 0) print(paste(i))
  
  # Return data
  return(data.frame(stake, profit.2, withdraw, balance))
  
})

result.2 <- rbindlist(result)

# Add back onto odds data
bets.data.4 <- bets.data.3 %>%
  cbind(result.2)

# How does stake and balance change over time?
bets.data.4 %>%
  ggplot(aes(x=date, y=stake)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal()

# Balance over time - after withdrawals
bets.data.4 %>%
  ggplot(aes(x=date, y=balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal()

# Monthly?
monthly.sim <- bets.data.4 %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  mutate(rank = row_number(desc(date)),
         profit.3 = sum(profit.2)) %>%
  filter(rank == 1) %>%
  select(year, month, date, stake, profit.3, balance) %>%
  ungroup %>%
  mutate(var = row_number(date))

monthly.sim %>%
  ggplot(aes(x=var, y=balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal()

# Get withdrawal data
withdraw.sim <- bets.data.4 %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  mutate(rank = row_number(date),
         bets = sum(bets),
         stake = mean(stake),
         profit = sum(profit.2)) %>%
  filter(rank == 1) %>%
  ungroup %>%
  select(month, year, date, bets, stake, profit, withdraw, balance)

# How much can you withdraw per month on average?
withdraw.sim
mean(withdraw.sim$withdraw)

# Plots
withdraw.sim %>%
  ggplot(aes(x=date, y = balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal()

withdraw.sim %>%
  mutate(bal = cumsum(withdraw)) %>%
  ggplot(aes(x=date, y = bal)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal()

# ----- Run several times with different random bets, to see how affected by matchrate ----

simulation <- lapply(1:50, function(x) {
  
  # Random sample
  bets.data <- data.2 %>%
    filter(xv > target) %>%
    group_by(id) %>%
    mutate(rank = row_number(desc(xv))) %>%
    ungroup %>%
    filter(rank == 1) %>%
    sample_frac(matchrate)
  
  startbalance <- 50
  bets.data.3 <- bets.data %>%
    arrange(date) %>%
    mutate(profit = ifelse(result == bet, 0.95*(max.odds - 1), -1)) %>%
    select(-rank) %>%
    group_by(date) %>%
    summarise(profit = sum(profit),
              bets = n())
  
  # Get changing stake and balance over time. Minimum of 2, maximum of 50, otherwise balance / 20.
  # Withdraw 50% of profits each month.
  prev <<- NA
  balance <<- startbalance
  totprofs <<- 0
  result <- lapply(1:nrow(bets.data.3), function(i) {
    
    if(is.na(prev)) {
      stake <- 2
      withdraw <- 0
    } else {
      stake <- balance/20
      stake = case_when(stake < 2 ~ 2,
                        stake > 50 ~ 50,
                        TRUE ~ stake)
      
    }
    
    # Get profit
    profit.2 <- stake * bets.data.3$profit[i]
    
    # If it's a new month, withdraw profits
    if(is.na(prev)) {
      
      # Profits in first day
      totprofs <<- profit.2
      
    } else if(month(bets.data.3$date[i]) != month(bets.data.3$date[i-1])) {
      
      # Profits in previous month
      withdraw <- ifelse(totprofs > 0, totprofs * 0.5, 0)
      totprofs <<- 0
      
    } else {
      withdraw <- 0
      totprofs <<- totprofs + profit.2
    }
    
    # Record previous
    prev <<- stake
    
    # Update balance
    balance <<- balance + profit.2 - withdraw
    
    #if(i %% 10 == 0) print(paste(i))
    
    # Return data
    return(data.frame(stake, profit.2, withdraw, balance))
    
  })
  
  result.2 <- rbindlist(result)
  
  # Add back onto odds data
  bets.data.4 <- bets.data.3 %>%
    cbind(result.2)
  
  # Get withdrawal data
  withdraw.sim <- bets.data.4 %>%
    mutate(month = month(date),
           year = year(date)) %>%
    group_by(year, month) %>%
    mutate(rank = row_number(date),
           bets = sum(bets),
           stake = mean(stake),
           profit = sum(profit.2)) %>%
    filter(rank == 1) %>%
    ungroup %>%
    select(month, year, date, bets, stake, profit, withdraw, balance)
  
  print(paste(x))
  
  # How much can you withdraw per month on average?
  return(data.frame(mean.withdraw = mean(withdraw.sim$withdraw),
                    sd.withdraw = sd(withdraw.sim$withdraw),
                    max.withdraw = max(withdraw.sim$withdraw)))
})

simulation.2 <- rbindlist(simulation)

# How does the profit vary?
simulation.2
mean(simulation.2$mean.withdraw)

hist(simulation.2$mean.withdraw)
hist(simulation.2$max.withdraw)

# ---- Check accuracy of odds ----

bets.data %>%
  mutate(p = round(p, 1)) %>%
  group_by(p) %>%
  summarise(true.pos = sum(bet==result),
            num = n(),
            pct = true.pos/num) %>%
  ggplot(aes(x=p, y=pct)) +
  geom_point(colour = "dodgerblue4") +
  theme_minimal() +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1)

# At this level, they are highly accurate, even with this being a random selection of bets.