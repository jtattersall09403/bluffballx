library(betScrapeR)
library(abettor)
library(dplyr)
library(data.table)
library(ggplot2)

# ---- Data ----

# Get login details
key.file <- readLines('Key')

# Log in
loginBF(key.file[1],key.file[2],key.file[3])

# Get current bets
current_bets <- readRDS('./03 Bots/data/current_bets.rds') %>%
  mutate(xv.abs = xv*liability)

# Get results to date
st <- abettor::getAccountStatement()

# Format
st.2 <- st %>%
  select(refId, itemDate, balance, amount) %>%
  mutate(itemDate = as.POSIXct(itemDate, format = "%Y-%m-%dT%H:%M:%S")) %>%
  arrange(itemDate) %>%
  mutate(index = row_number())

# Value so far
st.2 %>%
  filter(refId != 0) %>%
  mutate(balance = cumsum(amount)) %>%
  ggplot(aes(x = index, y = balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal() +
  expand_limits(y = 0)


# Get projection
proj <- current_bets %>%
  select(kickoff, xv.abs) %>%
  mutate(itemDate = max(current_bets$kickoff)) %>%
  group_by(itemDate) %>%
  summarise(balance = sum(xv.abs) + st.2$balance[nrow(st.2)])
proj

outcomes <- current_bets %>%
  mutate(win = ifelse(back.lay == "lay", 
                      stake,
                      price * stake))
outcomes

# Get all possible combinations of outcomes
n <- nrow(outcomes)
l <- rep(list(c(-1, 1)), n)
combos <- expand.grid(l)

probs <- apply(combos, 1, function(i) {
  outcomes %>%
    mutate(p = ifelse(back.lay == "back", true.p, true.p.not)) %>%
    select(back.lay, p, win, liability) %>%
    mutate(result = as.numeric(i),
           winnings = ifelse(result > 0, win, -1*liability))%>%
    mutate(p = ifelse(result == -1, 1- p, p)) %>%
    summarise(p = prod(p), winnings = sum(winnings))
}) %>% rbindlist %>% arrange(winnings) %>% mutate(cum.prob = cumsum(p))

probs

# Probability of winning more than 0
sum(probs$p[probs$winnings > 0])

# Expected value of portfolio
sum(probs$p * probs$winnings)

# Histogram
probs %>%
  mutate(w_bin = cut(winnings, breaks = seq(min(winnings)-1,
                                            max(winnings)+1,
                                            by = 5))) %>%
  group_by(w_bin) %>%
  summarise(p = sum(p)) %>%
  ggplot(aes(x = w_bin, y = p)) +
  geom_bar(stat = "identity", colour = "dodgerblue4", fill = "dodgerblue") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

# ---- Plots ----

# line
st.2 %>%
  filter(refId != 0) %>%
  mutate(balance = cumsum(amount)) %>%
  ggplot(aes(x = index, y = balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal() +
  expand_limits(y = 0)

# Bar
st %>%
  select(itemDate, balance, amount) %>%
  mutate(itemDate = as.POSIXct(itemDate, format = "%Y-%m-%dT%H")) %>%
  arrange(itemDate) %>%
  group_by(itemDate) %>%
  summarise(balance = max(balance)) %>%
  ungroup %>%
  slice(2:nrow(st)) %>%
  ggplot(aes(x = itemDate, y = balance)) +
  geom_bar(colour = "dodgerblue4", fill = "dodgerblue", stat = "identity") +
  theme_minimal() +
  expand_limits(y = 0)

# Smooth
st %>%
  select(itemDate, balance, amount) %>%
  mutate(itemDate = as.POSIXct(itemDate, format = "%Y-%m-%dT%H")) %>%
  arrange(itemDate) %>%
  group_by(itemDate) %>%
  summarise(balance = max(balance)) %>%
  ungroup %>%
  slice(2:nrow(st)) %>%
  ggplot(aes(x = itemDate, y = balance)) +
  geom_point(colour = "dodgerblue4") +
  theme_minimal() +
  geom_smooth(se = FALSE, method = "loess") +
  expand_limits(y = 0)


