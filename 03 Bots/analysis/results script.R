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

st <- list()
st[[1]] <-   abettor::getAccountStatement(fromRecordValue = 0) %>%
  select(refId, itemDate, balance, amount)

# Get all statements
i <- 1
while(nrow(st[[i]]) %% 100 == 0) {
  i <<- i + 1
  st[[i]] <- abettor::getAccountStatement(fromRecordValue = (i-1) * 100) %>%
    select(refId, itemDate, balance, amount)
}

st <- rbindlist(st) %>% unique


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

# By day
st.2 %>%
  filter(refId != 0) %>%
  mutate(date = as.Date(itemDate)) %>%
  group_by(date) %>%
  summarise(amount = sum(amount)) %>%
  ungroup %>%
  mutate(balance = cumsum(amount),
         index = as.factor(row_number(date))) %>%
  ggplot(aes(x = index, y = balance)) +
  geom_bar(colour = "dodgerblue4", fill = "dodgerblue", stat = "identity") +
  theme_minimal() +
  expand_limits(y = 0)

# Line
st.2 %>%
  filter(refId != 0) %>%
  mutate(date = as.Date(itemDate)) %>%
  group_by(date) %>%
  summarise(amount = sum(amount)) %>%
  ungroup %>%
  mutate(balance = cumsum(amount),
         index = row_number(date)) %>%
  ggplot(aes(x = index, y = balance)) +
  geom_line(colour = "dodgerblue4") +
  theme_minimal() +
  expand_limits(y = 0)


# Smooth
st.3 <- st.2 %>%
  filter(refId != 0) %>%
  mutate(date = as.Date(itemDate)) %>%
  group_by(date) %>%
  summarise(amount = sum(amount)) %>%
  ungroup %>%
  mutate(balance = cumsum(amount),
         index = row_number(date))

st.3 %>%
  ggplot(aes(x = index, y = balance)) +
  geom_point(colour = "dodgerblue4") +
  geom_smooth(se = F, method = "lm") +
  theme_minimal() +
  expand_limits(y = 0)

# Create projection model
model <- lm(balance ~ index, data = st.3)
summary(model)

# Forecast balance
predict(model, newdata = data.frame(index=30))

# Profits per month
st.2 %>%
  filter(refId != 0) %>%
  mutate(date = as.Date(itemDate)) %>%
  group_by(date) %>%
  summarise(amount = sum(amount)) %>%
  ungroup %>%
  #mutate(index = as.factor(row_number(date))) %>%
  mutate(index = as.factor(floor(row_number(date)/31) + 1)) %>%
  group_by(index) %>%
  summarise(amount = sum(amount)) %>%
  ggplot(aes(x = index, y = amount)) +
  geom_bar(colour = "dodgerblue4", fill = "dodgerblue", stat = "identity") +
  theme_minimal() +
  expand_limits(y = 0)


