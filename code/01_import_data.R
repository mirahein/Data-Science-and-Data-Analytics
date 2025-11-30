## Install and load required packages --------------------------------------

install.packages(c("quantmod", "tidyverse"), dependencies = TRUE)

library(quantmod)   # for downloading financial data
library(tidyverse)  # for data wrangling (dplyr, readr, tibble etc.)

## Download Tesla stock price data -----------------------------------------

start_date <- "2020-01-01"

# Download TSLA data from Yahoo Finance, starting at start_date
getSymbols("TSLA", src = "yahoo", from = start_date)

# Convert TSLA xts object to a tibble with date and closing price
tesla_df <- tibble(
  date  = index(TSLA),              # extract the date index
  close = as.numeric(TSLA$TSLA.Close) # convert closing price to numeric
) |>
  arrange(date) |>                  # make sure rows are ordered by date
  mutate(
    # compute daily returns: (P_t / P_{t-1}) - 1
    return = (close / lag(close)) - 1
  )

head(tesla_df)  # quick check of the resulting data frame

## Load tweet data and inspect structure ----------------------------------

# Read raw tweet data (assumed to be in a CSV file)
tweets_raw <- read_csv("data/elon_tweets.csv")

# Inspect column names to understand structure
colnames(tweets_raw)

## First version of tweet cleaning (kept for reference) --------------------

tweets <- tweets_raw |>
  mutate(
    # convert Datetime column to Date (drop time of day)
    date = as.Date(Datetime)
  ) |>
  # rename Text to content and keep sentiment as is for now
  select(date, content = Text, sentiment)

head(tweets)

library(stringr)   # string manipulation (also loaded via tidyverse, but explicit here)

# Clean up sentiment labels by removing brackets and quotes
tweets <- tweets |>
  mutate(
    sentiment = str_replace_all(sentiment, "\\[|'|\\]", "")
  )

head(tweets)
unique(tweets$sentiment)  # check which sentiment categories exist

## Aggregate tweets by day -------------------------------------------------

tweets_daily <- tweets |>
  group_by(date) |>
  summarise(
    tweet_count = n(),                            # number of tweets per day
    pos = sum(sentiment == "positive"),          # count of positive tweets
    neu = sum(sentiment == "neutral"),           # count of neutral tweets
    neg = sum(sentiment == "negative"),          # count of negative tweets
    .groups = "drop"
  )

head(tweets_daily)

## Merge Tesla returns with tweet data (first approach) --------------------

data_merged <- tesla_df |>
  left_join(tweets_daily, by = "date") |>        # merge by date
  mutate(
    # replace missing tweet counts with zero (days without tweets)
    tweet_count = replace_na(tweet_count, 0),
    pos         = replace_na(pos, 0),
    neu         = replace_na(neu, 0),
    neg         = replace_na(neg, 0),
    # binary indicator: 1 if at least one tweet that day, else 0
    tweet_day   = if_else(tweet_count > 0, 1L, 0L)
  )

head(data_merged)

## Alternative (equivalent) aggregation and merge --------------------------

tweets_daily <- tweets %>%
  group_by(date) %>%
  summarise(
    tweet_count = n(),
    pos = sum(sentiment == "positive"),
    neu = sum(sentiment == "neutral"),
    neg = sum(sentiment == "negative"),
    .groups = "drop"
  )

data_merged <- tesla_df %>%
  left_join(tweets_daily, by = "date") %>%
  # here we use replace_na() with a named list to fill NAs
  replace_na(list(
    tweet_count = 0,
    pos = 0,
    neu = 0,
    neg = 0
  )) %>%
  mutate(tweet_day = if_else(tweet_count > 0, 1L, 0L))

head(data_merged)

## Check date range of tweets ---------------------------------------------

range(tweets$date)  # useful to see overlap between tweets and price data

## Clean tweets again with simpler sentiment extraction --------------------
## (This block overwrites the previous tweets object with a cleaner version)

library(tidyverse)
library(stringr)

tweets <- tweets_raw |>
  mutate(
    # convert Datetime to Date
    date = as.Date(Datetime),
    # extract ONLY the words "positive", "neutral", or "negative"
    sentiment = str_extract(sentiment, "positive|neutral|negative")
  ) |>
  select(date, content = Text, sentiment)

head(tweets)
unique(tweets$sentiment)   # verify that only the three categories remain

## Re-aggregate tweets by day with cleaned sentiment ----------------------

unique(tweets$sentiment)

tweets_daily <- tweets |>
  group_by(date) |>
  summarise(
    tweet_count = n(),                      # total tweets per day
    pos = sum(sentiment == "positive"),    # positive tweets per day
    neu = sum(sentiment == "neutral"),     # neutral tweets per day
    neg = sum(sentiment == "negative"),    # negative tweets per day
    .groups = "drop"
  )

head(tweets_daily)

## Final merge: Tesla data + daily tweet statistics -----------------------

data_merged <- tesla_df |>
  left_join(tweets_daily, by = "date") |>
  # fill missing tweet stats with zero on non-tweet days
  replace_na(list(
    tweet_count = 0,
    pos = 0,
    neu = 0,
    neg = 0
  )) |>
  # dummy variable = 1 if there was at least one tweet, else 0
  mutate(tweet_day = if_else(tweet_count > 0, 1L, 0L))

head(data_merged)

## Equivalent merge written slightly differently --------------------------

data_merged <- tesla_df |>
  left_join(tweets_daily, by = "date") |>
  mutate(
    tweet_count = replace_na(tweet_count, 0),
    pos         = replace_na(pos, 0),
    neu         = replace_na(neu, 0),
    neg         = replace_na(neg, 0),
    tweet_day   = if_else(tweet_count > 0, 1L, 0L)
  )

head(data_merged)

## Compare days with vs. without tweets -----------------------------------

data_merged |>
  group_by(tweet_day) |>
  summarise(
    n_days   = n(),                          # number of days in each group
    mean_ret = mean(return, na.rm = TRUE),   # average daily return
    sd_ret   = sd(return, na.rm = TRUE)      # volatility of returns
  )

## Optional: statistical test and visualisation (currently commented out) --

# Test whether average returns differ between tweet days and non-tweet days
# t.test(return ~ tweet_day, data = data_merged)

# Boxplot of daily returns for tweet vs non-tweet days
# ggplot(data_merged, aes(x = factor(tweet_day), y = return)) +
#   geom_boxplot() +
#   labs(x = "Tweet day (0 = no tweet, 1 = tweet)",
#        y = "Daily return")

## Save merged dataset to CSV for later analysis --------------------------

write_csv(data_merged, "data/daily_merged.csv")
