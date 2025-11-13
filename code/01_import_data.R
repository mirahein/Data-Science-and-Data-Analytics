install.packages(c("quantmod", "tidyverse"), dependencies = TRUE)
library(quantmod)
library(tidyverse)
start_date <- "2020-01-01"
getSymbols("TSLA", src = "yahoo", from = start_date)
tesla_df <- tibble(
  date  = index(TSLA),
  close = as.numeric(TSLA$TSLA.Close)
) |>
  arrange(date) |>
  mutate(
    return = (close / lag(close)) - 1
  )
head(tesla_df)
tweets_raw <- read_csv("data/elon_tweets.csv")
colnames(tweets_raw)

tweets <- tweets_raw |>
  mutate(
    date = as.Date(Datetime)  
  ) |>
  select(date, content = Text, sentiment) 
head(tweets)
library(stringr)   # comes with tidyverse, but we load it explicitly

tweets <- tweets |>
  mutate(
    sentiment = str_replace_all(sentiment, "\\[|'|\\]", "")
  )
head(tweets)
unique(tweets$sentiment)
tweets_daily <- tweets |>
  group_by(date) |>
  summarise(
    tweet_count = n(),
    pos = sum(sentiment == "positive"),
    neu = sum(sentiment == "neutral"),
    neg = sum(sentiment == "negative"),
    .groups = "drop"
  )
head(tweets_daily)
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
  replace_na(list(
    tweet_count = 0,
    pos = 0,
    neu = 0,
    neg = 0
  )) %>%
  mutate(tweet_day = if_else(tweet_count > 0, 1L, 0L))
head(data_merged)
range(tweets$date)
library(tidyverse)
library(stringr)

tweets <- tweets_raw |>
  mutate(
    date = as.Date(Datetime),
    # keep ONLY the word positive / neutral / negative
    sentiment = str_extract(sentiment, "positive|neutral|negative")
  ) |>
  select(date, content = Text, sentiment)

head(tweets)
unique(tweets$sentiment)
unique(tweets$sentiment)
tweets_daily <- tweets |>
  group_by(date) |>
  summarise(
    tweet_count = n(),
    pos = sum(sentiment == "positive"),
    neu = sum(sentiment == "neutral"),
    neg = sum(sentiment == "negative"),
    .groups = "drop"
  )

head(tweets_daily)
data_merged <- tesla_df |>
  left_join(tweets_daily, by = "date") |>
  replace_na(list(
    tweet_count = 0,
    pos = 0,
    neu = 0,
    neg = 0
  )) |>
  mutate(tweet_day = if_else(tweet_count > 0, 1L, 0L))

head(data_merged)
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
data_merged |>
  group_by(tweet_day) |>
  summarise(
    n_days   = n(),
    mean_ret = mean(return, na.rm = TRUE),
    sd_ret   = sd(return, na.rm = TRUE)
  )

t.test(return ~ tweet_day, data = data_merged)
ggplot(data_merged, aes(x = factor(tweet_day), y = return)) +
  geom_boxplot() +
  labs(x = "Tweet day (0 = no tweet, 1 = tweet)",
       y = "Daily return")

