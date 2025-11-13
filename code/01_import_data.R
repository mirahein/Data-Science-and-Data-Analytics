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
