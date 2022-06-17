library(quantmod)
library(RMariaDB)
library(jsonlite)
library(tidyverse)
library(dplyr)

symbols <- stockSymbols() %>% 
  select(-LastSale, -MarketCap, -IPOyear, -Sector, -Industry, -Next.Shares) %>% 
  mutate(Date = Sys.Date())

#dbWriteTable(OptionsDb, "Stocks", symbols, append = TRUE)
Tickers <- symbols$Symbol
for (i in Tickers){
  print(i)
  tryCatch(
    {getSymbols(i, from = '2000-01-01',
             to = "2021-08-28",warnings = FALSE,
             auto.assign = TRUE)
    },
    error=function(cond){},
    warning=function(cond){},
    finally={})
}

Data <- data.frame(Ticker = as.character(), 
                   Open = as.numeric(), 
                   High = as.numeric(), 
                   Low = as.numeric(), 
                   Close = as.numeric(), 
                   Volume = as.numeric(), 
                   Date = as.character())

for (i in Tickers){
  tryCatch(
  {
    x <- as.data.frame(eval(as.name(i))) 
    x <- x %>% 
      rename_with(~substring(.,nchar(i)+2), everything()) %>% 
      mutate(Date = row.names(x)) %>% 
      mutate(Ticker = i) %>% 
      select(Ticker, Open, High, Low, Close, Volume, Date, -Adjusted)
    Data <<- rbind(Data, x)
  },
  error=function(cond){},
  warning=function(cond){},
  finally={})
}

dbWriteTable(OptionsDb, "RSI", YINN, append = TRUE)




SPY2 <- getQuote(i) %>% 
  mutate(Date = Sys.Date()) %>% 
  mutate(Ticker = i) %>% 
  select(Ticker, Open, High, Low, "Close" = Last, Volume, Date)


