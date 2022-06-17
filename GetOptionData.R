library(quantmod)
library(RMariaDB)
library(jsonlite)
library(tidyverse)


#connect to db
OptionsDb <- dbConnect(RMariaDB::MariaDB(), user='optionuser', password=localuserpassword, dbname='stock_db', host='localhost')
dbListTables(OptionsDb)

#create list of stocks
Tickers <- list("NKE","SPY","C","GILD", "TLT", "WFC", "UUP", "IBM", "ACN", "AMT", "PM", "ORCL",
                "PM", "ORCL", "NEE", "ABBV", "MDT", "TMO", "CRM", "COST","LLY","PYPL","MCD","AMGN",
                "BMY","ADBE","CMCSA","CVX","ABT","NVDA","KO","CSCO","BAC","PEP","DIS","WMT","XOM",
                "NFLX", "PFE","MRK","T","HD","MA","VZ","INTC","UNH","JPM","V","PG","GOOG","JNJ","FB",
                "AMZN","AAPL","MSFT","GLD", "BABA", "UUP", "YINN", "QQQ", "TQQQ", "VXX")

symbols <- stockSymbols() %>% 
  select(-LastSale, -MarketCap, -IPOyear, -Sector, -Industry, -Next.Shares) %>% 
  mutate(Date = Sys.Date())

#dbWriteTable(OptionsDb, "Stocks", symbols, append = TRUE)
Tickers <- symbols$Symbol

allOptions <- data.frame(Ticker = as.character(),
                         OptionType = as.character(),
                         Strike = as.numeric(),
                         Volume = as.numeric(),
                         Price = as.numeric(),
                         Change = as.numeric(),
                         Bid = as.numeric(),
                         Ask = as.numeric(),
                         Time = as.character(),
                         ExpDate = as.character())
error <- data.frame()
for (i in Tickers){
  print(i)
  Symbol <- i
  tryCatch(
  {
    Option <- quantmod::getOptionChain(i, NULL, src = "yahoo")
    
    #get number of expdates for ticker
    j <- 0
    for (a in names(Option)){
      j <- j+1
    }
    #loop through each expdate use names(GLD[i]) for strike date
    
    for(b in 1:j){
      #dataframe for tickers
      if (!is.null(Option[[b]]$calls)){
        df <-data.frame(
          Ticker = Symbol,
          OptionType = "Call",
          Strike = Option[[b]]$calls$Strike,
          Volume = Option[[b]]$calls$Vol,
          Price = Option[[b]]$calls$Last,
          Change = Option[[b]]$calls$Chg,
          Bid = Option[[b]]$calls$Bid,
          Ask = Option[[b]]$calls$Ask,
          Time = Sys.time(),
          ExpDate = names(Option[b]))
        allOptions <- rbind(allOptions, df)
      }
      if (!is.null(Option[[b]]$puts)){
        df <-data.frame(
          Ticker = Symbol,
          OptionType = "Put",
          Strike = Option[[b]]$puts$Strike,
          Volume = Option[[b]]$puts$Vol,
          Price = Option[[b]]$puts$Last,
          Change = Option[[b]]$puts$Chg,
          Bid = Option[[b]]$puts$Bid,
          Ask = Option[[b]]$puts$Ask,
          Time = Sys.time(),
          ExpDate = names(Option[b]))
        allOptions <- rbind(allOptions, df)
      }
    }
  },
  error=function(cond){
    print("error")
    error <- rbind(error, i)
    },
  warning=function(cond){},
  finally={})
}

dbWriteTable(OptionsDb, "Options", allOptions, append = TRUE)
