library(quantmod)
library(Quantfunctions)
library(RMariaDB)


#connect to db
OptionsDb <- dbConnect(RMariaDB::MariaDB(), user='options_user', password=localuserpassword, dbname='testdb', host='localhost')
dbListTables(OptionsDb)

#create list of stocks
Tickers <- list("NKE","SPY","C","GILD", "TLT", "WFC", "UUP", "IBM", "ACN", "AMT", "PM", "ORCL",
                "PM", "ORCL", "NEE", "ABBV", "MDT", "TMO", "CRM", "COST","LLY","PYPL","MCD","AMGN",
                "BMY","ADBE","CMCSA","CVX","ABT","NVDA","KO","CSCO","BAC","PEP","DIS","WMT","XOM",
                "NFLX", "PFE","MRK","T","HD","MA","VZ","INTC","UNH","JPM","V","PG","GOOG","JNJ","FB",
                "AMZN","AAPL","MSFT","GLD", "BABA", "UUP")
for (i in Tickers){
  Symbol <- i
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
      
      dbWriteTable(OptionsDb, "temp", df, append = TRUE)
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
      
      dbWriteTable(OptionsDb, "temp", df, append = TRUE) 
    }
  }
}

#Get all options for each ticker and do multiplications
for (i in Tickers){
  #query statement for calls
  string <- sprintf("SELECT * FROM testdb.option where LEFT(Time,10) = '2020-05-06' and Ticker = '%s'",i)
  #fetch data
  oldDATA <- dbGetQuery(OptionsDb, string)
  oldVolume <- oldDATA$Volume
  
  string2 <- sprintf("SELECT * FROM testdb.temp where Ticker = '%s'",i)
  
  DATA <- dbGetQuery(OptionsDb, string2)
  newVolume <- DATA$Volume
  
  z <- length(oldVolume)
  y <- length(newVolume)
  
  #make new dataframes
  df <-data.frame()
  de <- data.frame()
  #make sure option contracts list are the same
  if (z == y){
    for (j in 1:length(oldVolume)){
      q <- DATA$Volume[j]
      w <- oldDATA$Volume[j]
      #if new volume has occured
      if ((!is.na(q)) & (!is.na(w))){
        if (q != w){
          if (!is.na(oldDATA$Volume[j])){
            newVolume <- DATA$Volume[j]
          } else {
            newVolume <- DATA$Volume[j]-oldDATA$Volume[j]
          }
          #it will throw an error if new data is NA
          
          #check if call or put
          if (DATA$OptionType[j] == "Call"){
            if (is.data.frame(df) && nrow(df)==0){
              df <-rbind(df,data.frame(
                Ticker = i,
                OptionType = "Call",
                InvestmentDollars = newVolume * DATA$Price[j],
                Date = Sys.time()
              ) )
            } else {
              df["InvestmentDollars"] <- df["InvestmentDollars"] + (newVolume * DATA$Price[j])
            }
          } else {
            if (is.data.frame(de) && nrow(de)==0){
              de <- rbind(de,data.frame(
                Ticker = i,
                OptionType = "Put",
                InvestmentDollars = newVolume * DATA$Price[j],
                Date = Sys.time()
              ) )
            } else {
              de["InvestmentDollars"] <- de["InvestmentDollars"] + (newVolume * DATA$Price[j]) 
            }
          }
        }
      }
    }
    #write to db
    if (length(df) != 0) {
      dbWriteTable(OptionsDb, "Contracts", df, append = TRUE) 
    }
    if (length(de) != 0){
      dbWriteTable(OptionsDb, "Contracts", de, append = TRUE) 
    }
  }
}

#I need to read contract database to update
#I need to drop temp db and override with new data
#I need to automate date by trimming sys date
#finaly run r script on timer
  
  
  
  dbDisconnect(OptionsDb)
  