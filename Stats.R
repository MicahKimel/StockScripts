library(quantmod)
library(RMariaDB)
library(jsonlite)
library(tidyverse)
library(ggplot)

OptionsDb <- dbConnect(RMariaDB::MariaDB(), user='optionuser', password=localuserpassword, dbname='stock_db', host='localhost')
RSI_Data <- dbReadTable(OptionsDb,"RSI")

Data <- RSI_Data %>% 
  select(Ticker, Date, return50, BuySignal) %>% 
  filter(!is.na(BuySignal)) %>% 
  arrange(Date) %>% 
  mutate(row = as.numeric(row_number()))

SPY_data <- Data %>% 
  filter(Ticker == "SPY")

#t test used in normal distribution
t.test(RSI_Data$RSIOne, RSI_Data$return50)

#wilcox test when not normal distribution
wilcox.test(RSI_Data$RSIOne, RSI_Data$return50, conf.int = TRUE)

# wilcox test rank sum
wilcox.test(Data$RSIOne, Data$return50, alternative = "g")

#wilcox test rank sum if assuming 1 to 1 comparison
wilcox.test(Data$RSIOne, Data$return50, paired = TRUE)

#test if two data sets follow the same distribution
ks.test(Data$RSIOne, Data$return50)

#test if two samples have the same variance
var.test(Data$RSIOne, Data$return50)

#correlation test
cor.test(Data$RSIOne, Data$return50)

#ggplot
toPlot <- RSI_Data %>%
  select(RSIOne, return50) %>% 
  filter(!is.na(RSIOne)) %>% 
  filter(RSIOne < 40) %>% 
  mutate(return50 = return50*100)

p <- ggplot(toPlot, aes(RSIOne, return50)) + geom_point()
print(p)


