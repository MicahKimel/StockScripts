library(quantmod)
library(RMariaDB)
library(jsonlite)
library(tidyverse)
library(dplyr)

getSymbols("YINN", from = '2000-01-01',
           to = "2021-08-28",warnings = FALSE,
           auto.assign = TRUE)


YINN <- RSI_Data %>% 
  group_by(Ticker) %>% 
  arrange(Date) %>% 
  mutate(GainOrLoss = case_when(
    `Close` > lag(`Close`, k = 1) ~ TRUE,
    TRUE ~ FALSE)) %>% 
  mutate(Gain = ifelse(GainOrLoss, (`Close` / lag(`Close`, k = 1))-1, 0)) %>% 
  mutate(Loss = ifelse(!GainOrLoss, 1-(`Close` / lag(`Close`, k = 1)), 0)) %>% 
  mutate(totalGain = Gain + lag(`Gain`, 1) + lag(`Gain`, 2) + lag(`Gain`, 3) +
           lag(`Gain`, 4) + lag(`Gain`, 5) + lag(`Gain`, 6) + lag(`Gain`, 7) + 
           lag(`Gain`, 8) + lag(`Gain`, 9) + lag(`Gain`, 10) + lag(`Gain`, 11) +
           lag(`Gain`, 12) + lag(`Gain`, 13) + lag(`Gain`, 14)) %>% 
  mutate(totalLoss = Loss + lag(`Loss`, 1) + lag(`Loss`, 2) + lag(`Loss`, 3) +
           lag(`Loss`, 4) + lag(`Loss`, 5) + lag(`Loss`, 6) + lag(`Loss`, 7) + 
           lag(`Loss`, 8) + lag(`Loss`, 9) + lag(`Loss`, 10) + lag(`Loss`, 11) +
           lag(`Loss`, 12) + lag(`Loss`, 13) + lag(`Loss`, 14)) %>% 
  mutate(GainCount = as.logical(Gain) + as.logical(lag(`Gain`, 1)) + as.logical(lag(`Gain`, 2)) + 
           as.logical(lag(`Gain`, 3)) + as.logical(lag(`Gain`, 4)) + as.logical(lag(`Gain`, 5)) + 
           as.logical(lag(`Gain`, 6)) + as.logical(lag(`Gain`, 7)) + 
           as.logical(lag(`Gain`, 8)) + as.logical(lag(`Gain`, 9)) + 
           as.logical(lag(`Gain`, 10)) + as.logical(lag(`Gain`, 11)) +
           as.logical(lag(`Gain`, 12)) + as.logical(lag(`Gain`, 13)) + 
           as.logical(lag(`Gain`, 14))) %>% 
  mutate(LossCount = as.logical(Loss) + as.logical(lag(`Loss`, 1)) + as.logical(lag(`Loss`, 2)) + 
           as.logical(lag(`Loss`, 3)) + as.logical(lag(`Loss`, 4)) + 
           as.logical(lag(`Loss`, 5)) + as.logical(lag(`Loss`, 6)) + 
           as.logical(lag(`Loss`, 7)) + as.logical(lag(`Loss`, 8)) + 
           as.logical(lag(`Loss`, 9)) + as.logical(lag(`Loss`, 10)) + 
           as.logical(lag(`Loss`, 11)) + as.logical(lag(`Loss`, 12)) + 
           as.logical(lag(`Loss`, 13)) + as.logical(lag(`Loss`, 14))) %>% 
  mutate(AverageGainWeighted = totalGain/GainCount) %>% 
  mutate(AverageLossWeighted = totalLoss/LossCount) %>% 
  mutate(AverageGain = totalGain/14) %>% 
  mutate(AverageLoss = totalLoss/14) %>% 
  mutate(RSIWeighted = 100 - (100/(1+((AverageGainWeighted/14)/(AverageLossWeighted/14))))) %>% 
  mutate(RSIOne = 100 - (100/(1+(AverageGain/AverageLoss)))) %>% 
  mutate(RSITwo = 100 - (100/(1+((((lag(`AverageGain`, 1))*13)+AverageGain)/(((lag(`AverageLoss`, 1))*13)+AverageLoss))))) %>% 
  mutate(RSIAvg = (RSIWeighted+RSIOne+RSITwo)/3) %>% 
  mutate(return10 = ((lead(Close, 10) / Close)-1)) %>% 
  mutate(return20 = ((lead(Close, 20) / Close)-1)) %>% 
  mutate(return5 = ((lead(Close, 5) / Close)-1)) %>% 
  mutate(return50 = ((lead(Close, 50) / Close)-1)) %>% 
  mutate(RSIunder30 = ifelse(RSIOne < 30 & lag(RSIOne, 1) < 40 & lag(RSIOne, 2) < 50 & lag(RSIOne, 3) < 50
                             & lag(RSIOne, 4) < 50 & lag(RSIOne, 5) < 50
                             , return50, 0)) %>% 
  mutate(BuySignal = ifelse(RSIOne < 30 & lag(RSIOne, 1) < 40 & lag(RSIOne, 2) < 50 & lag(RSIOne, 3) < 50
                            & lag(RSIOne, 4) < 50 & lag(RSIOne, 5) < 50
                            , TRUE, FALSE)) %>% 
  mutate(RSIunder30 = ifelse(is.na(RSIunder30), 0, RSIunder30))

YINN_Df <- as.data.frame(YINN) %>% 
  group_by(Ticker) %>% 
  select(Ticker, Date, RSIWeighted, RSIOne, RSITwo, RSIAvg, return10, return20, return5,return50, GainCount, LossCount, AverageGain, 
         AverageLoss, AverageGainWeighted, AverageLossWeighted, RSIunder30) %>% 
  filter(RSIunder30 != 0) %>% 
  mutate(s = sum(return50)) %>% 
  add_count() %>% 
  mutate(Avg = s/n) %>% 
  distinct(Ticker, .keep_all = TRUE)

sum(YINN_Df$Avg)/count(YINN_Df)
