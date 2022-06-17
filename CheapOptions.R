Prices_Data <- dbReadTable(OptionsDb,"prices")

Prices <- Prices_Data %>% 
  filter(Date == '2021-08-27')

PriceReturns <- Prices_Data %>% 
  mutate(return10 = ((lead(Close, 10) / Close)-1)) %>% 
  mutate(return20 = ((lead(Close, 20) / Close)-1)) %>% 
  mutate(return50 = ((lead(Close, 50) / Close)-1)) %>% 
  mutate(return80 = ((lead(Close, 80) / Close)-1)) %>% 
  group_by(Ticker) %>%
  add_count() %>% 
  mutate(Avg10 = sum(return10)/n) %>% 
  mutate(Avg20 = sum(return20)/n) %>% 
  mutate(Avg50 = sum(return50)/n) %>% 
  mutate(Avg80 = sum(return80)/n) %>% 
  select(Ticker, Avg10, Avg20, Avg50, Avg80) %>% 
  distinct(Ticker, .keep_all = TRUE)

GetCheapOptions <- allOptions %>% 
  filter(Ask < 1 & Price < 1) %>% 
  left_join(Prices, by=c("Ticker" = "Ticker")) %>% 
  filter((Strike / Close) < 1.5) %>% 
  mutate(PerOut = Strike/Close) %>% 
  filter(OptionType == "Call") %>% 
  filter(Strike > Close) %>% 
  left_join(PriceReturns, by=c("Ticker" = "Ticker")) %>% 
  select(Ticker, Strike, Close, PerOut, Ask, ExpDate, Price, Avg10, Avg20, Avg50, Avg80) %>% 
  distinct(Ticker, .keep_all = TRUE) %>% 
  filter(Avg80 > 0.05)
write.csv(GetCheapOptions, "cheapoptions.csv")
