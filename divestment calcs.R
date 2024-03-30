library(tidyverse)

#clean column names
holdings <- nycers_holdings
  colnames(holdings) <- gsub(" ",".",colnames(holdings))

# select relevant columns
holdings<-select(holdings,Local.Currency.Code,Trade.Country.Name,Security.Name,Security.Description,Maturity.Date,Base.Market.Value,Period.End.Date,Asset.Class,Investment.Type.Name,Major.Industry.Name,Minor.Industry.Name)

#set column date formats
holdings<-holdings %>%
  mutate(Period.End.Date=as.Date(Period.End.Date,format="%m/%d/%Y"))

holdings<-holdings %>%
  mutate(Maturity.Date=as.Date(Maturity.Date,format="%m/%d/%Y"))

#filter and sum equities purchased on the Israeli market
equities_2022<- holdings %>%
  filter(Trade.Country.Name=="ISRAEL") %>% #filter for Israel
  filter(Period.End.Date>as.Date("2021-12-31")) %>% #filter for holdings 2022 and on
  summarise(sum=sum(Base.Market.Value)) #summarize the base market value = 41,212,194
    write.csv(equities_2022,"equities_2022.csv") #generate csv

#filter and sum equities purchased on other markets
other_mkts_2022<- holdings %>%
  filter(Trade.Country.Name!="ISRAEL") %>% #filter for non-Israel markets
  filter(grepl("ILS",Security.Description)) %>% #filter for Israeli Common Stock
  filter(Period.End.Date>as.Date("2021-12-31")) %>% #filter for holdings 2022
  summarise(sum=sum(Base.Market.Value)) #summarize the base market value = 46,811,117
    write.csv(other_mkts_2022,"other_mkts_2022.csv") #generate csv

#filter and sum bonds 
bonds <- holdings %>%
  filter(Asset.Class=="FIXED INCOME") %>% #filter for fixed-income asset class
  filter(Investment.Type.Name=="GOVERNMENT ISSUES") %>% #filter government issues
  filter(Major.Industry.Name=="YANKEE") %>% #filter just Yankee Bonds
  filter(grepl("ISRAEL",Security.Name)) %>% #filter for Israel
  filter(Maturity.Date>as.Date("2022-12-31")) %>% #filter for anything that had not matured prior to publication of data in Jan, 2023
  summarize(sum=sum(Base.Market.Value)) #summarize the base market value = 30,011,486
    write.csv(bonds,"bonds") #generate csv