library(readxl)
library(tidyverse)
library(patchwork)
PENN <- read_excel("ECN206/pwt110.xlsx",3)

glimpse(PENN)

PennUK <- PENN %>%
  filter(countrycode=='GBR')

colnames(PennUK)

UKData <- PennUK %>%
  select('year','rgdpna','pop','avh','rnna','hc')

names(UKData) <- c('Year','RGDP','Population','AvgHrs','CapitalStock','HC')

UKData <- UKData %>%
  mutate(RGDPCapita = RGDP/Population)

UKData <- UKData %>%
  relocate(RGDPCapita, .after=RGDP)

glimpse(UKData)


timeGDPplot1 <- UKData %>%
  ggplot() + geom_line(mapping=aes(x=Year,y=RGDPCapita),colour='steelblue') + 
  labs(title='Real GDP, 1953-2023',y='Real GDP, 2021 prices (US$)')
print(timeGDPplot1)

timeGDPplot2 <- UKData %>%
  ggplot() + geom_line(mapping=aes(x=Year,y=RGDPCapita),colour='royalblue4') + 
  scale_y_continuous(transform='log10') +
  labs(title='Real GDP (log scale), 1953-2023',y='Real GDP, 2021 prices (US$)')
print(timeGDPplot2)
print(timeGDPplot1+timeGDPplot2)
#patchwork function allows me to print 2 simultaneously

