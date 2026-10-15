library(tidyverse)
library(readxl)
library(patchwork)
libary(ggtheme)

# IMPORTING AND TIDYING DATA ################
?read_excel()
Penn <-read_excel("ECN206/pwt110.xlsx",3) #sheet 3 has the PENN data
summary(Penn)
glimpse(Penn)

print(colnames(Penn))

?which()
which(Penn$countrycode=='GBR')

PennUK <- Penn %>% #choosing only the UK
  filter(countrycode == 'GBR')

print(PennUK)
head(PennUK)

colnames(PennUK)
?select()
UKData <- PennUK %>%
  select('year','pop','avh','hc','rgdpna','rnna')

print(UKData, n=Inf)


?rename()

newNames=c('Year','Population','AvgHrs','HumanCapitalIndex','RGDP','CapitalStock')

names(UKData) <- newNames


# ?filter()
# 
# UKFilter <- UKData %>%
#   filter(Year >= 1990)
# UK_Last30Y <- UKFilter
# rm(UKFilter)
# 
# UK_Last23Y <- UK_Last30Y
# rm(UK_Last30Y)

UKData <- UKData %>%
  mutate(Population=Population*1000000,
         RGDP=RGDP*1000000,
         CapitalStock=CapitalStock*1000000)


UKData <- UKData %>%
  mutate(RGDPCapita = RGDP/Population)


# UKData <- UKData %>%
#   relocate() 'having blank = move to front'

UKData <- UKData %>%
  relocate(RGDPCapita, .after=RGDP)

# DATA VISUALISATION #############

p1_log <- UKData %>%
  ggplot() + 
  geom_line(mapping=aes(x=Year, y=RGDPCapita),colour='goldenrod')+
  ylab('GDP per capita ($)') + xlab('Year (1953-2023)')+
  scale_y_continuous(transform ='log10',limits=c(1e+4,5.1e+4))+
  labs(title='Real GDP Growth in the UK, 1953-2023 (log scale)')

p2_base <- UKData %>%
  ggplot() + 
  geom_line(mapping=aes(x=Year, y=RGDPCapita),colour='royalblue4')+
  ylab('GDP per capita ($)') + xlab('Year (1953-2023)')+
  scale_y_continuous(limits=c(1e+4,5.1e+4))+
  labs(title='Real GDP Growth in the UK, 1953-2023 (arithmetic scale)')
p2_base

print((p1_log+theme_bw()) / (p2_base+theme_bw())) #using patchwork effectively


?ggplot()
help("par")
help("sec axis")

colours()

UKData %>%
  filter(Year > 2000 & Year < 2020) %>%
  ggplot() + geom_line(mapping=aes(x=Year, y=RGDP),colour='royalblue4')+ylim(2.25e+12,3.5e+12)+scale_y_log10()+
  labs(title='UK Growth has not recovered post 2008 levels', y='log of Real GDP per capita, in US$', x='Time (yrs)')


# GROWTH ACCOUNTING ####
years <-(filter(UKData, Year >= 2000 & Year <=2007)$Year)
output <-(filter(UKData, Year >= 2000 & Year <=2007)$RGDP)
kapital <-(filter(UKData, Year >= 2000 & Year <=2007)$CapitalStock)
labour<-(filter(UKData, Year >= 2000 & Year <=2007)$Population)

growthAccounting <- tibble(years,output,kapital,labour)
?mutate()
growthAccounting <- growthAccounting %>%
  mutate(output=log(output),
         kapital=log(kapital),
         labour=log(labour),
         TFP=output-1/3*kapital - 2/3*labour)


gY <- as.numeric(growthAccounting[8,2]-growthAccounting[1,2])/7
gK <- as.numeric(growthAccounting[8,3]-growthAccounting[1,3])/7
gL <- as.numeric(growthAccounting[8,4]-growthAccounting[1,4])/7
gA <- as.numeric(growthAccounting[8,5]-growthAccounting[1,5])/7


finalplot1 <- ggplot(data=growthAccounting) + geom_line(mapping=aes(x=years,y=TFP)) + labs(title='TFP Growth, 2000-2007', y='TFP Growth, %', x='Year')

x1 <- c('gA','gK','gL','gY')
x2 <- c(gA,gK,gL,gY)
x <- tibble(x1,x2)

finalplot2 <- x %>%
  ggplot() + geom_col(mapping=aes(x=x1,y=x2),fill=c('orchid','lavender','steelblue','goldenrod')) + labs(title='Constant Growth split by component', y='Growth, %', x='Component')

final <- (finalplot1+theme_excel_new()) + (finalplot2+theme_excel_new())

print((finalplot1+theme_fivethirtyeight()) + (finalplot2+theme_fivethirtyeight()))

print((finalplot1+theme_wsj()) + (finalplot2+theme_wsj()))

final
colours()

