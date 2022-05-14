library(tidyverse)
library(rvest)
library(robotstxt)
library(viridisLite)
library(ggplot2)
library(tidyr)


str(cost_of_living)
str(happiness_rank)
#Clean the data
happiness_rank_2019 <- happiness_rank %>% rename('Country'= `Country or region`)
view(happiness_rank_2019)
names(cost_of_living)
cost_of_living_2020 <- cost_of_living[,-c(9:10)]
names(cost_of_living_2020)
#Left join the cost of living and the happiness rank tables to take a closer look at
#the correlation.
happiness_col <- left_join(cost_of_living_2020, happiness_rank_2019, by="Country")
view(happiness_col)
#Drop missing values
happiness_col <- happiness_col %>% 
  distinct(Country,.keep_all = TRUE) %>% 
  drop_na()


view(happiness_col)
#Create scatterplot for the top 10 happiest countries. 
happiness_col %>% filter(`Overall rank` <= 10) %>%
  ggplot(aes(x=`Overall rank`, y=`Rank 2020`, col=Country))+
  geom_point(size=3)+
  labs(title="Top 10 Happiness Rank vs. Cost of Living",x="Happiness Rank", y="Cost of Living") +
  theme(plot.title=element_text(hjust=.5,size=15,face="bold"))
#See the correlation. 
with(happiness_col[happiness_col$`Overall rank`<= 10,],cor(`Rank 2020`, `Overall rank`))



###################################################################################
#Remove unwanted columns from the GNI dataset. 
GNI_2014_2018 <- GNI_by_Country[,-c(2:25)]
view(GNI_2014_2018)
str(GNI_2014_2018)
#Change from character to numeric values. 
GNI_2014_2018$`2014` <- as.numeric(GNI_2014_2018$`2014`)
GNI_2014_2018$`2015` <- as.numeric(GNI_2014_2018$`2015`)
GNI_2014_2018$`2016` <- as.numeric(GNI_2014_2018$`2016`)
GNI_2014_2018$`2017` <- as.numeric(GNI_2014_2018$`2017`)
GNI_2014_2018$`2018` <- as.numeric(GNI_2014_2018$`2018`)
#Find the average income for each years. 
GNI_2014_2018 %>% summarize(avg=mean(`2014`,na.rm=T))
GNI_2014_2018 %>% summarize(avg=mean(`2015`,na.rm=T))
GNI_2014_2018 %>% summarize(avg=mean(`2016`,na.rm=T))
GNI_2014_2018 %>% summarize(avg=mean(`2017`,na.rm=T))
GNI_2014_2018 %>% summarize(avg=mean(`2018`,na.rm=T))

above_average_income <- GNI_2014_2018 %>% filter(`2014` > 0.7)
max(above_average_income$`2014`)  


#################################################################################
top_happiness <- happiness_rank_2019 %>%
  filter(`Overall rank` <= 10)

#Create a new data frame moving the columns to rows. 
DF <- data.frame(Country = c("Finland", "Denmark", "Norway", "Iceland", "Netherlands", "Switzerland", "Sweden", "New Zealand", "Canada", "Austria"),
                 Score = c(7.769,7.600,7.554,7.494,7.488,7.480,7.343,7.307,7.278,7.246),
                 `GDP per capita` = c(1.340,1.383,1.488,1.380,1.396,1.452,1.387,1.303,1.365,1.376 ),
                 Social.support = c(1.587,1.573,1.582,1.624,1.522,1.526,1.487,1.557,1.505,1.475),
                 Healthy.life.expectancy = c(0.986,0.996,1.028,1.026,0.999,1.052,1.009,1.026,1.039,1.016),
                 Freedom.to.make.life.choices = c(0.596,0.592,0.603,0.591,0.557,0.572,0.574,0.585,0.584,0.532),
                 Generosity = c(0.153,0.252,0.271,0.354,0.322,0.263,0.267,0.330,0.285,0.244),
                 Perceptions.of.corruption = c(0.393,0.410,0.341,0.118,0.298,0.343,0.373,0.380,0.308,0.226))
DFtall <- DF %>% gather(key = Event, value=Value, Score:Perceptions.of.corruption)

#Create facet grid for each event. 
ggplot(DFtall,aes(x= Country ,y = Value, fill = Country))+
  geom_bar(stat = "identity")+
  facet_grid(.~Event)+
  labs(title="Contribution to Happiness Rank by Country")+
  theme(axis.text.x = element_text(angle=90, size=9), plot.title=element_text(hjust=.5,size=25,face="bold"))
#####################################################################################
#Select the variables I need from the education dataset.
Education_2019 <- Education %>% select('Country Name', 'Country Code', 'Series', 'Series Code', '2019 [YR2019]') 
 
str(Education_2019)
Education_2019 <- Education_2019 %>% rename('2019'= `2019 [YR2019]`)

#Change character variable to numeric and drop all missing variables. 
Education_2019$`2019` <- as.numeric(Education_2019$`2019`)
view(Education_2019)
str(Education_2019)
str(Education)
Education_2019 <- Education_2019 %>% drop_na()

#Find the average the each category in the series variable. 
Education_2019 %>% group_by(Series) %>%
  summarize(avg=mean(`2019`,na.rm=T))
#Get rid of some of the outliers for the percentage invested in education. 
top_education_invest <- Education_2019 %>% filter(`2019` <  3359, `2019` >= 1)
gov_fund_ed <- Education_2019 %>% filter(`Series Code` == "SE.XPD.TOTL.GD.ZS")
view(top_education_invest)
str(Education_nona)
view(gov_fund_ed)
#Convert to percentage. 
gov_fund_ed <- gov_fund_ed %>% rename('Country' = 'Country Name')
percent <- function(x, digits = 2, format = "f", ...) {      
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
  }
percent(x$`2019`)
#Clean data 
top20_happ_rank <- happiness_col %>% arrange(.,`Overall rank`) %>% filter(`Overall rank` <= 20) %>%
  select("Country",  "Overall rank", "Rank 2020", "Cost of Living Index")
top20_happ_rank <- top20_happ_rank %>% rename('Happiness rank'= `Overall rank`)
top20_happ_rank <- top20_happ_rank %>% rename('Cost of living rank'= `Rank 2020`)
view(top20_happ_rank)
#Join all the datasets to find correlations. 
happiness_col_edu <- left_join(top20_happ_rank, gov_fund_ed, by="Country")
happiness_col_edu <- happiness_col_edu %>% drop_na()
view(happiness_col_edu)
#Clean data 
GNI_2018 <- GNI_2014_2018 %>% select("Country", "2018")
view(GNI_2018)
happiness_col_edu_gni <- left_join(happiness_col_edu, GNI_2018, by="Country")
happiness_col_edu_gni <- happiness_col_edu_gni %>% rename("Education funds" = `2019`)
happiness_col_edu_gni <- happiness_col_edu_gni %>% rename("GNI" = `2018`)
view(happiness_col_edu_gni)
#Create graphs 
ggplot(happiness_col_edu_gni,aes(x= `Country` ,y = `Education funds`, fill = `Cost of living rank`))+
  geom_bar(stat = "identity")+
  labs(title="Percentage of GDP Put Into Education")+
  theme(axis.text.x = element_text(angle=90, size=9), plot.title=element_text(hjust=.5,size=15,face="bold"))+
  scale_fill_gradientn(colours = viridis(256, option = "C"))

ggplot(happiness_col_edu_gni,aes(x= `Country` ,y = `GNI`, fill = `Cost of living rank`))+
  geom_bar(stat = "identity")+
  labs(title="GNI for Top Hppiest Countries")+
  theme(axis.text.x = element_text(angle=90, size=9), plot.title=element_text(hjust=.5,size=15,face="bold"))+
  scale_fill_gradientn(colours = viridis(256, option = "C"))
happiness_col_edu_gni <- happiness_col_edu_gni %>% select("Happiness rank", "Country", "Cost of living rank", "Cost of Living Index", "Education funds", "GNI")
view(happiness_col_edu_gni)
max(GNI_2018$`2018`)
view(GNI_2018)
max(Education_2019$`2019`)
view(gov_fund_ed)
