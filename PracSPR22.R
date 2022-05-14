library(ggplot2)
library(tidyverse)
library(rvest)
library(dplyr)
#For glance
library(broom)

#Use stat=identity to inform ggplot that I will provide values for the y-axis
#and it doesn't have to do any aggregations.
SocialUsage <- PercbyReg  %>%
  ggplot(aes(x= Region, y= Percentage))+
  geom_bar(stat="identity", fill="blue") +
  geom_text(aes(label=Percentage),position = position_dodge(width=0.9),vjust=-0.25)+
  labs(title="Percentage of Social Media Usage \nby Region (2021)",x="Region", y="Percentage") +
  theme_bw()+
  theme( axis.text.x = element_text(angle=75, size=9,face="bold"), plot.title=element_text(hjust=.5,size=15,face="bold"))
#Used to make the y access values percentages
SocialUsage + scale_y_continuous(labels = scales :: percent_format(accuracy = 1))
names(Suicidedata)
##Get the max suicides per 100K people of the population
max(Suicidedata$`suicides/100k pop`)
Suicidedata %>%
  filter(`suicides/100k pop` == 224.97)
max(Suicidedata$`suicides/100k pop`)
min(Suicidedata$year)
view(SMUsage2021)
view(SuicidebyCountry)

#Change the name of a value from United Arab Emirates to 
#UAE
SuicidebyCountry[SuicidebyCountry == "United Arab Emirates"] <- "UAE"
#Change the variable name country to Country with capital 'C'.
SuicidebyCountry <- SuicidebyCountry %>% rename("Country"= country)
#Inner join Suicide rates and social media usage by Country
UserVsSui<- inner_join(SuicidebyCountry,SMUsage2021, by="Country")
#Scatterplot showing social media usage vs. suicide rates. 
UserVsSuiplot <- UserVsSui %>%
  ggplot(aes(x=`Share of Population`, y=rate2019both)) +
  geom_point(aes(col = Country, size = 3)) +
  geom_hline(yintercept = mean(UserVsSui$rate2019both), color = "blue")+
  geom_smooth(method = "lm", se = F, col="pink") +
  labs(title="Social Media Usage vs. Suicicde Rates \n by Country ",x="Percentage of Usage", y="Suicide Rates") +
  theme_bw()+
  theme( axis.text.x = element_text(angle=75, size=11,face="bold"), plot.title=element_text(hjust=.5,size=15,face="bold"))
UserVsSuiplot + scale_x_continuous(labels = scales :: percent_format(accuracy = 1))


#Create linear model to test the relationship between the two variables. 
UserVsSuilm <- lm(rate2019both~`Share of Population`, data = UserVsSui)
glance(UserVsSuilm)
UserVsSuilm
summary(UserVsSuilm)
cor.test(UserVsSui$rate2019both, UserVsSui$`Share of Population`)
plot(UserVsSuilm)



#average suicide rate for both men and women across diff countries.
mean(UserVsSui$rate2019both)
#Suicide rate greater than the average(10) and greater than 50% of the pop 
#uses SM. 
high_rate_use <- UserVsSui %>%
  filter(rate2019both >= 10 & `Share of Population` > 0.5)
high_rate_use
#Suicide rate lower than the average(10) and less than 50% of the pop 
#uses SM. 
low_rate_use <- UserVsSui %>%
  filter(rate2019both < 10 & `Share of Population` < 0.5)
low_rate_use
#Average social media usage
avgusage <- SMUsage2021 %>% summarize(mean = mean(`Share of Population`))
avgusage
#Test the correlation b/w year and suicides per 100k. Looking for an increase over time. 
cor.test(Suicidedata$year, Suicidedata$`suicides/100k pop`)
#Test the correlation b/w suicides numbers and population. 
cor.test(Suicidedata$suicides_no, Suicidedata$population)

#Top 5 countries with high SM usage. 
top_five_usage <- UserVsSui %>%
  filter(`Share of Population` >= 0.85)
top_five_usage
#80% or more of the pop on SM on the original dataset. 
SMUsage2021 %>%
  filter(`Share of Population` >= 0.80)
Suicidedata[Suicidedata == "Russian Federation"] <- "Russia"
Suicidedata[Suicidedata == "Republic of Korea"] <- "South Korea"
#Dataset with randomly selected countries from countries with 80% or higher SM usage.  
high_sui_countries <- Suicidedata %>%
  filter(country %in% c("South Korea", "Guyana", "Suriname",  "Russia"))
view(high_usage_countries)
view(Suicidedata)
#Dataframe with mean and SD of suicides/100k pop grouped by age and sex. 
high_sui_countries %>%
  group_by(country, age) %>%
  summarize(mean=mean(`suicides/100k pop`, na.rm=T), stdev=sd(`suicides/100k pop`, na.rm = T)) %>%
  print.data.frame()
#Create bar graph showing suicide rates from 1985 - 2016
high_sui_countries %>% 
  ggplot(aes(x = year, y = `suicides/100k pop`, fill = age)) +
  geom_bar(stat = "identity")+
  facet_wrap(vars(country)) + 
  labs(title = "Suicides per 100k Between 1985-2016") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = .5, size = 15, face = "bold"))


#Create linear model for suicides/100k pop and year to see the correlation b/w the two. 
Suicidelm <- lm(`suicides/100k pop` ~ year + age + country + 0, data = Suicidedata) 
summary(Suicidelm)
#Create linear model only for the values I plan to put in the graph. 
str(high_sui_countries)
high_sui_countrieslm <-  lm(`suicides/100k pop` ~ year  + age + country + 0, data = high_sui_countries)
summary(high_sui_countrieslm)
plot(high_sui_countrieslm)
#Plot the linear modal to see if the data is normally distributed. 
plot(Suicidelm)
#Min/Max suicide rate 
max(UserVsSui$rate2019both)
min(UserVsSui$rate2019both)

# See which countries have the min/max suicide rate
suimax <- UserVsSui %>%
  filter(rate2019both == 28.6)
suimax
suimin <- UserVsSui %>%
  filter(rate2019both == 2.2)
suimin
#Min/Max usage
max(UserVsSui$`Share of Population`)
min(UserVsSui$`Share of Population`)
#See which Countries have the min/max usage rate.
usmax <- UserVsSui %>%
  filter(`Share of Population` == 0.99)
usmax
usmin <- UserVsSui %>%
  filter(`Share of Population` == 0.16)
usmin



max(SuicidebyCountry$rate2019both)
x <- SuicidebyCountry %>% 
  filter(rate2019both == 72.4)
x
#Outliers: Russia, South Korea, and South Africa 
X <- UserVsSui %>% filter(rate2019both >= 20)
X
############################################################################
#Webscrape
content <- read_html("https://www.pewresearch.org/internet/fact-sheet/social-media/?menuItem=81867c91-92ad-45b8-a964-a2a894f873ef")
tables <- content %>%
  html_table(fill=TRUE)
soci_demo <- tables[[2]]
soci_demo <- soci_demo %>% rename("Date" = c(1))

str(mental_health)

mental_health$Year <- as.Date(as.character(mental_health$Year), format = '%Y')

str(mental_health)
ggplot(mental_health, aes(x = Year, y = `Mental Illness`, color = `Age Group`)) +
  geom_point()+ 
  geom_line()+ 
  labs(title = "Mental Illness Diagnosis 2008-2020", caption = 'Data Source : samhsa.gov')+
  theme_bw()

mental_illlm <- lm(`Mental Illness` ~ `Age Group` + Year + 0, data = mental_health)
summary(mental_illlm)
plot(mental_illlm)
