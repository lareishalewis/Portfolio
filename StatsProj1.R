library(ggplot2)
library(dplyr)
library(tidyverse)
library(moderndive)
library(broom)


view(BiMonlingIQ)
names(BiMonlingIQ)




#Create new dataset with only the columns I need. 
BiMonlingIQ2 <- BiMonlingIQ[c(1:6,9:10,15,20)]
BiMonlingIQ2
view(BiMonlingIQ2)

#Find average IQ for the bilingual and monolingual group. 
BiMonlingIQ2 %>%
  group_by(Group.name) %>%
  summarize(IQ_mean = mean(IQ))

BiMonlingIQ2 %>%
  summarize(Age_mean = mean(Age))

BiMonlingIQ2 %>%
  summarize(Col_Sha_mean = mean(Switch.trial.acc))

BiMonlingIQ2 %>%
  group_by(Group.name) %>%
  summarize(stdev = sd(IQ))


#Create bar graph that visualizes the # of participants that scored each IQ. 
ggplot(BiMonlingIQ2, aes(IQ)) +
  geom_bar(color="blue", fill="white") +
  facet_wrap(vars(Group.name))+
  labs(title = "Bilingual vs. Monolingual IQ scores") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

#Find the min and max IQ score in the dataset. 
minIQ <- min(BiMonlingIQ2$IQ) 
minIQ
maxIQ <- max(BiMonlingIQ2$IQ)
maxIQ
x <- BiMonlingIQ2 %>%
  filter(IQ == 54 )
x
MinAge <- min(BiMonlingIQ2$Age)
MinAge
MaxAge <- max(BiMonlingIQ2$Age)
MaxAge

MinDig <- min(BiMonlingIQ2$Digitspanbackward)
MinDig
MaxDig <- max(BiMonlingIQ2$Digitspanbackward)
MaxDig


#Count the number of Monolingual and Bilingual participates with an above average IQ. 
MonAbovAvg <- BiMonlingIQ2 %>%
  count(Group.name == "Monolingual" & IQ > 43)
MonAbovAvg

BiAbovAvg <- BiMonlingIQ2 %>%
  count(Group.name == "Bilingual" & IQ > 43)
BiAbovAvg

  


#Count the number of participants for each group. 
x2 <- BiMonlingIQ2 %>%
  count(Group.name)
x2

#Run a correlation test on variables to see the correlation and p-value.
cor.test(BiMonlingIQ2$Age, BiMonlingIQ2$IQ)
cor.test(BiMonlingIQ2$Education, BiMonlingIQ2$IQ)
cor.test(BiMonlingIQ2$Group.ID, BiMonlingIQ2$IQ)
cor.test(BiMonlingIQ$IQ, BiMonlingIQ$Stroop.IC)
cor.test(BiMonlingIQ2$IQ, BiMonlingIQ2$Switch.trial.acc)
cor.test(BiMonlingIQ2$IQ, BiMonlingIQ2$Digitspanbackward)



#Plot Education and IQ and Age and IQ
ggplot(BiMonlingIQ2, aes(Education, IQ)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, color ="red") +
  labs(title = "IQ vs. Education")

ggplot(BiMonlingIQ2, aes(Age, IQ)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, color = "red") +
  labs(title = "IQ vs. Age")

ggplot(BiMonlingIQ2,aes(Stroop.IC,IQ)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, color = "red") +
  labs(title = "IQ vs Stroop(Incongruent)")

ggplot(BiMonlingIQ2,aes(Switch.trial.acc,IQ)) +
  geom_point(aes(color = Group.name)) + 
  geom_smooth(method = "lm", se=F, color="red") +
  labs(title = "IQ vs. Color-shape Task accuracy ")

ggplot(BiMonlingIQ2, aes(IQ, Sex)) +
  geom_point()
  
#Create linear regression models for certain variable to test the correlation between variables. 
mdl_edu <- lm(IQ ~ Education, data = BiMonlingIQ2)
summary(mdl_edu)

mdl_Age_IQ <- lm(IQ ~ Age, data = BiMonlingIQ2)
mdl_Age_IQ
summary(mdl_Age_IQ)

mdl_str_IQ <- lm(IQ ~ Stroop.IC, data = BiMonlingIQ2)
summary(mdl_str_IQ)

mdl_switch_trial <- lm(IQ ~ Switch.trial.acc, data = BiMonlingIQ2)
summary(mdl_switch_trial)

mdl_colshp_grp <- lm(Switch.trial.acc ~ Group.name, data = BiMonlingIQ2)
summary(mdl_colshp_grp)

mdl_edu_age_str <- lm(IQ ~ Education + Age + Stroop.IC, data = BiMonlingIQ2)
summary(mdl_edu_age_str)

#Add +0 to ensure to get p-value and other values of both male and female. 
mdl_sex_IQ <- lm(IQ ~ Sex + 0 , data = BiMonlingIQ2)
mdl_sex_IQ
summary(mdl_sex_IQ)

mdl_sex_group_dig <- lm(IQ ~ Sex + Group.name + Digitspanbackward + 0 , data = BiMonlingIQ2)
summary(mdl_sex_group_dig)

#Change group_id to a factor so it is recognized as two categories. 
BiMonlingIQ2$Group.ID <- as.factor(BiMonlingIQ2$Group.ID)
mdl_group_IQ <- lm(IQ ~ Group.ID + 0  , data = BiMonlingIQ2)
mdl_group_IQ
summary(mdl_group_IQ)

mdl_all <- lm(IQ ~  Age + Sex + Group.ID + Education + Stroop.IC + Switch.trial.acc + Digitspanbackward + 0 , data = BiMonlingIQ2)
mdl_all
summary(mdl_all)

mdl_sex_GN <- lm(IQ ~  Sex + Group.name + 0 , data = BiMonlingIQ2)
summary(mdl_sex_GN)
plot(mdl_sex_GN)

mdl_digits <- lm(IQ ~ Digitspanbackward , data = BiMonlingIQ2)
summary(mdl_digits)

mdl_group_digits <- lm(IQ ~ Group.name + Digitspanbackward , data = BiMonlingIQ2)
summary(mdl_group_digits)
#Create explanatory variables to make predictions for. 

explanatory_data1 <- expand_grid(
  Sex = unique(BiMonlingIQ2$Sex),
  Group.ID = unique(BiMonlingIQ2$Group.ID)
)
#Add predictions to the explanatory data. 
prediction_data1 <- explanatory_data1 %>%
  mutate(
    IQ = predict(mdl_sex_group,explanatory_data1)
  )
prediction_data1
#Set up explanatory data tibble(dataframe) for Age, sex,group, and IQ
explanatory_data2 <- expand_grid(
  Digitspanbackward = 4:10,
  Sex = unique(BiMonlingIQ2$Sex),
  Group.name = unique(BiMonlingIQ2$Group.name)
  
)
explanatory_data2
#Make IQ predictions and add them to the above data. 
prediction_data <- explanatory_data2 %>%
  mutate(
    IQ = predict(mdl_sex_group_dig, explanatory_data2)
  )

prediction_data
#Plot IQ vs age colored by sex and facet wrap group. 
ggplot(BiMonlingIQ2, aes(Digitspanbackward, IQ)) +
  geom_point(aes(color = Sex)) +
  geom_smooth(method = "lm", se=F, col="yellow")+
  geom_point(data = prediction_data, shape = 8, color = "orange")+
  facet_wrap(vars(Group.name)) +
  labs(title = "IQ vs Digit Span Backwards ")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"))

#IQ and predictions for bilingual and monolingual participates. 
ggplot(BiMonlingIQ2, aes( IQ, Group.ID)) +
  geom_point(aes(col = Sex)) +
  geom_smooth(method = "lm", se=F, col="red")+
  geom_point(data = prediction_data1, color = "black", shape = 5, size = 2) +
  labs(title = "IQ for Bilingual vs. Monolingual ")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"))

plot(mon_lin$IQ, ylab = "IQ") +
  title("IQ for Monolingual vs. Bilingual") +
  abline(h = mean(mon_lin$IQ)) +
  points(bi_ling$IQ, col = "red") +
  abline(h = mean(bi_ling$IQ), col = "red") 

  



#Find the mean IQ for the bilingual group and monolingual group.   
BiMonlingIQ2 %>%
  group_by(Group.name) %>%
  summarize(IQ_mean=mean(IQ))
#Find the average IQ for males and females. 
BiMonlingIQ2 %>%
  group_by(Sex) %>%
  summarize(IQ_mean=mean(IQ))
#Find out how many males and how many females participated in the study.
table(BiMonlingIQ2$Sex)
x4 <- BiMonlingIQ2 %>%
  filter(Switch.trial.acc > 93 & Group.name == "Monolingual")
count(x4)
#Average overall IQ 
mean(BiMonlingIQ2$IQ)
#Count number of females with above avg IQ.
 x <- BiMonlingIQ2 %>%
   filter(IQ > 43 & Sex== "female")
 count(x)
 #Count number of males with above avg IQ. 
 x2 <- BiMonlingIQ2 %>%
   filter(IQ > 43 & Sex== "male")
 count(x2) 
#Bilingual participates with above avg IQ.  
topBi <- BiMonlingIQ2 %>%
   filter(IQ > 43 & Group.name == "Bilingual")
topBi
table(topBi$Sex)
#Monolingual participates with above avg IQ. 
topMon <- BiMonlingIQ2 %>%
  filter(IQ > 43 & Group.name == "Monolingual")
topMon
table(topMon$Sex)


step(mdl_all, direction = "both", data = BiMonlingIQ2, trace = TRUE)
step(mdl_all, direction = "forward", data = BiMonlingIQ2, trace = TRUE)
step(mdl_all, direction = "backward", data = BiMonlingIQ2, trace = TRUE)
