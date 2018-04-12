
setwd("C:/Users/crabtreec/Desktop/R Projects/Research Log Project")

## Make dataset an object
partners <- read.csv("Research Log Project - working.csv", header = TRUE)

## Show variable names
names(partners)

## Summary of variables
summary(partners)

##### Convert Industry, Industry, contact improvement, Initial Contact Level, Final Contact Level and ResearchedSince1_16 to Factors (not sure this is necessary)
partners$Industry <- factor(partners$Industry)
partners$ResearchedSince1_16 <- factor(partners$ResearchedSince1_16)
partners$IndustryCode2 <- factor(partners$IndustryCode2)
partners$Contact_Improvement <- factor(partners$Contact_Improvement)
partners$Initial_Contact_Level <- factor(partners$Initial_Contact_Level)
partners$Final_Contact_Level <- factor(partners$Final_Contact_Level)

## Run Regression
mylogit <- glm(Partner ~ Industry + IndustryCode2 + ResearchedSince1_16 + Initial_Contact_Level + Final_Contact_Level + Days_Until_Final_Contact + Contact_Improvement + Revenue_Millions, data = partners, family = "binomial")
mylogit2 <- glm(Partner ~ ResearchedSince1_16, data = partners, family = "binomial")
mylogit3 <- glm(Partner ~ Initial_Contact_Level + Final_Contact_Level + Days_Until_Final_Contact + Contact_Improvement + Revenue_Millions, data = partners, family = "binomial")
mylogit4 <- glm(Partner ~ Initial_Contact_Level, data = partners, family = "binomial")
mylogit5 <- glm(Partner ~ Final_Contact_Level, data = partners, family = "binomial")
mylogit6 <- glm(Partner ~ Revenue_Millions, data = partners, family = "binomial")
mylogit7 <- glm(Partner ~ Contact_Improvement, data = partners, family = "binomial")
mylm <- lm(Days_Until_Final_Contact ~ Revenue_Millions, data = partners)




## Run Summary
summary(mylogit)
summary(mylogit2)
summary(mylogit3)
summary(mylogit4)
summary(mylogit5)
summary(mylogit6)
summary(mylogit7)
summary(mylm)

## CI using profiled log-likelihood
confint(mylogit)
confint(mylogit2)
confint(mylogit3)
confint(mylogit4)
confint(mylogit5)
confint(mylogit6)

## CI using standard errors
confint.default(mylogit)
confint.default(mylogit2)
confint.default(mylogit3)
confint.default(mylogit4)
confint.default(mylogit5)
confint.default(mylogit6)

#Odds ratio only
exp(coef(mylogit))
exp(coef(mylogit2))
exp(coef(mylogit3))
exp(coef(mylogit4))
exp(coef(mylogit5))
exp(coef(mylogit6))


## Odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))
exp(cbind(OR = coef(mylogit3), confint(mylogit3)))
exp(cbind(OR = coef(mylogit4), confint(mylogit4)))
exp(cbind(OR = coef(mylogit5), confint(mylogit5)))
exp(cbind(OR = coef(mylogit6), confint(mylogit6)))


###
###
###

library(tidyverse)

###
###  Aggregating/Grouping Numbers for basic statistics
###

test <- partners %>%
  group_by(Partner , Initial_Contact_Level,) %>%
  summarise(DUDE = n()) %>%
  arrange(Partner, Initial_Contact_Level)

Partner_Agg_Numbers <- partners %>%
  group_by(Partner) %>%
  summarise(N = n()) 

Initial_Contact_Agg_Numbers <- partners %>%
  group_by(Initial_Contact_Level) %>%
  summarise(N = n()) 


Days_Contact_Check <- partners %>%
  group_by(Partner) %>%
  summarise(N = n() , 
            Mean_Days = mean(Days_Until_Final_Contact) , 
            Median_Days = median(Days_Until_Final_Contact) , 
            SD_Days = sd(Days_Until_Final_Contact))

###
###  Subsetting Data
###

Only_Partners <- partners %>%
  filter(Partner == 1)

Quick_Accounts <- partners %>%
  filter(Days_Until_Final_Contact < 90)

Quick_Partners <- partners %>%
  filter(Days_Until_Final_Contact < 90 & Partner == 1)

Quick_Partners_2 <- partners %>%
  filter(Days_Until_Final_Contact < 90 ) %>%
  filter(Partner == 1) %>%
  group_by(Initial_Contact_Level) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  mutate(Example = sum(N))


partners_revenue_normalized <- partners %>%
  filter(!is.na(Revenue_Millions)) %>%
  filter(Revenue_Millions > 0 ) %>%
  mutate(log_revenue = log(Revenue_Millions)) %>%
  mutate(percentile_group_5 = ntile(Revenue_Millions , 5))

test_glm <- glm(Partner ~ percentile_group_5 , data = partners_revenue_normalized , family = 'binomial')



###
###  
###

Ntile_Example <- partners %>%
  mutate(Days_Percentile = ntile(Days_Until_Final_Contact , 4))

###
###
###

GG_Days <- ggplot(d)

GG_Days <- ggplot(data = partners, aes(x = Days_Until_Final_Contact)) + 
  geom_histogram(color = 'black' , fill = 'dark green') +
  facet_grid(Partner ~ . )


###
###
###


Small_Cell_Example <- partners %>%
  group_by(Partner , ResearchedSince1_16) %>%
  summarise(N = n()) %>%
  spread(Partner , N)
