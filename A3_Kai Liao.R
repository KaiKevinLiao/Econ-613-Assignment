library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)


#############
# Excercise 1
#############

dpop<-read.csv("C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A2\\population.csv")
dcrime<-read.csv("C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A2\\crime_long.csv")

#############
# Excercise 2
#############

dcrime <- dcrime %>% 
  group_by(crime_month, district, crime_type) %>%
  summarise(crimes=sum(crimes)) %>% tibble()

dcrime$crime_month <- as.Date(dcrime$crime_month)


dcrime %>%
  select(crime_month, crimes) %>%
  group_by(by=crime_month) %>% summarise(crimes.sum=sum(crimes)) %>%
  mutate(month=by) %>%
  ggplot(aes(month, crimes.sum)) +
  geom_line() +
  xlab("time") +
  ylab("crimes")



dcrime$month1<-month(dcrime$crime_month)
dcrime$year<-year(dcrime$crime_month)
dcrime$ym<-paste(dcrime$year,dcrime$month1,sep="/")

dcrime<-rename(dcrime,month=crime_month)
crime_pop1<-merge(dcrime,dpop,by=c("month","district"))

crime_pop1 <-crime_pop1 %>%
  mutate(crime.res.type=crimes/tot_pop) %>%
  mutate(violent.res= ifelse(crime_type=="violent",crime.res.type,0)) %>%
  mutate(property.res= ifelse(crime_type=="property",crime.res.type,0))

crime_pop2 <-crime_pop1 %>%  
  group_by(ym,district) %>% mutate(crimes.res=sum(crime.res.type))

crime_pop2 <-crime_pop2 %>% 
  mutate(black.share=tot_black/tot_pop)%>%
  mutate(hisp.share=tot_hisp/tot_pop)%>%
  mutate(white.share=tot_white/tot_pop)

population.crime <- crime_pop2 %>% drop_na() %>%
  select(-tot_pop, -tot_white, -tot_black, -tot_hisp) %>%
  select(district, everything()) %>% arrange(district, month)

population.crime2 <- crime_pop2 %>% 
  group_by(ym,district) %>% mutate(tot.crime=sum(crimes)) %>% 
  filter(crime_type=="drug") %>% 
  select(-crime_type,-violent.res,-property.res)

#############
# Excercise 3
#############

doff<-read.csv("C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A2\\officers.csv")
doff$month<-as.Date(doff$month)
doff<-rename(doff,district=unit)
office.crime<-merge(doff,population.crime2,by=c("month","district"))


vm1 <- lm(arrest ~ tenure + p50_inc + tot.crime + black.share + hisp.share + white.share, data = office.crime)

pool <- lm(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime, data = office.crime)

#############
# Excercise 4
#############

fe <- lm(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime +factor(month1) + factor(year) + factor(unit) - 1, data = office.crime)

#############
# Excercise 5
#############

plm2<-plm(arrest ~ tenure + p50_inc + tot.crime + black.share + hisp.share + white.share, data = office.crime, model = "within", index = c("NUID","month", "district"))

plm3<-plm(arrest ~ tenure + p50_inc + tot.crime + black.share + hisp.share + white.share, data = office.crime, model = "between", index = c("NUID","month", "district"))

plm4<-plm(arrest ~ tenure + p50_inc + tot.crime + black.share + hisp.share + white.share, data = office.crime, model = "fd", index = c("NUID","month", "district"))

plm5<-pgmm(arrest ~ tenure + p50_inc + tot.crime + black.share + hisp.share + white.share, data = office.crime, index = c("NUID","month", "district"), transformation = "ld")







