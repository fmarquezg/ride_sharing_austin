# Austin Crime Report
library(httr)
library(jsonlite)
library(nnet)
library(twitteR)
library(stringr)
library(lubridate)
library(tidyverse)


#Code to Pull data
#df<-NULL
#s<-seq(0:1000000)
#for (i in s){
#  i<-as.character(i*1000)
#  query_url<-paste0('https://data.austintexas.gov/resource/fdj4-gpfu.json?$limit=1000&$offset=',i)
#  try(
#  dat<-fromJSON(query_url,flatten = TRUE)
#  )
#  try(
#  df<-rbind(dat,df)
#  )
#}
# https://www.kut.org/post/have-ride-hailing-services-uber-and-lyft-reduced-drunk-driving-arrests-austin
# https://www.kut.org/post/uber-and-lyft-say-theyll-hit-road-austin-again-monday

# Dallas Report
#https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7
#
# Uber and Lyft first came to Austin in 2013, but it wasn’t until October 2014 that the city passed rules allowing their operation
# UBER/LYFT pull out May 2016
# UBER/LYFT RETURNS TO AUSTIN MAY 29, 2017
#
#

# stored locally to avoid running this again.
df<-read_csv('Crime_Reports.csv')
df%>%filter(`Highest Offense Description` %in%c('DWI','PUBLIC INTOXICATION'))%>%write_csv('atx_dwi_pi.csv')


df%>%group_by(`Highest Offense Description`)%>%summarize(n=n())%>%arrange(-n)%>%head(20)

  

df%>%
  rename(crime_type=`Highest Offense Description`,
         occ_datetime=`Occurred Date Time`)%>%
  mutate(datetime=mdy_hms(occ_datetime))%>%
  mutate(date=date(datetime))%>%
  filter(crime_type%in%c('DWI','PUBLIC INTOXICATION'))%>%
  mutate(year=year(date))%>%
  filter(date>'2013-12-31')%>%
  group_by(year,crime_type)%>%
  summarize(n=n())

# Plot DWIs in Austin
df%>%
  rename(crime_type=`Highest Offense Description`,
         occ_datetime=`Occurred Date Time`)%>%
  mutate(datetime=mdy_hms(occ_datetime))%>%
  mutate(date=date(datetime))%>%
  filter(crime_type%in%c('DWI','PUBLIC INTOXICATION'))%>%
  filter(date>'2014-10-01')%>%
  mutate(fdate=floor_date(date,unit='month'))%>%
  mutate(rs = case_when(date>='2017-05-29'~1,
                       date<='2016-05-01'~1,
                       TRUE ~ 0))%>%
  group_by(crime_type,date,crime_type,rs)%>%summarise(events=n())%>%select(date,events,crime_type,rs)%>%
  ggplot(aes(x=date,y=events,color=as.factor(rs)))+geom_point()



mly_dat<-df%>%
  rename(crime_type=`Highest Offense Description`,
         occ_datetime=`Occurred Date Time`)%>%
  mutate(datetime=mdy_hms(occ_datetime))%>%
  mutate(date=date(datetime))%>%
  filter(date>'2014-10-01')%>%
  filter(crime_type%in%c('DWI','PUBLIC INTOXICATION'))%>%
  mutate(fdate=floor_date(date,unit='month'))%>%
  mutate(rs = case_when(date>='2017-05-29'~0,
                        date<='2016-05-01'~0,
                        TRUE ~ 1))%>%
  mutate(rs=as.factor(rs))%>%
  group_by(crime_type,fdate,rs)%>%summarise(events=n())%>%select(fdate,crime_type,rs,events)%>%ungroup()%>%
  mutate(tl = 1:n())

mly_dat%>%head()

#post = no rideshare
mly_dat%>%
  ggplot(aes(x=fdate,y=events,color=crime_type))+geom_point()

mly_dat%>%
  ggplot(aes(x=fdate,y=events,color=rs))+geom_point()+facet_wrap(~crime_type)+
  labs(x='Date',y='Arrests',title='Austin Arrest Cases',color='RideShare Unavailable',subtitle = '')



#Difference in Difference Approach
data<-mly_dat%>%filter(fdate<='2017-06-01')%>%
  mutate(crime_type = factor(crime_type,levels = c("PUBLIC INTOXICATION","DWI")))%>%
  mutate(D = ifelse(crime_type=='DWI',1,0))

dd<-lm(formula=events ~ D + rs + rs*D, data=data)
summary(dd)


lm1<-lm(events~fdate+rs+tl, data=mly_dat)

summary(lm1)




d_df<-read_csv('Dallas_Police_incidents.csv')
d_df%>%filter(`Type of Incident`=='DWI')%>%glimpse()

d_df%>%
  rename(crime_type=`Type of Incident`,
         occ_datetime=`Date incident created`)%>%
  mutate(datetime=mdy_hms(occ_datetime))%>%
  mutate(date=date(datetime))%>%
  filter(crime_type=='DWI')%>%
  filter(date>'2014-10-01')%>%
  mutate(fdate=floor_date(date,unit='month'))%>%
  mutate(rs = case_when(date>='2017-05-29'~1,
                        date<='2016-05-01'~1,
                        TRUE ~ 0))%>%
  group_by(crime_type,fdate,rs)%>%summarise(events=n())%>%select(fdate,events,crime_type,rs)%>%
  ggplot(aes(x=fdate,y=events,color=as.factor(rs)))+geom_point()

