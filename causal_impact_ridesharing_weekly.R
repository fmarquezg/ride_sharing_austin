# Causal Impact Ridesharing
library(CausalImpact)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(ggthemes)
library(scales)
library(reshape2)



setwd("~/Documents/github/ride_sharing_austin")


df<-read_csv('crime_reports.csv')

df<-df%>%mutate(date = as.Date(`Occurred Date`,'%m/%d/%Y'))%>%
  filter(date>=as.Date('2015-01-01'), date<=as.Date('2016-12-31'))%>%
  rename(crime_type = `Highest Offense Description`)%>%
  mutate(crime_type = case_when(crime_type == 'DUI AGE 17 to 20' ~ 'DWI_DUI',
                                crime_type == 'DWI .15 BAC OR ABOVE' ~ 'DWI_DUI',
                                crime_type == 'DWI .CHILD PASSENGER' ~ 'DWI_DUI',
                                crime_type == 'DRUG RECOGNITION EXPERT' ~ 'DWI_DUI',
                                crime_type == 'DWI 2ND' ~ 'DWI_DUI',
                                crime_type == 'DWI' ~ 'DWI_DUI',
                                TRUE ~ crime_type))


top_crime<-df%>%group_by(crime_type)%>%summarize(n=n())%>%arrange(desc(n))%>%head(20)%>%select(crime_type)
top_crime<-top_crime$crime_type
df <- df%>%filter(crime_type %in%top_crime)

sel_crimes <-c('DWI_DUI','PUBLIC INTOXICATION','CRIMINAL MISCHIEF','FAMILY DISTURBANCE','AUTO THEFT','ASSAULT BY CONTACT')
df_select<-df%>%filter(crime_type%in%sel_crimes)





df_crimes<-df_select%>%select(date,crime_type)%>%arrange(date)%>%
  mutate(week_num = week(date),year=year(date))%>%
  group_by(week_num,year,crime_type)%>%summarize(events=n(),date=min(date))%>%
  arrange(date)

df_crimes%>%head(20)

df_crimes%>%
  ggplot(aes(x=date,y=events,color=crime_type))+geom_point(alpha=0.2)+geom_smooth()+
  geom_vline(xintercept = as.Date('2016-05-01'),linetype='longdash',colour = 'orange')+
  annotate("text", x = as.Date('2016-05-15'), y = 1, label = "Uber/Lyft\nleave")+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b-%Y"))+
  labs(x='',y='Daily Arrests',title='Weekly Arrests by Crime in Austin Texas',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF",color='Crime')+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

df_crimes_spread<-df_crimes%>%spread(crime_type,events)%>%arrange(date)

df_crimes_spread%>%summary()

DWI<-df_crimes_spread%>%select(`DWI_DUI`)
DWI<-DWI$DWI_DUI

PI<-df_crimes_spread%>%select(`PUBLIC INTOXICATION`)
PI<-PI$`PUBLIC INTOXICATION`

#
ABC<-df_crimes_spread%>%select(`ASSAULT BY CONTACT`)
ABC<-ABC$`ASSAULT BY CONTACT`

AT<-df_crimes_spread%>%select(`AUTO THEFT`)
AT<-AT$`AUTO THEFT`

CM<-df_crimes_spread%>%select(`CRIMINAL MISCHIEF`)
CM<-CM$`CRIMINAL MISCHIEF`

FD<-df_crimes_spread%>%select(`FAMILY DISTURBANCE`)
FD<-FD$`FAMILY DISTURBANCE`

dates_d<-df_crimes_spread$date

pre.period <- as.Date(c("2015-01-01", "2016-04-30"))
post.period <- as.Date(c("2016-05-01", "2016-12-31"))


# Create zoo object
data <- zoo(cbind(DWI,PI, ABC, AT, CM,FD), dates_d)
data<-na.fill(data,0)


matplot(data, type = "l")
impact <- CausalImpact(data, pre.period, post.period,alpha=0.1,model.args = list(niter = 1000,nseasons = 52, prior.level.sd=0.1))
# Customize a plot
plot(impact)+
  labs(x='',y='',title='Causal Impact Analysis of RideSharing\nWeekly binning',subtitle='Data range: 2015-01-01 to 2016-12-31',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF",color='Crime')+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

summary(impact, "report")


