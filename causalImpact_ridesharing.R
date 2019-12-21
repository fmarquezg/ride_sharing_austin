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
  filter(date>=as.Date('2015-07-01'), date<=as.Date('2016-07-31'))%>%
  rename(crime_type = `Highest Offense Description`)%>%
  mutate(crime_type = case_when(crime_type == 'DUI AGE 17 to 20' ~ 'DWI_DUI',
                                crime_type == 'DWI .15 BAC OR ABOVE' ~ 'DWI_DUI',
                                crime_type == 'DWI .CHILD PASSENGER' ~ 'DWI_DUI',
                                crime_type == 'DRUG RECOGNITION EXPERT' ~ 'DWI_DUI',
                                crime_type == 'DWI 2ND' ~ 'DWI_DUI',
                                crime_type == 'DWI' ~ 'DWI_DUI',
                                TRUE ~ crime_type))

df%>%glimpse()

top_crime<-df%>%group_by(crime_type)%>%summarize(n=n())%>%arrange(desc(n))%>%head(20)%>%select(crime_type)
top_crime<-top_crime$crime_type
df <- df%>%filter(crime_type %in%top_crime)


# See which crimes are similar
df%>%group_by(date,crime_type)%>%summarize(events=n())%>%
  ggplot(aes(x=date,y=events))+geom_point(alpha=0.2,color='#8C39E1')+geom_smooth(color='#4CC415')+facet_wrap(~crime_type, scales = 'free_y')+
  geom_vline(xintercept = as.Date('2016-05-01'),linetype='longdash',colour = 'orange')+
  #annotate("text", x = as.Date('2016-05-15'), y = 15, label = "Uber/Lyft\nleave")+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b-%Y"))+
  labs(x='',y='Arrests',title='Crime Arrests in Austin Texas',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF")+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))


sel_crimes <-c('DWI_DUI','PUBLIC INTOXICATION','CRIMINAL MISCHIEF','FAMILY DISTURBANCE','AUTO THEFT','ASSAULT BY CONTACT')
#sel_crimes <-c('DWI_DUI','PUBLIC INTOXICATION')
df_select<-df%>%filter(crime_type%in%sel_crimes)

df_select%>%group_by(date,crime_type)%>%summarize(events=n())%>%
  ggplot(aes(x=date,y=events,color=crime_type))+geom_point(alpha=0.2)+geom_smooth()+
  geom_vline(xintercept = as.Date('2016-05-01'),linetype='longdash',colour = 'orange')+
  annotate("text", x = as.Date('2016-05-15'), y = 15, label = "Uber/Lyft\nleave")+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b-%Y"))+
  labs(x='',y='Daily Arrests',title='Crime Arrests in Austin Texas',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF",color='Crime')+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))



df_crimes <-df_select%>%group_by(date,crime_type)%>%summarize(events=n())
#Get dataframe with date
date_col<-seq(as.Date('2015-07-01'),by='day',length.out = 2000)
day_df_dwi<-data.frame(date_col)%>%filter(date_col<=as.Date('2016-07-31'))

df_crimes_spread<-df_crimes%>%spread(crime_type,events)%>%
  filter(date>=as.Date('2016-02-20'), date<='2016-06-01')

# Get Crime Series in a very dumb way
DWI<-df_crimes_spread%>%select(`DWI_DUI`)
DWI<-DWI$DWI_DUI

PI<-df_crimes_spread%>%select(`PUBLIC INTOXICATION`)
PI<-PI$`PUBLIC INTOXICATION`

ABC<-df_crimes_spread%>%select(`ASSAULT BY CONTACT`)
ABC<-ABC$`ASSAULT BY CONTACT`

AT<-df_crimes_spread%>%select(`AUTO THEFT`)
AT<-AT$`AUTO THEFT`

CM<-df_crimes_spread%>%select(`CRIMINAL MISCHIEF`)
CM<-CM$`CRIMINAL MISCHIEF`

FD<-df_crimes_spread%>%select(`FAMILY DISTURBANCE`)
FD<-FD$`FAMILY DISTURBANCE`



dates_d<-df_crimes_spread$date

#Create Zoo object
data <- zoo(cbind(DWI,PI, ABC, AT, CM,FD), dates_d)
data<-na.fill(data,0)

#pre.period <- as.Date(c("2015-07-01", "2016-04-30"))
#post.period <- as.Date(c("2016-05-01", "2016-07-31"))
pre.period <- as.Date(c("2016-02-20", "2016-04-30"))
post.period <- as.Date(c("2016-05-01", "2016-06-01"))

matplot(data, type = "l")

# Run CI analysis, notice nseasons=7 because we are using daily data
impact <- CausalImpact(data, pre.period, post.period,alpha=0.1,model.args = list(niter = 10000,nseasons = 7, season.duration = 1, prior.level.sd=0.1))
# Customize a plot
plot(impact)+
  labs(x='',y='',title='Causal Impact Analysis of RideSharing',subtitle='Data range: 2016-02-20 to 2016-06-01',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF",color='Crime')+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

impact.plot <- plot(impact)
impact.plot <- impact.plot + theme_bw(base_size = 10)
impact.plot
summary(impact)
summary(impact, "report")
impact$summary














#### Same analysis but longer time frame. This yielded similar results
df<-read_csv('crime_reports.csv')

df<-df%>%mutate(date = as.Date(`Occurred Date`,'%m/%d/%Y'))%>%
  filter(date>=as.Date('2015-07-01'), date<=as.Date('2017-07-31'))%>%
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


# See which crimes are similar
df%>%group_by(date,crime_type)%>%summarize(events=n())%>%
  ggplot(aes(x=date,y=events))+geom_point(alpha=0.2,color='#8C39E1')+geom_smooth(color='#4CC415')+facet_wrap(~crime_type, scales = 'free_y')+
  geom_vline(xintercept = as.Date('2016-05-01'),linetype='longdash',colour = 'orange')+
  #annotate("text", x = as.Date('2016-05-15'), y = 15, label = "Uber/Lyft\nleave")+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b-%Y"))+
  labs(x='',y='Arrests',title='Crime Arrests in Austin Texas',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF")+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))


sel_crimes <-c('DWI_DUI','PUBLIC INTOXICATION','CRIMINAL MISCHIEF','FAMILY DISTURBANCE','AUTO THEFT','ASSAULT BY CONTACT')
#sel_crimes <-c('DWI_DUI','PUBLIC INTOXICATION')
df_select<-df%>%filter(crime_type%in%sel_crimes)

df_select%>%group_by(date,crime_type)%>%summarize(events=n())%>%
  ggplot(aes(x=date,y=events,color=crime_type))+geom_point(alpha=0.2)+geom_smooth()+
  geom_vline(xintercept = as.Date('2016-05-01'),linetype='longdash',colour = 'orange')+
  annotate("text", x = as.Date('2016-05-15'), y = 15, label = "Uber/Lyft\nleave")+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b-%Y"))+
  labs(x='',y='Arrests',title='Crime Arrests in Austin Texas',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF")+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))



df_crimes <-df_select%>%group_by(date,crime_type)%>%summarize(events=n())
#Get dataframe with date
date_col<-seq(as.Date('2015-07-01'),by='day',length.out = 2000)
day_df_dwi<-data.frame(date_col)%>%filter(date_col<=as.Date('2017-07-31'))

df_crimes_spread<-df_crimes%>%spread(crime_type,events)%>%
  filter(date>=as.Date('2016-02-20'), date<='2017-07-01')

df_crimes_spread%>%head()


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
#df_crimes_spread

data <- zoo(cbind(DWI,PI, ABC, AT, CM,FD), dates_d)
data<-na.fill(data,0)
#colnames(data) <- gsub(" ","_",colnames(data))
data%>%head()
#pre.period <- as.Date(c("2015-07-01", "2016-04-30"))
#post.period <- as.Date(c("2016-05-01", "2016-07-31"))
pre.period <- as.Date(c("2016-02-20", "2016-04-30"))
post.period <- as.Date(c("2016-05-01", "2017-05-29"))

matplot(data, type = "l")

#data

impact <- CausalImpact(data, pre.period, post.period,alpha=0.1,model.args = list(niter = 10000,nseasons = 7, season.duration = 1, prior.level.sd=0.1))
# Customize a plot
plot(impact)

impact.plot <- plot(impact)
impact.plot <- impact.plot + theme_bw(base_size = 10)
impact.plot
summary(impact)
summary(impact, "report")
impact$summary



### END OF ANALYSIS HERE - The analsyis below uses google trends but was going nowhere either
df<-read_csv('atx_dwi_pi.csv')


a_df<-read_csv('all_arrests_austin_cases.csv')
#a_df%>%glimpse()
#a_df%>%
#  mutate(datetime=mdy_hms(occ_date_time))%>%
#  mutate(date=as.Date(occ_date))%>%
#  mutate(year=year(date))%>%
#  filter(date>=as.Date('2015-01-01'))%>%
#  group_by(year,crime_type)%>%
#  summarize(n=n())%>%arrange(desc(n))%>%view()


#read other cases
getwd()

s1<-read_csv('best_car_austin_gquery.csv')
s2<-read_csv('dwi_lawyer_gquery.csv')
s3<-read_csv('realtor_austin_gquery.csv')
s4<-read_csv('restaurants_austin_gquery.csv')

s1<-s1%>%rename(gs_best_cars = `best cars: (Austin TX)`, date_col = Day)
s2<-s2%>%rename(gs_dwi_laywer = `dwi lawyers: (United States)`, date_col = Day)
s3<-s3%>%rename(gs_realtors = `realtors: (Austin TX)`, date_col = Day)
s4<-s4%>%rename(gs_restaurants = `restaurants: (Austin TX)`, date_col = Day)

date_col<-seq(as.Date('2014-01-01'),by='day',length.out = 2000)
day_df<-data.frame(date_col)
day_df %>% head()

day_df%>%head()
search_queries<-day_df%>%left_join(s1,by=c('date_col'))%>%mutate( gs_best_cars = replace_na(gs_best_cars,0)) %>%
  left_join(s2,by=c('date_col'))%>%mutate( gs_dwi_laywer = replace_na(gs_dwi_laywer,0)) %>%
  left_join(s3,by=c('date_col'))%>%mutate( gs_realtors = replace_na(gs_realtors,0)) %>%
  left_join(s4,by=c('date_col'))%>%mutate( gs_restaurants = replace_na(gs_restaurants,0))
  

#Get dataframe with date
date_col<-seq(as.Date('2014-01-01'),by='day',length.out = 2000)
day_df_dwi<-data.frame(date_col)
day_df_dwi<-day_df_dwi%>%rename(date = date_col)%>%mutate(crime_type='DWI')
day_df_pi<-data.frame(date_col)
day_df_pi<-day_df_pi%>%rename(date = date_col)%>%mutate(crime_type='PUBLIC INTOXICATION')
day_df<-rbind(day_df_dwi,day_df_pi)%>%mutate(crime_type = factor(crime_type, levels = c("PUBLIC INTOXICATION","DWI")))


datad<-df%>%rename(crime_type=`Highest Offense Description`,
         occ_datetime=`Occurred Date Time`)%>%
  mutate(datetime=mdy_hms(occ_datetime))%>%
  mutate(date=date(datetime))%>%
  mutate(month=floor_date(date,unit='month'))%>%
  mutate(rs = case_when((date>='2016-05-01') & (date<'2017-06-01') ~ 0,
                        TRUE ~ 1))%>%
  select(month,date, rs,crime_type)%>%
  mutate(crime_type = factor(crime_type, levels = c("PUBLIC INTOXICATION","DWI"))
  )%>%
  group_by(date,rs,crime_type)%>%summarize(events = n())

datad2<-day_df%>%left_join(datad,by=c('date','crime_type'))%>%
  mutate(events = ifelse(is.na(events),0,events),
         rs = ifelse(is.na(rs),0,rs))%>%
  mutate(rs = as.factor(rs))%>%
  #filter(date>='2015-07-01', date<'2016-08-01')
  filter(date>='2016-02-20', date<'2016-06-01')

search_queries<-search_queries%>%filter(date_col >='2015-07-01',date_col<'2016-08-01')
search_queries%>%head()
gs_bestcars<-search_queries%>%dplyr::select(gs_best_cars)
gs_dwi_laywer<-search_queries%>%dplyr::select(gs_dwi_laywer)
gs_realtors<-search_queries%>%dplyr::select(gs_realtors)
gs_restaurants<-search_queries%>%dplyr::select(gs_restaurants)


search_q_df<-melt(search_queries, id.vars = 'date_col')
search_q_df <- search_q_df%>%rename(date = date_col, crime_type = variable, events = value)
datad3<- rbind(search_q_df, datad2%>%select(-rs))


datad2%>%head()


dwi<-datad2%>%filter(crime_type=='DWI')%>%dplyr::select(events)%>%rename(dwi = events)
pi<-datad2%>%filter(crime_type=='PUBLIC INTOXICATION')%>%dplyr::select(events)%>%rename(pi = events)
dates_d<-datad2%>%filter(crime_type=='PUBLIC INTOXICATION')
dates_d<-dates_d$date
dates_d%>%head()
dates_d%>%tail()

data <- zoo(cbind(dwi, pi), dates_d)
#data <- zoo(cbind(dwi, pi), dates_d)

data%>%head()


#g1<-s1%>%filter(Day>=as.Date('2015-07-01') , Day<=as.Date('2016-08-01'))%>%select(gs_best_cars)
pre.period <- as.Date(c("2016-02-20", "2016-04-30"))
post.period <- as.Date(c("2016-05-01", "2016-06-01"))
#pre.period <- as.Date(c("2015-07-01", "2016-04-30"))
#post.period <- as.Date(c("2016-05-01", "2016-07-31"))

#data
foo<-data.frame(data)
foo%>%head()
impact <- CausalImpact(data, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1, niter=1000,prior.level.sd=0.1))
impact <- CausalImpact(data, pre.period, post.period)
# Customize a plot
plot(impact)
impact.plot <- plot(impact)
impact.plot <- impact.plot
impact.plot
summary(impact)
summary(impact, "report")
impact$summary


datad2%>%ggplot(aes(x=date,y=events,color = crime_type))+geom_point()+geom_smooth()+
  geom_vline(xintercept = as.Date('2016-05-01'),linetype='longdash',colour = 'orange')+
  annotate("text", x = as.Date('2016-05-15'), y = 15, label = "Uber/Lyft\nleave")+
  scale_color_manual(values=c("#8C39E1", "#4CC415"))+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b-%Y"))+
  labs(x='',y='Arrests',title='Arrests Austin Texas',color='Crime', subtitle='',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF")+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

search_q_df%>%head()
datad3%>%ggplot(aes(x=date,y=events,color = crime_type))+geom_point()+geom_smooth()+
  geom_vline(xintercept = as.Date('2016-05-01'),linetype='longdash',colour = 'orange')+
  annotate("text", x = as.Date('2016-05-15'), y = 15, label = "Uber/Lyft\nleave")+
  #scale_color_manual(values=c("#8C39E1", "#4CC415"))+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b-%Y"))+
  labs(x='',y='Arrests',title='Arrests Austin Texas',color='Crime', subtitle='',
       caption = "Data: data.austintexas.gov\nVisual:@TheMarquezF")+theme_classic()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

