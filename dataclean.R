library(dplyr) #data tool;
library(RCurl) #read data url;
setwd("C:/Users/kuan liu/Dropbox (Personal)/STAT_consulting/covidvisual/canada_dash")

#------------------ Read Canada data ------------------
# #read raw github data from the working group github;
x1 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv") #new time series data, apr13;
x2 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/mortality_timeseries_prov.csv") #new time series data, apr13;
x3 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/recovered_cumulative.csv")
x4 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/testing_cumulative.csv")
# x5 <- getURL("https://raw.githubusercontent.com/Kuan-Liu/canada_dash/master/docs/data/recovered_cumulative.csv")
can_c <- read.csv(text=x1, header = TRUE, sep = ",", encoding = 'UTF-8')
can_d <- read.csv(text=x2, header = TRUE, sep = ",", encoding = 'UTF-8')
can_r <- read.csv(text=x3, header = TRUE, sep = ",", encoding = 'UTF-8')
can_t <- read.csv(text=x4, header = TRUE, sep = ",", encoding = 'UTF-8')

# newdataline<- data.frame(date_recovered="06-05-2020",province="Yukon",cumulative_recovered=11)
# can_r<-rbind(can_r, newdataline)

`%>%` <- magrittr::`%>%`
#------------------ canada data formating ------------------
#format dates;
can_c$date_report<-as.Date(can_c$date_report,format="%d-%m-%y")
can_d$date_death_report<-as.Date(can_d$date_death_report,format="%d-%m-%y")
can_r$date_recovered<-as.Date(can_r$date_recovered,format="%d-%m-%y")
can_t$date_testing<-as.Date(can_t$date_testing,format="%d-%m-%y")



#format province labels;
province_labelc<-c("Alberta","British Columbia","Manitoba","New Brunswick", "Newfoundland and Labrador", "Nova Scotia","Nunavut","NorthWest", "Ontario","Prince Edward Island","Quebec", "Repatriated","Saskatchewan","Yukon")
province_labeld<-c("Alberta","British Columbia","Manitoba","New Brunswick", "Newfoundland and Labrador", "Nova Scotia","Nunavut","NorthWest", "Ontario","Prince Edward Island","Quebec","Saskatchewan","Yukon")

levels(can_c$province)<-province_labelc
levels(can_d$province)<-province_labeld
levels(can_r$province)<-province_labelc
levels(can_t$province)<-province_labeld

#aggregate counts by date combining all province;
can_c_daily <- can_c  %>% group_by(date_report) %>% summarise(c_daily=sum(cases, na.rm = T)) 
can_d_daily <- can_d  %>% group_by(date_death_report) %>% summarise(d_daily=sum(deaths, na.rm = T)) 
can_r_daily <- can_r  %>% group_by(date_recovered) %>% summarise(r_cum=sum(cumulative_recovered, na.rm = T)) 
can_t_daily <- can_t  %>% group_by(can_t$date_testing) %>% summarise(t_cum=sum(cumulative_testing, na.rm=T)) 

#merge all data by dates in a canada dataset;
#this data can be shared on the dashboard; #we can also share the canada data by province;
can<-merge(can_c_daily, can_d_daily, by.x = "date_report",by.y="date_death_report",all.x = T)
can<-merge(can, can_r_daily, by.x = "date_report",by.y="date_recovered",all.x = T)
can<-merge(can, can_t_daily, by.x = "date_report", by.y="can_t$date_testing",all.x = T)
can[is.na(can)]<-0  #give value zero for missing;

can <- can %>% mutate(c_cum = cumsum(c_daily)) #cumulative level;
can <- can %>% mutate(d_cum = cumsum(d_daily)) #cumulative level;
can$a_cum<-can$c_cum - can$r_cum - can$d_cum


#getting daily recovered;
can$r_lag<-lag(can$r_cum)
can$r_lag[is.na(can$r_lag)]<-0
can$r_daily<-can$r_cum-can$r_lag

write.csv(can, "docs/data/can.csv",row.names = F)

#------------Marc Olivier plots data prep ------------
can_p<-merge(can_c, can_d[,c("province","date_death_report","deaths")], 
             by.x = c("date_report","province"),
             by.y=c("date_death_report","province"), all.x = T)

can_p[is.na(can_p)]<-0 #only do this on daily value;
# can_p <- can_p  %>% group_by(province) %>% mutate(c_cum = cumsum(c_daily)) #cumulative level;
can_p <- can_p  %>% group_by(province) %>% mutate(d_cum = cumsum(deaths)) #cumulative level;

can_p<-merge(can_p, can_r, 
             by.x = c("date_report","province"),
             by.y=c("date_recovered","province"), all = T)
can_p[is.na(can_p)]<-0 

can_p<-merge(can_p, can_t[,c("date_testing","province","cumulative_testing")], 
             by.x = c("date_report","province"),
             by.y=c("date_testing","province"), all = T)

can_p[is.na(can_p)]<-0
names(can_p)[3:8]<-c("c_daily","c_cum","d_daily","d_cum","r_cum","t_cum")

# can_p[1442,7]<-11

write.csv(can_p, "docs/data/can_p.csv",row.names = F)


world<-read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", header = T,
                sep = ",", encoding = 'UTF-8')


world_clean<- world %>% filter(country %in% c("US","United Kingdom","Australia","Germany","Spain","Italy","Switzerland"), 
                               type == "confirmed") 

world_clean <- world_clean %>% select(country, date, cases)

write.csv(world_clean, "docs/data/world_clean.csv", row.names = F)
# C:/Users/kuan liu/Dropbox (Personal)/STAT_consulting/covidvisual/canada_dash/

x6<-getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv")
x7<-getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_hr/mortality_timeseries_hr.csv")

hr_c <- read.csv(text=x6, header = TRUE, sep = ",", encoding = 'UTF-8')
hr_d <- read.csv(text=x7, header = TRUE, sep = ",", encoding = 'UTF-8')

hr_c_clean <- hr_c %>% filter(province %in% c("Ontario","Quebec")) 

hr_d_clean <- hr_d %>% filter(province %in% c("Ontario","Quebec")) 

write.csv(hr_c_clean, "docs/data/hr_c_clean.csv", row.names = F, fileEncoding = 'UTF-8')
# C:/Users/kuan liu/Dropbox (Personal)/STAT_consulting/covidvisual/canada_dash/
write.csv(hr_d_clean, "docs/data/hr_d_clean.csv", row.names = F, fileEncoding = 'UTF-8')

#------------trajectory data ------------

world <- read.csv("docs/data/world_clean.csv", header = TRUE, sep = ",", encoding = 'UTF-8')

df_us <- world %>% dplyr::filter( country == "US") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(us = cumsum(cases)) %>%
  dplyr::filter(us > 100)  %>%
  dplyr::select(-cases, -date)

df_us$index <- 1:nrow(df_us)


df_uk <- world %>% dplyr::filter( country == "United Kingdom") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(uk = cumsum(cases)) %>%
  dplyr::filter(uk > 100)  %>%
  dplyr::select(-cases, -date)

df_uk$index <- 1:nrow(df_uk)

df_au <- world %>% dplyr::filter( country == "Australia") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(au = cumsum(cases)) %>%
  dplyr::filter(au > 100)  %>%
  dplyr::select(-cases, -date)

df_au$index <- 1:nrow(df_au)

df_ge <- world %>% dplyr::filter( country == "Germany") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(ge = cumsum(cases)) %>%
  dplyr::filter(ge > 100)  %>%
  dplyr::select(-cases, -date)

df_ge$index <- 1:nrow(df_ge)

df_sp <- world %>% dplyr::filter( country == "Spain") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(sp = cumsum(cases)) %>%
  dplyr::filter(sp > 100)  %>%
  dplyr::select(-cases, -date)

df_sp$index <- 1:nrow(df_sp)


df_it <- world %>% dplyr::filter( country == "Italy") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(it = cumsum(cases)) %>%
  dplyr::filter(it > 100)  %>%
  dplyr::select(-cases, -date)

df_it$index <- 1:nrow(df_it)

df_sw <- world %>% dplyr::filter( country == "Switzerland") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(sw = cumsum(cases)) %>%
  dplyr::filter(sw > 100)  %>%
  dplyr::select(-cases, -date)

df_sw$index <- 1:nrow(df_sw)


#------------Canada data this is for the trajectory plot------------
c_cum<- can$c_cum[can$c_cum>100]
df_can<-data.frame(c_cum[complete.cases(c_cum)], 1:length(c_cum[complete.cases(c_cum)]))
names(df_can)<-c("can","index")

df_trajectory <- df_it %>% 
  dplyr::left_join(df_us, by = "index") %>%
  dplyr::left_join(df_can, by = "index") %>%
  dplyr::left_join(df_uk, by = "index") %>%
  dplyr::left_join(df_ge, by = "index") %>%
  dplyr::left_join(df_sw, by = "index") %>%
  dplyr::left_join(df_sp, by = "index") %>%
  dplyr::left_join(df_au, by = "index") 


#------------provincial data this is for the province trajectory plot------------
#now getting data for provinces, only those with more than 50 cases;

ab_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Alberta")  %>% dplyr::select(cumulative_cases)
ab_cum$index <- 1:nrow(ab_cum)  
names(ab_cum)[1]<-"ab"

bc_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="British Columbia")  %>% dplyr::select(cumulative_cases)
bc_cum$index <- 1:nrow(bc_cum)  
names(bc_cum)[1]<-"bc"

on_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Ontario")  %>% dplyr::select(cumulative_cases)
on_cum$index <- 1:nrow(on_cum)  
names(on_cum)[1]<-"on"

qc_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Quebec")  %>% dplyr::select(cumulative_cases)
qc_cum$index <- 1:nrow(qc_cum)  
names(qc_cum)[1]<-"qc"

mb_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Manitoba")  %>% dplyr::select(cumulative_cases)
mb_cum$index <- 1:nrow(mb_cum)  
names(mb_cum)[1]<-"mb"

nb_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="New Brunswick")  %>% dplyr::select(cumulative_cases)
nb_cum$index <- 1:nrow(nb_cum)  
names(nb_cum)[1]<-"nb"

nl_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Newfoundland and Labrador")  %>% dplyr::select(cumulative_cases)
nl_cum$index <- 1:nrow(nl_cum)  
names(nl_cum)[1]<-"nl"

ns_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Nova Scotia")  %>% dplyr::select(cumulative_cases)
ns_cum$index <- 1:nrow(ns_cum)  
names(ns_cum)[1]<-"ns"

sk_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Saskatchewan")  %>% dplyr::select(cumulative_cases)
sk_cum$index <- 1:nrow(sk_cum)  
names(sk_cum)[1]<-"sk"

df_trajectory_can <- df_can %>% 
  dplyr::left_join(on_cum, by = "index") %>%
  dplyr::left_join(qc_cum, by = "index") %>%
  dplyr::left_join(ab_cum, by = "index") %>%
  dplyr::left_join(bc_cum, by = "index") %>%
  dplyr::left_join(ns_cum, by = "index") %>%
  dplyr::left_join(sk_cum, by = "index") %>%
  dplyr::left_join(nl_cum, by = "index") %>%
  dplyr::left_join(mb_cum, by = "index") %>%
  dplyr::left_join(nb_cum, by = "index") 

write.csv(df_trajectory, "docs/data/df_trajectory.csv",row.names = F)
write.csv(df_trajectory_can, "docs/data/df_trajectory_can.csv",row.names = F)

