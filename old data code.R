#------------------ Packages ------------------
library(shiny) #shiny app;
library(flexdashboard) #dashboard;
library(dplyr) #data tool;
# library(RCurl) #read data url;
library(plotly) #visual tool;
library(tibble) #scrap variable characters;
library(DT) #Datatable;
#------------------ Read Canada data ------------------
# #read raw github data from the working group github;
# x1 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv") #new time series data, apr13;
# x2 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/mortality_timeseries_prov.csv") #new time series data, apr13;
# x3 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/recovered_cumulative.csv")
# x4 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/testing_cumulative.csv")
#read raw github data from my own github;
# x1 <- getURL("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/cases_timeseries_prov.csv") #new time series data, apr13;
# x2 <- getURL("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/mortality_timeseries_prov.csv") #new time series data, apr13;
# x3 <- getURL("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/recovered_cumulative.csv")
# x4 <- getURL("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/testing_cumulative.csv")
# can_c <- read.csv(text=x1, header = TRUE, sep = ",", encoding = 'UTF-8')
# can_d <- read.csv(text=x2, header = TRUE, sep = ",", encoding = 'UTF-8')
# can_r <- read.csv(text=x3, header = TRUE, sep = ",", encoding = 'UTF-8')
# can_t <- read.csv(text=x4, header = TRUE, sep = ",", encoding = 'UTF-8')
# can_c <- read.csv("docs/data/cases_timeseries_prov.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# can_d <- read.csv("docs/data/mortality_timeseries_prov.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# can_r <- read.csv("docs/data/recovered_cumulative.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# can_t <- read.csv("docs/data/testing_cumulative.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# 
# `%>%` <- magrittr::`%>%`
# #------------------ canada data formating ------------------
# #format dates;
# can_c$date_report<-as.Date(can_c$date_report,format="%d-%m-%y")
# can_d$date_death_report<-as.Date(can_d$date_death_report,format="%d-%m-%y")
# can_r$date_recovered<-as.Date(can_r$date_recovered,format="%d-%m-%y")
# can_t$date_testing<-as.Date(can_t$date_testing,format="%d-%m-%y")
# 
# #format province labels;
# province_labelc<-c("Alberta","British Columbia","Manitoba","New Brunswick", "Newfoundland and Labrador", "Nova Scotia","Nunavut","NorthWest", "Ontario","Prince Edward Island","Quebec", "Repatriated","Saskatchewan","Yukon")
# province_labeld<-c("Alberta","British Columbia","Manitoba","New Brunswick", "Newfoundland and Labrador", "Nova Scotia","Nunavut","NorthWest", "Ontario","Prince Edward Island","Quebec","Saskatchewan","Yukon")
# 
# levels(can_c$province)<-province_labelc
# levels(can_d$province)<-province_labeld
# levels(can_r$province)<-province_labeld
# levels(can_t$province)<-province_labeld
# 
# #aggregate counts by date combining all province;
# can_c_daily <- can_c  %>% group_by(date_report) %>% summarise(c_daily=sum(cases, na.rm = T)) 
# can_d_daily <- can_d  %>% group_by(date_death_report) %>% summarise(d_daily=sum(deaths, na.rm = T)) 
# can_r_daily <- can_r  %>% group_by(date_recovered) %>% summarise(r_cum=sum(cumulative_recovered, na.rm = T)) 
# can_t_daily <- can_t  %>% group_by(can_t$date_testing) %>% summarise(t_cum=sum(cumulative_testing, na.rm=T)) 
# 
# #merge all data by dates in a canada dataset;
# #this data can be shared on the dashboard; #we can also share the canada data by province;
# can<-merge(can_c_daily, can_d_daily, by.x = "date_report",by.y="date_death_report",all.x = T)
# can<-merge(can, can_r_daily, by.x = "date_report",by.y="date_recovered",all.x = T)
# can<-merge(can, can_t_daily, by.x = "date_report", by.y="can_t$date_testing",all.x = T)
# can[is.na(can)]<-0  #give value zero for missing;
# 
# can <- can %>% mutate(c_cum = cumsum(c_daily)) #cumulative level;
# can <- can %>% mutate(d_cum = cumsum(d_daily)) #cumulative level;
# can$a_cum<-can$c_cum - can$r_cum - can$d_cum
# 
# 
# #getting daily recovered;
# can$r_lag<-lag(can$r_cum)
# can$r_lag[is.na(can$r_lag)]<-0
# can$r_daily<-can$r_cum-can$r_lag
can<-read.csv("docs/data/can.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
can$date_report<-as.Date(can$date_report,format="%Y-%m-%d")
`%>%` <- magrittr::`%>%`
#------------World data this is for the trajectory plot------------
# world <- read.csv("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/coronavirus_clean.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# world <- read.csv("docs/data/world_clean.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# 
# df_us <- world %>% dplyr::filter( Country.Region == "US") %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(cases = sum(cases)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(us = cumsum(cases)) %>%
#   dplyr::filter(us > 100)  %>%
#   dplyr::select(-cases, -date)
# 
# df_us$index <- 1:nrow(df_us)
# 
# 
# df_uk <- world %>% dplyr::filter( Country.Region == "United Kingdom") %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(cases = sum(cases)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(uk = cumsum(cases)) %>%
#   dplyr::filter(uk > 100)  %>%
#   dplyr::select(-cases, -date)
# 
# df_uk$index <- 1:nrow(df_uk)
# 
# df_au <- world %>% dplyr::filter( Country.Region == "Australia") %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(cases = sum(cases)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(au = cumsum(cases)) %>%
#   dplyr::filter(au > 100)  %>%
#   dplyr::select(-cases, -date)
# 
# df_au$index <- 1:nrow(df_au)
# 
# df_ge <- world %>% dplyr::filter( Country.Region == "Germany") %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(cases = sum(cases)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(ge = cumsum(cases)) %>%
#   dplyr::filter(ge > 100)  %>%
#   dplyr::select(-cases, -date)
# 
# df_ge$index <- 1:nrow(df_ge)
# 
# df_sp <- world %>% dplyr::filter( Country.Region == "Spain") %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(cases = sum(cases)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(sp = cumsum(cases)) %>%
#   dplyr::filter(sp > 100)  %>%
#   dplyr::select(-cases, -date)
# 
# df_sp$index <- 1:nrow(df_sp)
# 
# 
# df_it <- world %>% dplyr::filter( Country.Region == "Italy") %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(cases = sum(cases)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(it = cumsum(cases)) %>%
#   dplyr::filter(it > 100)  %>%
#   dplyr::select(-cases, -date)
# 
# df_it$index <- 1:nrow(df_it)
# 
# df_sw <- world %>% dplyr::filter( Country.Region == "Switzerland") %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(cases = sum(cases)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(sw = cumsum(cases)) %>%
#   dplyr::filter(sw > 100)  %>%
#   dplyr::select(-cases, -date)
# 
# df_sw$index <- 1:nrow(df_sw)
# 
# 
# #------------Canada data this is for the trajectory plot------------
# c_cum<- can$c_cum[can$c_cum>100]
# df_can<-data.frame(c_cum[complete.cases(c_cum)], 1:length(c_cum[complete.cases(c_cum)]))
# names(df_can)<-c("can","index")
# 
# df_trajectory <- df_it %>% 
#   dplyr::left_join(df_us, by = "index") %>%
#   dplyr::left_join(df_can, by = "index") %>%
#   dplyr::left_join(df_uk, by = "index") %>%
#   dplyr::left_join(df_ge, by = "index") %>%
#   dplyr::left_join(df_sw, by = "index") %>%
#   dplyr::left_join(df_sp, by = "index") %>%
#   dplyr::left_join(df_au, by = "index") 
# #------------provincial data this is for province the trajectory plot------------
# #now getting data for provinces, only those with more than 50 cases;
# 
# ab_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Alberta")  %>% dplyr::select(cumulative_cases)
# ab_cum$index <- 1:nrow(ab_cum)  
# names(ab_cum)[1]<-"ab"
# 
# bc_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="British Columbia")  %>% dplyr::select(cumulative_cases)
# bc_cum$index <- 1:nrow(bc_cum)  
# names(bc_cum)[1]<-"bc"
# 
# on_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Ontario")  %>% dplyr::select(cumulative_cases)
# on_cum$index <- 1:nrow(on_cum)  
# names(on_cum)[1]<-"on"
# 
# qc_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Quebec")  %>% dplyr::select(cumulative_cases)
# qc_cum$index <- 1:nrow(qc_cum)  
# names(qc_cum)[1]<-"qc"
# 
# mb_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Manitoba")  %>% dplyr::select(cumulative_cases)
# mb_cum$index <- 1:nrow(mb_cum)  
# names(mb_cum)[1]<-"mb"
# 
# nb_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="New Brunswick")  %>% dplyr::select(cumulative_cases)
# nb_cum$index <- 1:nrow(nb_cum)  
# names(nb_cum)[1]<-"nb"
# 
# nl_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Newfoundland and Labrador")  %>% dplyr::select(cumulative_cases)
# nl_cum$index <- 1:nrow(nl_cum)  
# names(nl_cum)[1]<-"nl"
# 
# ns_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Nova Scotia")  %>% dplyr::select(cumulative_cases)
# ns_cum$index <- 1:nrow(ns_cum)  
# names(ns_cum)[1]<-"ns"
# 
# sk_cum<- dplyr::filter(can_c, cumulative_cases > 50 & province=="Saskatchewan")  %>% dplyr::select(cumulative_cases)
# sk_cum$index <- 1:nrow(sk_cum)  
# names(sk_cum)[1]<-"sk"
# 
# df_trajectory_can <- df_can %>% 
#   dplyr::left_join(on_cum, by = "index") %>%
#   dplyr::left_join(qc_cum, by = "index") %>%
#   dplyr::left_join(ab_cum, by = "index") %>%
#   dplyr::left_join(bc_cum, by = "index") %>%
#   dplyr::left_join(ns_cum, by = "index") %>%
#   dplyr::left_join(sk_cum, by = "index") %>%
#   dplyr::left_join(nl_cum, by = "index") %>%
#   dplyr::left_join(mb_cum, by = "index") %>%
#   dplyr::left_join(nb_cum, by = "index") 
df_trajectory<-read.csv("docs/data/df_trajectory.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
df_trajectory_can<-read.csv("docs/data/df_trajectory_can.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
`%>%` <- magrittr::`%>%`
#------------Marc Olivier plots data prep ------------
# can_p<-merge(can_c, can_d[,c("province","date_death_report","deaths")], 
#              by.x = c("date_report","province"),
#              by.y=c("date_death_report","province"), all.x = T)
# 
# can_p[is.na(can_p)]<-0 #only do this on daily value;
# # can_p <- can_p  %>% group_by(province) %>% mutate(c_cum = cumsum(c_daily)) #cumulative level;
# can_p <- can_p  %>% group_by(province) %>% mutate(d_cum = cumsum(deaths)) #cumulative level;
# 
# can_p<-merge(can_p, can_r, 
#              by.x = c("date_report","province"),
#              by.y=c("date_recovered","province"), all = T)
# can_p[is.na(can_p)]<-0 
# 
# can_p<-merge(can_p, can_t[,c("date_testing","province","cumulative_testing")], 
#              by.x = c("date_report","province"),
#              by.y=c("date_testing","province"), all = T)
# 
# can_p[is.na(can_p)]<-0
# 
# names(can_p)[3:8]<-c("c_daily","c_cum","d_daily","d_cum","r_cum","t_cum")
can_p<-read.csv("docs/data/can_p.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
can_p$date_report<-as.Date(can_p$date_report,format="%Y-%m-%d")
#------------------ Parameters colours  ------------------
# Set colors
# https://www.w3.org/TR/css-color-3/#svg-color
#value box colour;
tested_color <- "#00008B"
positive_color <- "#DC143C"
active_color <- "#1E90FF"
recovered_color <- "forestgreen"
death_color <- "#FF0000"
ab_color<-"#000000"
bc_color<-"#E69F00"
mb_color<-"#800000"
nb_color<-"#CC79A7"
nl_color<-"#56B4E9"
ns_color<-"#808000"
nt_color<-"#625D5D"
nu_color<-"#1589FF"
on_color<-"#009E73"
pe_color<-"#0000A0"
qc_color<-"#0072B2"
sk_color<-"#800080"
yt_color<-"#E56717"
#------------------ creating Ontario data;  ------------------
on<-can_p[can_p$province=="Ontario",]
on$a_cum<-on$c_cum - on$r_cum - on$d_cum
#getting daily recovered;
on$r_lag<-lag(on$r_cum)
on$r_lag[is.na(on$r_lag)]<-0
on$r_daily<-on$r_cum-on$r_lag
on$c_lag<-lag(on$c_cum)
on$d_lag<-lag(on$d_cum)
on$c_lag[is.na(on$c_lag)]<-0
on$d_lag[is.na(on$d_lag)]<-0
on$c_percentchange<-round(on$c_daily/on$c_lag*100,1)
on$d_percentchange<-round(on$d_daily/on$d_lag*100,1)
on$r_percentchange<-round(on$r_daily/on$r_lag*100,1)
on <- on %>% 
  dplyr::mutate(c_daily_smooth = (c_daily +
                                    dplyr::lag(c_daily, n = 1) +
                                    dplyr::lag(c_daily, n = 2) +
                                    dplyr::lag(c_daily, n = 3) +
                                    dplyr::lag(c_daily, n = 4)) / 5) %>%
  dplyr::mutate(d_daily_smooth = (d_daily +
                                    dplyr::lag(d_daily, n = 1) +
                                    dplyr::lag(d_daily, n = 2) +
                                    dplyr::lag(d_daily, n = 3) +
                                    dplyr::lag(d_daily, n = 4)) / 5) %>%
  dplyr::mutate(r_daily_smooth = (r_daily +
                                    dplyr::lag(r_daily, n = 1) +
                                    dplyr::lag(r_daily, n = 2) +
                                    dplyr::lag(r_daily, n = 3) +
                                    dplyr::lag(r_daily, n = 4)) / 5)
# 0. Health region data - all of this is for the heatmaps;
# hr_c <- read.csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# hr_d <- read.csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_hr/mortality_timeseries_hr.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# hr_c <- read.csv("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/cases_timeseries_hr.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
# hr_d <- read.csv("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/mortality_timeseries_hr.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
hr_c <- read.csv("docs/data/hr_c_clean.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
hr_d <- read.csv("docs/data/hr_d_clean.csv", header = TRUE, sep = ",", encoding = 'UTF-8')
hr_c$date_report<-as.Date(hr_c$date_report,format="%d-%m-%y")
hr_d$date_death_report<-as.Date(hr_d$date_death_report,format="%d-%m-%y")
# 1. Cases
on_case<- filter(hr_c, province == "Ontario" ) 
on_case_region<- filter(hr_c, province == "Ontario" )  %>% group_by(health_region) %>% summarise(Freq=sum(cases,na.rm = T))
region_order <- unlist(on_case_region[order(on_case_region$Freq),][,1]) #save new region order by total case
on_case$health_region<-factor(on_case$health_region, levels = region_order[1:nrow(on_case_region)])
# 2. deaths
on_death<- filter(hr_d, province == "Ontario" ) 
on_death_region<- filter(hr_d, province == "Ontario" )  %>% group_by(health_region) %>% summarise(Freq=sum(deaths,na.rm = T))
region_order <- unlist(on_death_region[order(on_death_region$Freq),][,1]) #save new region order by total case
on_death$health_region<-factor(on_death$health_region, levels = region_order[1:nrow(on_case_region)])
#------------------ creating Toronto data;  ------------------
#aggregate counts by date;
trt_c_daily <- filter(hr_c, health_region=="Toronto")  %>% group_by(date_report) %>% select(-cumulative_cases, -province)
trt_d_daily <- filter(hr_d, health_region=="Toronto")  %>% group_by(date_death_report) %>% select(-cumulative_deaths, -province)
#this data can be shared on the dashboard; #we can also share the canada data by province;
trt<-merge(trt_c_daily, trt_d_daily, by.x = c("date_report", "health_region"),by.y=c("date_death_report","health_region"),all.x = T)
trt[is.na(trt)]<-0  #give value zero for missing; only do this on daily value!
names(trt)[3:4]<-c("c_daily","d_daily")
trt <- trt %>% mutate(c_cum = cumsum(c_daily)) #cumulative level;
trt <- trt %>% mutate(d_cum = cumsum(d_daily)) #cumulative level;
trt$a_cum <- trt$c_cum - trt$d_cum
df_trt<-data.frame(trt$c_cum[trt$c_cum>50], c(1:length(trt$c_cum[trt$c_cum>50])))
names(df_trt)<-c("trt","index")
df_trajectory_can <- df_trajectory_can %>% dplyr::left_join(df_trt, by = "index")

#------------------ creating quebec data;  ------------------
qc<-can_p[can_p$province=="Quebec",]
qc$a_cum<-qc$c_cum - qc$r_cum - qc$d_cum
#getting daily recovered;
qc$r_lag<-lag(qc$r_cum)
qc$r_lag[is.na(qc$r_lag)]<-0
qc$r_daily<-qc$r_cum-qc$r_lag
qc$c_lag<-lag(qc$c_cum)
qc$d_lag<-lag(qc$d_cum)
qc$c_lag[is.na(qc$c_lag)]<-0
qc$d_lag[is.na(qc$d_lag)]<-0
qc$c_percentchange<-round(qc$c_daily/qc$c_lag*100,1)
qc$d_percentchange<-round(qc$d_daily/qc$d_lag*100,1)
qc$r_percentchange<-round(qc$r_daily/qc$r_lag*100,1)
qc <- qc %>% 
  dplyr::mutate(c_daily_smooth = (c_daily +
                                    dplyr::lag(c_daily, n = 1) +
                                    dplyr::lag(c_daily, n = 2) +
                                    dplyr::lag(c_daily, n = 3) +
                                    dplyr::lag(c_daily, n = 4)) / 5) %>%
  dplyr::mutate(d_daily_smooth = (d_daily +
                                    dplyr::lag(d_daily, n = 1) +
                                    dplyr::lag(d_daily, n = 2) +
                                    dplyr::lag(d_daily, n = 3) +
                                    dplyr::lag(d_daily, n = 4)) / 5) %>%
  dplyr::mutate(r_daily_smooth = (r_daily +
                                    dplyr::lag(r_daily, n = 1) +
                                    dplyr::lag(r_daily, n = 2) +
                                    dplyr::lag(r_daily, n = 3) +
                                    dplyr::lag(r_daily, n = 4)) / 5)
# 1. Cases
qc_case<- filter(hr_c, province == "Quebec" ) 
qc_case_region<- filter(hr_c, province == "Quebec" )  %>% group_by(health_region) %>% summarise(Freq=sum(cases,na.rm = T))
region_order <- unlist(qc_case_region[order(qc_case_region$Freq),][,1]) #save new region order by total case
qc_case$health_region<-factor(qc_case$health_region, levels = region_order[1:nrow(qc_case_region)])
# 2. deaths
qc_death<- filter(hr_d, province == "Quebec" ) 
qc_death_region<- filter(hr_d, province == "Quebec" )  %>% group_by(health_region) %>% summarise(Freq=sum(deaths,na.rm = T))
region_order <- unlist(qc_death_region[order(qc_death_region$Freq),][,1]) #save new region order by total case
qc_death$health_region<-factor(qc_death$health_region, levels = region_order[1:nrow(qc_case_region)])
#------------------ creating New bubble map data;  ------------------
# instead of using polygon maps, we will display bubble map it reads faster;
# only information needed are the bubble centre lat and lng for each province and territory;
library(leaflet) #mapping;
canada_region<- can_p[can_p$date_report==max(can_p$date_report)&can_p$province != "Repatriated",]
canada_region$a_cum<-canada_region$c_cum - canada_region$d_cum - canada_region$r_cum
canada_region$province<-factor(canada_region$province)
canada_region$lat<-c(53.9333, 49.2827, 53.7609, 46.5653, 53.1355, 44.6820, 70.2998, 64.8255, 51.2538, 46.5107, 52.9399, 52.9399, 64.2823)
canada_region$lng<-c(-116.5765, -123.1207,  -98.8139,  -66.4619,  -57.6604, -63.7443, -83.1076 ,-124.8457, -85.3232  ,-63.4168,  -73.5491, -106.4509, -135.0000)
# Prepare the text for the tooltip:
mytext <- paste(
  canada_region$province, "<br/>",
  "Confirmed Cases: ", canada_region$c_cum, "<br/>", 
  "Deaths: ", canada_region$d_cum, "<br/>", 
  "Recovered: ", canada_region$r_cum, sep="") %>%
  lapply(htmltools::HTML)
#------------------ data update time;  ------------------
# date_update_date<-read.table("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/update_time.txt")
# date_update_date<-read.table("https://raw.githubusercontent.com/Kuan-Liu/coronavirus-csv/master/update_time.txt")
date_update_date<-read.table("docs/data/update_time.txt")
data_date<-as_tibble(paste(date_update_date$V1, date_update_date$V2, date_update_date$V3))
