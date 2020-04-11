#data creation;

library(dplyr) #data tool;
library(reshape2) #data tool;
library(chron) #data tool;
library(RCurl) #read data url;


x1 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv")
x2 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/mortality.csv")
# x3 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/recovered_cumulative.csv")
# x4 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/testing_cumulative.csv")

can_c <- read.csv(text=x1, header = TRUE, sep = ",", encoding = 'UTF-8')
can_d <- read.csv(text=x2, header = TRUE, sep = ",", encoding = 'UTF-8')
# can_r <- read.csv(text=x3, header = TRUE, sep = ",", encoding = 'UTF-8')
# can_t <- read.csv(text=x4, header = TRUE, sep = ",", encoding = 'UTF-8')

#------------------ canada data formating ------------------
#format dates;
can_c$date_report<-as.Date(can_c$date_report,format="%d-%m-%y")
can_d$date_death_report<-as.Date(can_d$date_death_report,format="%d-%m-%y")
# can_r$date_recovered<-as.Date(can_r$date_recovered,format="%d-%m-%y")
# can_t$date_testing<-as.Date(can_t$date_testing,format="%d-%m-%y")

#format province labels;
province_labelc<-c("Alberta","British Columbia","Manitoba","New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                   "NorthWest", "Ontario","Prince Edward Island","Quebec",
                   "Repatriated","Saskatchewan","Yukon")
province_labeld<-c("Alberta","British Columbia","Manitoba","Newfoundland and Labrador", "Nova Scotia", "Ontario","Quebec","Saskatchewan")
# province_labelr<-c("Alberta","British Columbia","Manitoba","New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                   "Nunavut","NorthWest", "Ontario","Prince Edward Island","Quebec","Saskatchewan","Yukon")

# province_labela1<-c("AB","BC","MB","NB","NL", "NS","NW", "ON","PE","QC","SK","YT")
# province_labela2<-c("AB","BC","MB","NB","NL", "NS","NW","NU", "ON","PE","QC","SK","YT")

levels(can_c$province)<-province_labelc
levels(can_d$province)<-province_labeld
# levels(can_r$province)<-province_labelr
# levels(can_t$province)<-province_labelr


#------------------ canada data formating ------------------

#aggregate counts by date;
can_p_dpaily <- can_c  %>% group_by(date_report, province) %>% summarise(c_dpaily=n()) #individual level;
can_p_dpaily <- can_p_dpaily  %>% group_by(province)%>% mutate(c_pum = cumsum(c_dpaily)) #cumulative level;
can_dp_dpaily <- can_d  %>% group_by(date_dpeath_report,, province) %>% summarise(d_dpaily=n()) #individual level;
can_dp_dpaily <- can_dp_dpaily  %>% group_by(province) %>% mutate(d_pum = cumsum(d_dpaily)) #cumulative level;
# can_r_daily <- can_r  %>% group_by(date_recovered) %>% summarise(r_cum=sum(cumulative_recovered, na.rm = T)) #cumulative level;
# can_t_daily <- can_t  %>% group_by(can_t$date_testing) %>% summarise(t_cum=sum(cumulative_testing, na.rm=T)) #cumulative level;

#merge all data by dates in a canada dataset;
#this data can be shared on the dashboard;
can<-merge(can_c_daily, can_d_daily, by.x = "date_report",by.y="date_death_report",all = T)
can<-merge(can, can_r_daily, by.x = "date_report",by.y="date_recovered",all.x = T)
can<-merge(can, can_t_daily, by.x = "date_report", by.y="can_t$date_testing",all.x = T)
can[is.na(can)]<-0  #give value zero for missing;
can$a_cum<-can$c_cum - can$r_cum - can$d_cum

#first date when reached 100 cases; use this to plot for trajectories;
# can_date_100<-min(can$date_report[can$c_cum>=100],na.rm = T)


