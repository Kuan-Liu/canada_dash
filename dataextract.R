#data creation;
library(dplyr) #data tool;
library(reshape2) #data tool;
library(chron) #data tool;
library(RCurl) #read data url;


x1 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv")
x2 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/mortality.csv")

can_c <- read.csv(text=x1, header = TRUE, sep = ",", encoding = 'UTF-8')
can_d <- read.csv(text=x2, header = TRUE, sep = ",", encoding = 'UTF-8')


#------------------ canada data formating ------------------
#format dates;
can_c$date_report<-as.Date(can_c$date_report,format="%d-%m-%y")
can_d$date_death_report<-as.Date(can_d$date_death_report,format="%d-%m-%y")

#format province labels;
province_labelc<-c("Alberta","British Columbia","Manitoba","New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                   "NorthWest", "Ontario","Prince Edward Island","Quebec",
                   "Repatriated","Saskatchewan","Yukon")
province_labeld<-c("Alberta","British Columbia","Manitoba","Newfoundland and Labrador", "Nova Scotia", "Ontario","Quebec","Saskatchewan")

levels(can_c$province)<-province_labelc
levels(can_d$province)<-province_labeld


#------------------ canada data creation ------------------

#aggregate counts by date;
can_p_daily <- can_c  %>% group_by(date_report, province) %>% summarise(c_daily=n()) #individual level;
can_dp_daily <- can_d  %>% group_by(date_death_report, province) %>% summarise(d_daily=n()) #individual level;

can_p<-merge(can_p_daily, can_dp_daily, 
             by.x = c("date_report","province"),
             by.y=c("date_death_report","province"), all = T)

can_p[is.na(can_p)]<-0  #give value zero for missing;

can_p <- can_p  %>% group_by(province) %>% mutate(c_pum = cumsum(c_daily)) #cumulative level;
can_p <- can_p  %>% group_by(province) %>% mutate(d_pum = cumsum(d_daily)) #cumulative level;


write.csv(can_p,"can.csv",row.names = F)




