 #only used citi pass holders(annual) because 24 hour and 3 day passes don't have customer informaion
#15% of citibike users are annual holders

#loop to read in csv files
#remove all row that have "NA"
#remove gender "0"
#remove birth year "\N"
#create month column
#remove start and stop columns(since we have trip duration in seconds)
#remove bike id?

#focus on only advertising idea??
#visitors have no gender and birthyear info
# ratiomembers vs visitors and how that changes, and month that peaks? for promotion.  (all data sets, only one colomn, by month keep # of members # of visitiors, need to keep areas of growth, where membrship is increasing)
#trip duration(in seconds)? avg mph? avg length of trip? avg length of trip by starting station? basket into boroughs by long latitude to see most common trip i.e. manhattan to manhattan.
#monthly number of usage over years- track growth, track busy season for advertising and promotions,  
#growth of number of stations over years
#productivity per station. (users per station)
#usage by hour of day! most traffic time for advertising.
#most productive stations by location/boroughs! by tripstart vs trip end
#age group, and gender
#members vs visitors age groups and gender
# most growth of members by borough! start and end separate? fastest growth rate?
#ambitious but by day of week?!!
# riders by borough, age group and gender splits for each borough, separate start end. 

#graphideas
#maps of best stations, by year?
#maps or graph by hour of day
#age group graph
#gender graph
#visitor to member ratio over years? and subscription growth over time.

#Trip Duration (seconds)
#Start Time and Date
#Stop Time and Date 
#Start Station Name
#End Station Name
#Station ID
#Station Lat/Long
#Bike ID
#User Type (Customer = 24-hour pass or 3-day pass user; Subscriber = Annual Member)
#Gender (Zero=unknown; 1=male; 2=female)
#Year of Birth

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(tidyr)

#looping through csv files by month and putting into one huge df.  Really large df for each month so instead of looping and editing, may want to edit first
citibike_csv_files= paste0(rep(c("2016","2017"),c(6,6)), c("08","09","10","11","12","01","02","03","04","05","06","07"),".csv")
citibike_raw= tbl_df(c())
for(i in citibike_csv_files) {
  readcsv=read.csv(i, stringsAsFactors = FALSE, header=T)
  citibike_raw=rbind(citibike_raw, readcsv)
}
citibike_raw

#takes too long, doing by month and filtering customers might be faster in order to tie rbind each month at the end
#keeping july16rawdf as control data frame.  so we don't have to load again if I filter wrong.
aug16raw = read.csv("201608.csv", stringsAsFactors = F, header =T)
aug16df2 = aug16raw %>% filter(usertype == "Subscriber")

sep16raw = read.csv("201609.csv", stringsAsFactors = F, header =T)
sep16df2 = sep16raw %>% filter(usertype == "Subscriber")


oct16raw = read.csv("201610.csv", stringsAsFactors = F, header =T)
oct16df2 = oct16raw %>% filter(User.Type == "Subscriber")

nov16raw = read.csv("201611.csv", stringsAsFactors = F, header =T)
nov16df2 = nov16raw %>% filter(User.Type == "Subscriber")

dec16raw = read.csv("201612.csv", stringsAsFactors = F, header =T)
dec16df2 = dec16raw %>% filter(User.Type == "Subscriber")

jan17raw = read.csv("201701.csv", stringsAsFactors = F, header =T)
jan17df2 = jan17raw %>% filter(User.Type == "Subscriber")

feb17raw = read.csv("201702.csv", stringsAsFactors = F, header =T)
feb17df2 = feb17raw %>% filter(User.Type == "Subscriber")

mar17raw = read.csv("201703.csv", stringsAsFactors = F, header =T)
mar17df2 = mar17raw %>% filter(User.Type == "Subscriber")

apr17raw = read.csv("201704.csv", stringsAsFactors = F, header =T)
apr17df2 = apr17raw %>% filter(usertype == "Subscriber")

may17raw = read.csv("201705.csv", stringsAsFactors = F, header =T)
may17df2 = may17raw %>% filter(usertype == "Subscriber")

jun17raw = read.csv("201706.csv", stringsAsFactors = F, header =T)
jun17df2 = jun17raw %>% filter(usertype == "Subscriber")

jul17raw = read.csv("201707.csv", stringsAsFactors = F, header =T)
jul17df2 = jul17raw %>% filter(usertype == "Subscriber")

#synchronize column names!! all different!!
#rbind all month df's that are already filtered for annual members!!
names(mar17df2)= names(aug16df2)
citibikedf1=rbind(aug16df2, sep16df2, oct16df2, nov16df2, dec16df2, jan17df2, feb17df2, mar17df2, apr17df2, may17df2, jun17df2, jul17df2)
citibikedf1

#predominantly work with july due to 13.5 million observations for the year.
#check#tidyr split dates to day, and time.
#check# make age column
#check#make Male female column

#make column hour of the day
#make column day of the week
#remove columns I don't need.
write.table(jul17dfClean, file="jul17dfClean.csv")
sampledf=read.csv("jul17dfClean.csv")

jul17raw = read.csv("201707.csv", stringsAsFactors = F, header =T)
jul17df3 = jul17raw %>% filter(usertype == "Subscriber") %>%
  filter(birth.year !="NULL")

timevector=paste0(rep(c(12,1:11),2), rep(c("AM","PM"), c(12,12)))

#remove select function to include all original variables(columns)
jul17dfClean = jul17df3 %>%
  separate(starttime, into = c("date","time"), sep=" ") %>%
  mutate(age= 2017-as.numeric(birth.year),
  ageRange= ifelse(age<=12, "12 and younger",
                   ifelse(age>=13 & age<=18, "13 to 18",
                          ifelse(age>=19 & age<=24, "19 to 24",
                                 ifelse(age>=25 & age<=30, "25 to 30",
                                        ifelse(age>=31 & age<=36, "31 to 36",
                                               ifelse(age>=37 & age<=42, "37 to 42",
                                                      ifelse(age>=43 & age<=48, "43 to 48",
                                                             ifelse(age>=49 & age<=54, "49 to 54",
                                                                    ifelse(age>=55 & age<=60, "55 to 60",
                                                                           ifelse(age>=61 & age<=66, "61 to 66",
                                                                                  ifelse(age>=67, "67 and over", "NA"))))))))))),
  sex=ifelse(gender==1, "Male","Female"),
  hour= as.factor(unlist(substr(time,1,2))),
  day= weekdays(as.Date(date, format="%Y-%m-%d"))) %>%
  select(start.station.id,start.station.name,start.station.latitude, start.station.longitude, age, ageRange, sex, hour, day) %>%
  filter(ageRange != "NA")
levels(jul17dfClean$hour) = timevector
jul17dfClean$day = factor(jul17dfClean$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))


ageRangeVector = jul17dfClean$ageRange
julDFbyHour = jul17dfClean %>% group_by(hour)
julDFbyDay = jul17dfClean %>% group_by(day)

names(julDFbyDay)


g = ggplot(data=jul17dfClean, aes(x=sex)) +geom_bar(aes(fill="sex"))
g2 = ggplot(data=jul17dfClean, aes(x=age)) +geom_histogram(binwidth = 5, aes(fill = sex), position="dodge") + coord_cartesian(xlim = c(0,100))
g3 = ggplot(data=jul17dfClean, aes(x=ageRange))+geom_bar()
g4= ggplot(data= jul17dfClean, aes(x=hour)) +geom_bar()
g5= ggplot(data= jul17dfClean, aes(x=day)) +geom_bar()


