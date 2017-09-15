library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(reshape2)
library(lubridate)

setwd("E:/courses/Stat215A/lab1/lab1_template")
source("R/load.R")
source("R/clean.R")

#Readin Dates, Locations and Sonoma
epoch_df <- loadDatesData(path = 'data/')
sonoma <- loadRedwoodData(path = 'data/','all')
sonoma$epoch = as.factor(sonoma$epoch)

location <- loadMoteLocationData(path = 'data/')
date <- cleanDatesData(epoch_df) 
rm(epoch_df)

#Rename column names
colnames(date)[1] <- 'epoch'
colnames(location)[1] <- 'nodeid'

#join table

sonoma_info = sonoma %>% left_join(date, by = "epoch") %>%
  left_join(location, by = 'nodeid')

###
# The following part is used to detect abnormal records
# I will also clean the dataset along the process
###

#First, I want to ask the question like this: does one node
# has only one record at a certain epoch?(has duplicated data?)

node_epoch = sonoma_info %>% group_by(nodeid, epoch) %>% mutate(count=n())

#I see that most nodes have duplicated data in one epoch, 
#Do the duplicated records share the same values on humity,temp,PAR...

dplct_node_epoch = node_epoch %>% group_by(nodeid,epoch) %>%
summarise(humid_range = max(humidity) -min(humidity),
          temp_range = max(humid_temp) -min(humid_temp),
          hamatop_range = max(hamatop)-min(hamatop),
          hamabot_range = max(hamabot)-min(hamabot))

range_node_epoch = filter(dplct_node_epoch,humid_range > 1 | temp_range > 1 |
         hamatop_range > 100 | hamabot_range > 100)

#Here, I found that most of them have similar values, 
#expect: node 4 at epoch 7074; node 11 at epoch 18;
#        node 17 at epoch 36; node 22 at epoch 3860;
#        ......

#Total 26 of them, remove them from sonoma data and take one value from the dulplicates
sonoma_info <- sonoma_info %>% anti_join(range_node_epoch,by = c("nodeid","epoch")) %>%
      group_by(nodeid,epoch) %>% filter(row_number(humidity) == 1) %>% ungroup()   

#remove RAM
rm(dplct_node_epoch)
rm(node_epoch)
rm(range_node_epoch)

#Second let's check humidity:
negative_humid <- filter(sonoma_info, humidity <0)
group_by(negative_humid, nodeid)%>%summarise(n=n())

#Here I see that node 29, 123, 141 have some many negative values for humidity.
#check node 29
node29 = sonoma_info %>% filter(nodeid == 29)
summary(node29)
#constant on humidity and tempture, so remove node 29 from sonoma data.
sonoma_info <- filter(sonoma_info,nodeid != 29)

#check node 123
node123 = sonoma_info %>% filter(nodeid == 123)
summary(node123)
#ggplot(data = node123[1:500,]) + geom_point(aes(x = epoch, y = humidity))
#After checking the those negative values on humidity, I found that other values
# such as temp and humid_adj are also probablematic, so I drop them. (It is the same for other nodes having negative humidity)
sonoma_info <- filter(sonoma_info, nodeid !=  123 |humidity > 0) %>%
  filter(nodeid !=  141 |humidity > 0) %>% filter(nodeid !=  78 |humidity > 0) %>% 
  filter(nodeid !=  198 |humidity > 0) %>% filter(nodeid !=  65535 |humidity > 0)

#Clean RAM
rm(node123)
rm(node29)


#Third, let us check voltage data
ggplot(data = sonoma_info, aes(voltage)) + geom_histogram(binwidth = 5)+
    ggtitle("Histogram of Voltage") + ylab("Frequency")+
    theme_classic()


voltage_info <- sonoma_info %>% mutate(voltage_range = ifelse(voltage > 3,
                        'too high',ifelse(voltage < 2.4,'too low','normal')))
voltage_info %>% group_by(voltage_range) %>% summarise(count = n())

# A tibble: 3 x 2
#voltage_range  count
#<chr>  <int>
#1        normal 190k
#2      too high 96k
#3       too low  22k

#However, we see the head of table shows that lots of the voltages are of hundred-level: around 220
sum(sonoma_info$voltage > 100) # = 96k
#Are they really problematic???
high_voltage_node = sonoma_info %>% filter(voltage > 200) %>% 
  distinct(nodeid) %>% arrange(nodeid) %>% t() %>% as.vector()
length(high_voltage_node) #30+

normal_voltage_node = sonoma_info %>% filter(voltage < 5) %>% 
  distinct(nodeid) %>% arrange(nodeid) %>% t() %>% as.vector()
length(normal_voltage_node) #70+

#total number of nodes
length(sonoma_info %>% distinct(nodeid) %>% t()) #70+

#Let divided the data into three groups:low-voltage,normal-voltage,high-voltage 
low_voltage = filter(sonoma_info,voltage < 2.4)
high_voltage = filter(sonoma_info,voltage > 3)
normal_voltage = filter(sonoma_info,voltage <= 3 & voltage >= 2.4)

#And get some plots to see whether they have much difference
ggplot(low_voltage)+geom_point(aes(x = epoch, y = humidity))
ggplot(high_voltage)+geom_point(aes(x = epoch, y = humidity))
ggplot(normal_voltage)+geom_point(aes(x = epoch, y = humidity))


##At this moment, I don't see the high_voltage plot has the same pattern as the 
#normal one. So, I just follow what the article says and only pick the normal one.

write.csv(normal_voltage,"cleaned_data.csv")
rm(high_voltage)
rm(low_voltage)

##########
##The Following part is used for data analysis
##

##Finding 1: The differences of humidity, temp, hamatop and hamabot 
##          according to spacial distributions
mydata = read.csv("cleaned_data.csv")
mydata$date = date(mydata$datetime)
sonoma_info$date = date(sonoma_info$datetime)

#EDA
ggplot(data)+geom_histogram(aes(x=date), binwidth = 1) +
  ggtitle('Number of Records on Each Day') + theme_classic()

histo_direc<- ggplot(mydata %>% filter(! is.na(Direc)),aes(x=date,fill=Direc))
histo_direc + geom_histogram(binwidth = 1) + theme(legend.key.size = unit(0.3, "cm"))+
  ggtitle('Number of Records on Each Day') + theme_classic()

histo_tree<- ggplot(mydata %>% filter(! is.na(Direc)),aes(x=date,fill=Tree))
histo_tree + geom_histogram(binwidth = 1) + theme(legend.key.size = unit(0.4, "cm"))+
  ggtitle('Number of Records on Each Day') + theme_classic()




#one sample of voltage
voltage107 = mydata %>% filter(date == '2004-5-1') %>% select(voltage,time,nodeid)
ggplot(data = voltage107 %>% filter(nodeid == '107' | nodeid == 3),aes(x = time, y = voltage,
                             group = nodeid)) + geom_line(aes(linetype = as.factor(nodeid)))+
  ggtitle("Change of Voltage for Node 3 and Node 107") + xlab('Time: May 1 2004')+
  theme_classic()
#
#

space_time_data = mydata %>% group_by(Tree,Direc,datetime) %>%
  summarise(humidity = mean(humidity),temp = mean(humid_temp),
            hamatop = mean(hamatop),hamabot = mean(hamabot),count = n())

space_time_data<- space_time_data %>% filter(! is.na(Tree))

#space_date_data = space_time_data %>%
#  group_by(Tree,Direc,date)   %>%
#  summarise(humidity = mean(humidity),temp = mean(temp),
#  hamatop = mean(hamatop),hamabot = mean(hamabot),count = n())

temp_space <- ggplot(space_time_data %>% mutate(date = date(datetime)) %>% filter(date == '2004-5-1' & ! is.na(Direc)),
                     aes(x = datetime,
          y =temp, col = Tree,group = Tree))  + geom_line(aes(colour=Tree)) +
        #geom_point(aes(color = Tree))+
facet_wrap(~ Direc, ncol=3) +
  ggtitle('Locations and Temperature') +
  ylab('Temp(degree Celsius)') + xlab('Time:2004-5-1') + theme_classic()

humid_space <- ggplot(space_time_data %>% mutate(date = date(datetime)) %>% filter(date == '2004-5-1' & ! is.na(Direc)),
                     aes(x = datetime,
                         y =humidity, col = Tree,group = Direc))  + geom_line(aes(colour=Direc)) +
  #geom_point(aes(color = Tree))+
  facet_wrap(~ Tree, ncol=2) +
  ggtitle('Locations and Humidity') +
  ylab('Humidity') + xlab('Time:2004-5-1') + theme_classic()




tempdirec = location %>% group_by(Direc) %>% summarise(n())
write.csv(tempdirec,'temp.csv')


temp_humid_data = space_time_data %>%
  mutate(hour = hour(datetime)) %>% filter(hour >= 0 & hour <= 6) %>%
  group_by(datetime) %>% summarise(temp = mean(temp),humidity = mean(humidity))

temp_humid_data = temp_humid_data %>% mutate(month = month(datetime), date = date(datetime))

temp_humidity_night = ggplot(data = temp_humid_data %>% filter(date >= '2004-05-01' & date <= '2004-5-09'),
   aes(x = humidity, y = temp)) + geom_point() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm')+
  facet_wrap(~ date, ncol=3) +
  ggtitle("Relationship Between Humidity and Temperature at night")
  

#Moon
hour_light  = space_time_data %>% mutate(date = date(datetime),
                                              hour = hour(datetime)) %>%
  filter(hour >= 18 | hour <= 6)

hour_light = hour_light %>% group_by(date,hour) %>% 
  summarise(IPAR = mean(hamatop), RPAR = mean(hamabot))



moon = read.csv("data/moon.csv")
hour_light = 