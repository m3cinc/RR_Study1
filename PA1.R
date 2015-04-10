# PA1 Script for reproductible Research 1
#
# use the ProjectTemplate add-on
#
# Start with configuration documentation
#
Sys.info()
#
setwd("P:/Reproducible Study 1")        # start with a fresh working directory first   
library('ProjectTemplate')              # a new project
projectname<-'PA1'
currentdir<-getwd()
if (!file.exists("PA1")){create.project(projectname)}   # only create once
setwd(paste(currentdir,projectname,sep="/")) # point to project directory
datadir<-"./data"                       # sub-directory where the data will reside
graphsdir<-"./graphs"                   # sub-directory where the graphs will reside
load.project()
#
# helper function to convert interval in minutes
#
to.minutes <- function(x){
        s<-60*as.numeric(as.character(x%/%100))+as.numeric(as.character(x%%100))
        x<-s
        x
}
#
to.miltime <- function(x){
        s<-str_pad(as.character(x),4,pad="0")
        x<-s
        x
}
#
to.interval <- function(x){
        s<-(x%%60)+100*(x%/%60)
        x<-s
        x
}
#
to.seconds <- function(x){
        s<-(timeDate(to.minutes(x),format="%H%M")-timeDate(0))*60
        x<-s
        x
}
#
to.POSIXlt <- function(x){
        x$interval %>%
                sapply(to.miltime) %>%
                paste(x$date,.,sep=" ") %>%
                strptime("%Y-%m-%d %H%M") -> x$time
        x        
}
#
sessionInfo()   # to document platform
#
# update global.dcf file to load appropriate libraries
#
# read the datafile supplied and place its content in the data subdirectory
#
SSLurl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!file.exists("data")) {
        dir.create("data")
}
if (!file.exists("graphs")){
        dir.create("graphs")
}
filename<-paste(datadir,strsplit(SSLurl,"%2F")[[1]][length(strsplit(SSLurl,"%2F")[[1]])],sep="/")
download.file(SSLurl, dest=filename, mode="wb") 
unzip (filename, exdir = datadir)       # unzip creates and populates the data structure 
unlink(filename)                        # cleanup after and remove the zip file
## retrieve activity filename 
filename<-(list.files(path=datadir,full.names=T,recursive=T))
filename
activity<-data.frame()
activity<-read.csv(filename,header=TRUE,stringsAsFactors=FALSE)
# this concludes the data retrieval step
activity$date<-as.Date(activity$date, "%Y-%m-%d") # apply date format
activity$minutes<-sapply(activity$interval,to.minutes)    # interval to minutes
summary(activity)    # observe a lot of missing values

#> steps             date               interval         minutes      
#> Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :   0.0  
#> 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.: 358.8  
#> Median :  0.00   Median :2012-10-31   Median :1177.5   Median : 717.5  
#> Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Mean   : 717.5  
#> 3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.:1076.2  
#> Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :1435.0  
#> NA's   :2304  

#
# Q1: What is mean total number of steps taken per day ?
#
df<-activity[complete.cases(activity),]

data.frame(df %>%                                                                              
        group_by(date) %>%
        summarize(total = sum(steps),
                  average = mean(steps)) -> daily) %$%
        summary(daily$total, digits=7)

#> Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 41.00  8841.00 10765.00 10766.19 13294.00 21194.00 

myPNGfile<-paste(graphsdir,"plot1.png",sep='/')  # point to graphs subdirectory
png(filename=myPNGfile,width=480,height=480)        # open png device for plot1.png 480x480 pix
ggplot (daily,aes(x=total/1e3))+geom_histogram()+
        labs(title="Histogram of Total Daily Steps\n(in Thousands)")+
        xlab("Total Daily Steps (in Thousands)")+
        ylab("Count (in Days)")

dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#>                   size isdir mode               mtime               ctime               atime exe 
#>./graphs/plot1.png 5083 FALSE  666 2015-04-08 23:17:16 2015-04-07 17:40:11 2015-04-07 17:40:11  no
#
# Q2. What is the average daily activity pattern ?
#
data.frame(df %>%
           group_by(interval) %>%
           summarize(total = sum(steps),
           average = mean(steps)) -> hourly) %$%
           summary(hourly$average, digits=7)

#> Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 0.00000   2.48585  34.11321  37.38260  52.83491 206.16980 

df$interval %>%
        sapply(to.miltime) %>%
        paste(df$date[1],.,sep=" ") %>%
        strptime("%Y-%m-%d %H%M") %>%
        as.POSIXlt("%H:%M")-> df$time
hourly$interval %>%
        sapply(to.miltime) %>%
        paste(df$date[1],.,sep=" ") %>%
        strptime("%Y-%m-%d %H%M") %>%
        as.POSIXlt("%H:%M")-> hourly$time

peek<-hourly[hourly$total==max(hourly$total),]
msg<-paste("Maximum average: ~",round(peek$average),"Steps recorded at interval ",peek$interval,sep=" ")
print(msg)

#> [1] "Maximum average: ~ 206 Steps recorded at interval  835"

myPNGfile<-paste(graphsdir,"plot2.png",sep='/')  # point to graphs subdirectory
png(filename=myPNGfile,width=480,height=480)        # open png device for plot1.png 480x480 pix
#
ggplot(hourly,aes(x=time,y=average))+
        geom_line()+
        scale_x_datetime(breaks=date_breaks("4 hour"),labels=date_format("%Hh:%Mm"))+
        geom_point()+
        geom_point(data=peek,aes(x=time,y=average),color="red",size=3,show_guide=FALSE)+
        labs(title=paste("Average Steps (in 5-minutes intervals)\n",msg,sep=""))+
        xlab("")+
        ylab("Average Steps")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#>                   size isdir mode               mtime               ctime               atime exe 
#>./graphs/plot2.png 7877 FALSE  666 2015-04-08 23:17:35 2015-04-07 18:31:10 2015-04-07 18:31:10  no
#
# Q3: Inputing missing values
#
# 1. Number of NA rows in the dataset
#
missing<-sum(is.na(activity$steps))
#
# 2. Replace NAs with 5-min interval mean in activity
#
ma<-subset(activity,is.na(activity$steps))   # retain only the missing activity steps data subset
nma<-subset(activity,!is.na(activity$steps))
#
# 3. create and populate new dataset ma
#
ma<-merge(ma,hourly,all=T)
ma$steps<-ma$average
ma<-ma[,-(5:7)]
ma<-rbind(ma,nma)
ma<-arrange(ma,date,interval)
#
# 4. Make histogram
#
data.frame(ma %>%                                                                              
                   group_by(date) %>%
                   summarize(total = sum(steps),
                             average = mean(steps)) -> madaily) %$%
        summary(daily$total, digits=7)

#> Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 41.00  8841.00 10765.00 10766.19 13294.00 21194.00 

myPNGfile<-paste(graphsdir,"plot3.png",sep='/')  # point to graphs subdirectory
png(filename=myPNGfile,width=480,height=480)        # open png device for plot1.png 480x480 pix
ggplot (madaily,aes(x=total/1e3))+geom_histogram()+
        labs(title="Histogram of Total Daily Steps\n(in Thousands)")+
        xlab("Total Daily Steps (in Thousands)")+
        ylab("Count (in Days)")

dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#>                   size isdir mode               mtime               ctime               atime exe 
#>./graphs/plot3.png 5340 FALSE  666 2015-04-08 23:17:35 2015-04-08 20:14:37 2015-04-08 20:14:37  no
#
data.frame(ma %>%
                   group_by(interval) %>%
                   summarize(total = sum(steps),
                             average = mean(steps)) -> mahourly) %$%
        summary(mahourly$average, digits=7)

#> Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 0.00000   2.48585  34.11321  37.38260  52.83491 206.16980
ma$interval %>%
        sapply(to.miltime) %>%
        paste(ma$date[1],.,sep=" ") %>%
        strptime("%Y-%m-%d %H%M") %>% 
        as.POSIXlt("%H:%M")-> ma$time
mahourly$interval %>%
        sapply(to.miltime) %>%
        paste(ma$date[1],.,sep=" ") %>%
        strptime("%Y-%m-%d %H%M")%>%
        as.POSIXlt("%H:%M")-> mahourly$time

mapeek<-mahourly[mahourly$total==max(mahourly$total),]
msg<-paste("Maximum average: ~",round(mapeek$average),"Steps recorded at interval ",mapeek$interval,sep=" ")
print(msg)
#> [1] "Maximum average: ~ 206 Steps recorded at interval  835"

peek<-rbind(peek,mapeek)
identical(peek$average[1],peek$average[2])
#> [1] TRUE             
#
# Note the 2 datasets produce same mean and peek values: no change if substituting NAs with means
#
# Q4. Are there differences in activity patterns between weekdays and weekends ?
#
# 1. Create the daytype factor derived from weekdays(ma$date)
#
daytype.f<-factor("weekday","weekend")
ma$daytype[which(weekdays(ma$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))]<-"weekday"
ma$daytype[which(weekdays(ma$date) %in% c("Saturday","Sunday"))]<-"weekend"
#
# 2. Make a panel plot of time series
#
#
data.frame(ma %>%
                   select(-time) %>%
                   group_by(daytype,interval) %>%
                   summarize(total = sum(steps),
                   average = mean(steps)) -> mahourly) 

#> Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 0.00000   2.04706  28.13325  38.98849  61.26327 230.37820 

ma$interval %>%
        sapply(to.miltime) %>%
        paste(ma$date[1],.,sep=" ") %>%
        strptime("%Y-%m-%d %H%M") %>%
        as.POSIXlt("%H:%M")-> ma$time
mahourly$interval %>%
        sapply(to.miltime) %>%
        paste(ma$date[1],.,sep=" ") %>%
        strptime("%Y-%m-%d %H%M") %>%
        as.POSIXlt("%H:%M")-> mahourly$time
mahourly<-as.data.frame(mahourly)
#
myPNGfile<-paste(graphsdir,"plot4.png",sep='/')  # point to graphs subdirectory
png(filename=myPNGfile,width=480,height=480)        # open png device for plot1.png 480x480 pix
ggplot(mahourly,aes(x=time,y=average))+
        geom_line()+
        scale_x_datetime(breaks=date_breaks("4 hour"),labels=date_format("%Hh:%Mm"))+
        labs(title=paste("Average Steps (in 5-minutes intervals)\n",msg,sep=""))+
        xlab("")+
        ylab("Average Steps")+
        facet_wrap(~daytype,ncol=1)

dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#>                   size isdir mode               mtime               ctime               atime exe 
#>./graphs/plot4.png 8166 FALSE  666 2015-04-08 23:18:20 2015-04-08 21:27:33 2015-04-08 21:27:33  no
