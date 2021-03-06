#This file is the same as the BUEnergyFunctions.R file, but the implementations are more efficient


#given month, day, year, returns the correct format for sql queries like year-month-day
makeDate <- function(month, day, year){
  strDates <- paste(month, day, year)
  date <- as.Date(strDates, "%m %d %Y")
  return (date)
}

#returns a single temperature for a single date
singleTemp <- function(date) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("select avg(temp) from BUEnergy.weather where time BETWEEN '", date," 00:00:00' and '", date," 23:59:59';", sep="")
  daytemp <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (daytemp[[1]])
}

#returns a list of daily temperatures over a given date range 
listTemp <- function(startdate, enddate) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("select avg(temp) from BUEnergy.weather where time BETWEEN '", startdate," 00:00:00' and '", enddate," 23:59:59' group by date(time);", sep="")
  templist <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (templist[[1]])
}

#returns a single HDD with a given base temperature 
singleHDD <- function(date, basetemp) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("select If(Avg(temp)>",basetemp,",0,",basetemp,"-avg(temp)) from BUEnergy.weather where time BETWEEN '", date," 00:00:00' and '", date," 23:59:59' group by date(time);", sep="")
  HDD <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (HDD[[1]])
}

#returns a list of HDD over a range of dates with a given base temperature  
listHDD <- function(startdate, enddate, basetemp) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("select If(Avg(temp)>",basetemp,",0,",basetemp,"-avg(temp)) from BUEnergy.weather where time BETWEEN '", startdate," 00:00:00' and '", enddate," 23:59:59' group by date(time);", sep="")
  HDD <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (HDD[[1]])
}

#returns a single CDD with a given base temperature 
singleCDD <- function(date, basetemp) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("select If(Avg(temp)<",basetemp,",0,avg(temp)-",basetemp,") from BUEnergy.weather where time BETWEEN '", date," 00:00:00' and '", date," 23:59:59' group by date(time);", sep="")
  HDD <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (HDD[[1]])
}

#returns a list of CDD over a range of dates with a given base temperature  
listCDD <- function(startdate, enddate, basetemp) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("select If(Avg(temp)<",basetemp,",0,avg(temp)-",basetemp,") from BUEnergy.weather where time BETWEEN '", startdate," 00:00:00' and '", enddate," 23:59:59' group by date(time);", sep="")
  HDD <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (HDD[[1]])
}

#the average temperature over a range
averageTemp <- function(startdate, enddate) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("SELECT avg(TEMP) from (SELECT avg(temp) as TEMP FROM BUEnergy.weather
                 WHERE time  BETWEEN '",startdate," 00:00:00' and '",enddate," 23:59:59'
                 group by date(time)) as TEMPTABLE;",sep="")
  
  HDD <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (HDD[[1]])
}

#HDD over a base temperature and a range of dates 
averageHDD <- function(startdate, enddate, basetemp) {
  HDD <- sum(listHDD(startdate, enddate, basetemp))
  return (HDD)
}

averageCDD <- function(startdate, enddate, basetemp) {
  CDD <- sum(listCDD(startdate, enddate, basetemp))
  return (CDD)
}

#returns an amount of elecricity in KwH for a single day
singleElectricity <- function(date, building_ID) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  #makes sure that building_ID is valid 
  if (building_ID > 15 | building_ID < 1) {
    print("Invalid building_ID.")
    return (0)
  }
  
  #make query, the sum of all the points in 1 day is the energy usage of that building in KwH 
  query <- paste("select sum(Amount) from BUEnergy.electricity where building_ID = ", building_ID," and time BETWEEN '", date," 00:00:00' and '", date," 23:59:59';", sep="")
  energyAmount <- sqlQuery(connect, query)
  
  #note: each data point is actually units of KW/15 minutes. The sum of 4 points is then in units of KwH 
  #this means that convert a point to kwH, you must multiply that point by 4
  
  odbcCloseAll()
  return (energyAmount[[1]])
  
}

#returns a list of electricities over a range of dates and a building ID
listElectricity <- function(startdate, enddate, building_ID) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  #makes sure that building_ID is valid 
  if (building_ID > 15 | building_ID < 1) {
    print("Invalid building_ID.")
    return (0)
  }
  
  #make query, the sum of all the points in 1 day is the energy usage of that building in KwH 
  query <- paste("select sum(Amount) from BUEnergy.electricity where building_ID = ", building_ID," and time BETWEEN '", startdate," 00:00:00' and '", enddate," 23:59:59' group by date(time);", sep="")
  energyAmount <- sqlQuery(connect, query)
  
  #note: each data point is actually units of KW/15 minutes. The sum of 4 points is then in units of KwH 
  #this means that convert a point to kwH, you must multiply that point by 4
  
  odbcCloseAll()
  return (energyAmount[[1]])
}

showBuildingInfo <- function() {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  #make query to display all information except the building_code column 
  query <- paste("select Building_ID, Address, Building_Name, Construction_Date, Building_Type, Area from BUEnergy.building_info;")
  buildings <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (buildings)
}

plotElectricityHDD <- function(startdate, enddate, basetemp) {
  #plots time in x axis, electricity usage in Y axis for each day between start and end 
  #HDD in another line for each day between start and end 
  
  #print(daylist)
  HDDlist<-listHDD(startdate, enddate, basetemp)
  #CDDlist<-listCDDdate(startdate, enddate, 65)
  
  Eleclist<- listElectricity(startdate, enddate, 1)
  
  plot<-cbind(HDDlist,Eleclist)
  plot(plot)
}

plotElectricityCDD <- function(startdate, enddate, basetemp,building_ID) {
  #plots time in x axis, electricity usage in Y axis for each day between start and end 
  #HDD in another line for each day between start and end 
  
  #print(daylist)
  CDDlist<-listCDD(startdate, enddate, basetemp)
  #CDDlist<-listCDDdate(startdate, enddate, 65)
  datelist<-seq(as.Date(startdate), as.Date(enddate), "1 day") 
  Eleclist<- listElectricity(startdate, enddate, building_ID)
  df<-data.frame(CDD=CDDlist,Elec=Eleclist,time=datelist)
 # plot<-ggplot(data=df, aes(x=CDD, y=Elec,colour=as.integer(time)))+geom_point()+scale_colour_gradient(limits=as.integer(as.Date(c(startdate,enddate))),
  #                                                                                                                low="white", high="blue") +geom_smooth(aes(x=CDD, y=Elec),data=df,method="lm")
  plot<-ggplot(df,aes(x=CDD,y=Elec,colour=as.integer(time))) + geom_point(alpha = 0.6) +
    scale_colour_gradientn(colours=c('red','green','blue'),labels=time)
  return(plot)

  #plot(df) 
  #fit<-lm(Elec~CDD,data=df)
  #abline(fit)
  #print(summary(fit))
}

plotElectricityHDD <- function(startdate, enddate, basetemp, building_ID) {
  #plots time in x axis, electricity usage in Y axis for each day between start and end 
  #HDD in another line for each day between start and end 
  
  #print(daylist)
  HDDlist<-listHDD(startdate, enddate, basetemp)
  #CDDlist<-listCDDdate(startdate, enddate, 65)
  datelist<-seq(as.Date(startdate), as.Date(enddate), "1 day") 
  Eleclist<- listElectricity(startdate, enddate, building_ID)
  classlist<-listClassDay (startdate, enddate)
  df<-data.frame(HDD=HDDlist,Elec=Eleclist,time=datelist)
  # plot<-ggplot(data=df, aes(x=CDD, y=Elec,colour=as.integer(time)))+geom_point()+scale_colour_gradient(limits=as.integer(as.Date(c(startdate,enddate))),
  #                                                                                                                low="white", high="blue") +geom_smooth(aes(x=CDD, y=Elec),data=df,method="lm")
  plot<-ggplot(df,aes(x=HDD,y=Elec,colour=as.integer(time))) + geom_point(alpha = 0.6,shape=factor(classlist)) +
    scale_colour_gradientn(colours=c('red','green','blue'),labels=time)
  return(plot)
  
  #plot(df) 
  #fit<-lm(Elec~CDD,data=df)
  #abline(fit)
  #print(summary(fit))
}


plotHDDCDD <- function(startdate, enddate, basetemp, building_ID) {
  HDDlist<-listHDD(startdate, enddate, basetemp)
  CDDlist<-listCDD(startdate, enddate, basetemp)
  class<-listClassDay(startdate, enddate)
  datelist<-seq(as.Date(startdate), as.Date(enddate), "1 day") 
  Eleclist<- listElectricity(startdate, enddate, building_ID)
  ts.H<-ts(HDDlist)
  ts.C<-ts(CDDlist)
  ts.E<-ts(Eleclist)
  par(mfrow=c(1,1))
  #plot(ts.H,main=paste('basetemp ',basetemp,'building ',building_ID,startdate,enddate))
  #plot(ts.C,main=paste('basetemp ',basetemp,'building ',building_ID,startdate,enddate))
  #plot(ts.E,main=paste('basetemp ',basetemp,'building ',building_ID,startdate,enddate))
  df <- data.frame(time = datelist, CDD = CDDlist, HDD=HDDlist,Elec=Eleclist,class=as.factor(class))
  plot<-ggplot() + geom_point(data=df, aes(x=time, y=Elec,colour=CDD),size=2.5)+facet_grid(. ~ class)+ggtitle(paste('basetemp ',basetemp,'building ',building_ID,startdate,enddate))
  plot2<-ggplot() + geom_point(data=df, aes(x=time, y=Elec,colour=HDD),size=2.5)+facet_grid(. ~ class)+ggtitle(paste('basetemp ',basetemp,'building ',building_ID,startdate,enddate))
  library(gridExtra) 
  multi<-grid.arrange(plot, plot2, ncol=1)
  
  CDDElec<-subset(df,CDD>0|class==1)
  #plot(CDDElec$CDD,CDDElec$Elec,main=paste('basetemp ',basetemp,'building ',building_ID,startdate,enddate))
  fit<-lm(CDDElec$Elec~CDDElec$CDD)
  #abline(fit)
  print(summary(fit))
  sub<-subset(df,CDD==0)
  print(mean(sub$Elec,na.rm=T))
  return(multi)
}

listClassDay <- function(startdate, enddate) {
  #connect to BUEnergy database
  library(RODBC)
  connect <- odbcConnect(dsn="BUEnergy", uid="energy", pwd = "sjuSH64xPq9qBxQkEvve")
  
  query <- paste("select if(class='C' || class='SC1' || class='SC' || class='SC2',if(weekdays = 'Sa'|| weekdays = 'Su',FALSE,TRUE),FALSE)  from BUEnergy.calendar where time BETWEEN '", startdate," 00:00:00' and '", enddate," 23:59:59' group by date(time);", sep="")
  HDD <- sqlQuery(connect, query)
  
  odbcCloseAll()
  return (HDD[[1]])
}

#--------testing functions 
month <- 1
day <- 1
year <- 2010
startdate <- makeDate(month,day,year)
endmonth <-2
endday <- 21
endyear <- 2014
enddate <- makeDate(endmonth, endday, endyear)

basetemp <- 60
building_ID <- 14

#single date functions 
# print(paste("Temperature for ",startdate,": ", singleTemp(startdate), sep=""))
# print(paste("Electricity for ",startdate," for building ",building_ID,": ", singleElectricity(startdate, building_ID), sep=""))
# print(paste("HDD for ",startdate, " with base temp ", basetemp,": ", singleHDD(startdate, basetemp), sep=""))
# print(paste("CDD for ",startdate, " with base temp ", basetemp,": ", singleCDD(startdate, basetemp), sep=""))


#average over range functions 
# print(paste("Temperature from ",startdate," to ",enddate,": ",averageTemp(startdate, enddate), sep=""))
# print(paste("HDDs from ",startdate," to ",enddate,": ",averageHDD(startdate, enddate, basetemp), sep=""))
# print(paste("CDDs from ",startdate," to ",enddate,": ",averageCDD(startdate, enddate, basetemp), sep=""))

#list of range functions 
# print(paste("Temperatures from ",startdate," to ",enddate,": ", sep=""))
# print(listTemp(startdate, enddate))
# print(paste("HDDs from ",startdate," to ",enddate,": ", sep=""))
# print(listHDD(startdate, enddate, basetemp))
# print(paste("CDDs from ",startdate," to ",enddate,": ", sep=""))
# print(listCDD(startdate, enddate, basetemp))
# print(paste("Electricities from ",startdate," to ",enddate," for building ",building_ID,": ", sep=""))
# print(listElectricity(startdate, enddate, building_ID))

#plot functions
# plotElectricityHDD(startdate, enddate, basetemp)
# plotElectricityCDD(startdate, enddate, basetemp)
#plotHDDCDD(startdate, enddate, basetemp)

#lonely info function
#print(showBuildingInfo())  


plotElectricityHDD (startdate, enddate, basetemp, building_ID)
#plotElectricityCDD (startdate, enddate, basetemp, building_ID)

plotHDDCDD(startdate, enddate, basetemp,building_ID)




