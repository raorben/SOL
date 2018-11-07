library(googlesheets)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)


# bring in data -----------------------------------------------------------
a<-gs_ls()#makes list of googlesheets in drive

a$sheet_title[13]
sheetCOMU <- gs_title("COMU_Repro_YHONA_2018_v11August")

# list worksheets
gs_ws_ls(sheetCOMU)

ColRock <- gs_read(ss=sheetCOMU, ws = "Colony Rock")
head(ColRock) #top 5 lines of data.frame
tail(ColRock) #last 5 lines of data.frame
str(ColRock) #structure - really confusing
colnames(ColRock) #column names
unique(ColRock$`5-Jul`)
unique(ColRock$`20-Jun`)
length(ColRock)

#Google R find unique factors in dataframe multiple columns
as.character(unique(unlist(ColRock[,6:76])))#I did some Googleing to get this line


# RESHAPE dataframe  ------------------------------------------------------
head(ColRock[,1:10])

#New Column Names: SubColony,Plot,NestNumber,Color,Year,Date,Contents
#                  CR,1,1,B/DS,2018,6-03-2018,EBL

colnames(ColRock)
mdata <- melt(ColRock, id=c("SubColony","Year","Plot","Nest Number","Color"))
head(mdata)
nrow(mdata) #number of observations!
colnames(mdata)<-c("SubColony","Year","Plot","NestNumber","Color","Date","Contents")
head(mdata)
#make the Date into a dateformat
unique(mdata$Date)
#days as 1 or two digets, months all as three letters

mdata$Date1<-paste0(mdata$Date,"-",mdata$Year)
unique(mdata$Date1)#NOT A GOOD IDEA
mdata$Date1<-paste0(mdata$Date,"-2018")
unique(mdata$Date1)#OK!

mdata$Date2<-as.Date(mdata$Date1,format = "%d-%mmm-%yyyy")
unique(mdata$Date2)#NOPE :()
mdata$Date2<-as.Date(mdata$Date1,format = "%d-%b-%y")
unique(mdata$Date2)#closer...but the future suggests something is wrong
mdata$Date2<-as.Date(mdata$Date1,format = "%d-%b-%Y")
unique(mdata$Date2)#yup!
str(mdata$Date2)

head(mdata)

mdata<-mdata%>%select(SubColony,Plot,NestNumber,Color,Date2,Contents)
head(mdata)
mdata<-mdata%>%dplyr::filter(is.na(SubColony)==FALSE)
head(mdata)

# make a plot of number of nests per day ----------------------------------
str(mdata$NestNumber)

#removes nests with no nest contents for that day
a<-mdata%>%filter(is.na(Contents)==FALSE)

#makes a dataframe with a count of the number of distince nests for each day
b<-a%>%group_by(Plot,Date2)%>%summarise(n=n_distinct(NestNumber))
head(b)


ggplot()+
  geom_line(data=b,aes(x=Date2,y=n))#crazy & something is not quite right!

ggplot()+
  geom_line(data=b,aes(x=Date2,y=n,group=Plot)) #look we have groups

ggplot()+
  geom_line(data=b,aes(x=Date2,y=n,group=Plot,color=Plot)) #now they are colored as a continious variable

ggplot()+
  geom_line(data=b,aes(x=Date2,y=n,group=Plot,color=as.factor(Plot))) #now the groups are colored as a factor

ggplot()+
  geom_line(data=b,aes(x=Date2,y=n,group=Plot,color=as.factor(Plot)))+
  facet_wrap(~Plot)


# removed “Dropped” Nests and replot --------------------------------------
colnames(mdata)

mdata <-mdata %>% 
  mutate(key = paste0(Plot,  "_", NestNumber))
dn_IDs<-mdata %>% 
  filter(Contents=="DROP") %>%
  distinct(key)

#selects out just nests that were not dropped. 
mdata_keep<-mdata%>%
  dplyr::filter(!(key %in% dn_IDs$key))

#makes a dataframe with a count of the number of distince nests for each day
#same code as before but all in one "pipe line"
nestsum<-mdata_keep%>%
  filter(is.na(Contents)==FALSE)%>%
  group_by(Plot,Date2)%>%summarise(n=n_distinct(NestNumber))
head(nestsum)

ggplot()+
  geom_line(data=nestsum,aes(x=Date2,y=n,group=Plot,color=as.factor(Plot)))+
  facet_wrap(~Plot,ncol = 3)+
  scale_x_date(date_labels =("%Y-%m-%d"))+
  theme_classic()

ggplot()+
  geom_line(data=nestsum,aes(x=Date2,y=n,group=Plot,color=as.factor(Plot)))+
  facet_wrap(~Plot,ncol = 3)+
  scale_x_date(date_labels =("%b"))+ #%b = three letter months for labels
  theme_classic()


