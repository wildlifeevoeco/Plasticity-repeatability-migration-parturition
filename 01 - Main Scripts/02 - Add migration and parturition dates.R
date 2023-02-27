############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, QMR Webber, E Vander Wal #########################################################

############# Script 2: Appending parturition dates from Bonar et al and migration mapper data #############

############################################################################################################
############################################################################################################

library(data.table)
library(lubridate)

### Read migration start/end times

Mig<-read.csv("Output/MigrationMapper/tabSixOutputs/migtime.csv")

### Need to append herd data to MM output for join. Join using main data:

data<-readRDS("Output/Locs/CleanData.RDS")
## Make field for joining
data$id_yr<-paste(data$ID,data$year,sep="_")

## Remove individuals that didn't migrate 
Mig<-subset(Mig,springMig==1 & notes!="nm")

### Clean it up a bit:
Mig$nsdYear<-NULL
Mig$startFall<-NULL
Mig$endFall<-NULL
Mig$springMig<-NULL
Mig$fallMig<-NULL
Mig$notes<-NULL
Mig$moveType<-NULL
Mig$fallMigDst<-NULL
Mig$classifiedBy<-NULL
Mig$X<-NULL


### Adding in Julian date versions of all dates
Mig$StartMig<-yday(Mig$startSpring)
Mig$EndMig<-yday(Mig$endSpring)


### Remove original date columns
Mig$startSpring<-NULL
Mig$endSpring<-NULL
Mig$newUid<-NULL

### Truncate migration date. If migration end is after parturition, 
### use parturition date as migration end

data<-merge(data,Mig,by="id_yr")

data$Month<-NULL
data$Day<-NULL
data$Hour<-NULL
data$Minute<-NULL
data$Second<-NULL
data$burst<-data$id_yr
data$id_yr<-NULL
data$burst<-as.factor(data$burst)

##### Add in the parturition dates

Surv<-read.csv("Inputs/Parturition/AllHerdsSurvival2hrMR.csv")
Surv$X<-NULL
Surv$PBMcalf<-yday(Surv$CalvingDate)
Surv$Calved<-NULL
Surv$CalvingDate<-NULL
Surv$LossDate<-NULL

Surv$surv<-ifelse(Surv$Lost==TRUE,0,1)

## Read in manually estimated dates

Part_update<-read.csv("BRNupdate_complete.csv")
Part_up<-data.frame(Part_update$ID.year,Part_update$NewDate)

colnames(Part_up)<-c("IDyear","CalfDateManual")

Surv2<-merge(Surv,Part_up,by="IDyear")
Surv2$Lost<-NULL

## Replace all dates that were estimated as day 142 with the manually estimated date

Surv2$CalfDateHybrid<-ifelse(Surv2$PBMcalf==142,Surv2$CalfDateManual,Surv2$PBMcalf)
data$IDyear<-paste(data$Herd,data$burst,sep="_")
data2<-merge(data,Surv2,by="IDyear")

## End of calving season (4 weeks after parturition)

data2$EndCalving<-data2$CalfDateHybrid+28


data2 <- data2[order(data2$ID, data2$Time),]

sub<-subset(data2,ID=="mr2009a14")

data2<-subset(data2,JDay>=StartMig)
IDPre<-unique(droplevels(data2$ID))
data2<-subset(data2,JDay<=EndCalving)
IDPost<-unique(droplevels(data2$ID))

data<-data2

data$IDyear<-as.factor(data$IDyear)

## Order the data
data<-data[order(data$Herd,data$ID,data$Time),]

## Save the data
saveRDS(data,"Output/Locs/CleanDataSeasonal.RDS")

#### Extra section: reading in the data and calculating the fix rate:

data<-readRDS("Output/Locs/CleanDataSeasonal.RDS")

head(data)

library(adehabitatLT)
data$diftime<-data$Time

data<-droplevels(data)

Traj<-as.ltraj(xy=data.frame(data$x,data$y), date=data$Time, 
               id=data$ID, burst=data$IDyear, 
               proj4string = CRS('+proj=utm +zone=21 ellps=WGS84'))

head(Traj)

data2<-ld(Traj)
head(data2)

data2$fixRate<-data2$dt/3600

summary(data2$fixRate)

hist(data2$fixRate,breaks = seq(0,80,0.25))

data3<-subset(data2,fixRate<8)

nrow(data3)/nrow(data2)

hist(data3$fixRate)

data1h<-subset(data2,fixRate>0.75 & fixRate<1.25)
data2h<-subset(data2,fixRate>1.75 & fixRate<2.25)
data4h<-subset(data2,fixRate>3.75 & fixRate<4.25)
data5h<-subset(data2,fixRate>4.75 & fixRate<5.25)

nrow(data1h)/nrow(data2)
nrow(data2h)/nrow(data2)
nrow(data4h)/nrow(data2)
nrow(data5h)/nrow(data2)

sum(nrow(data1h)/nrow(data2)+nrow(data2h)/nrow(data2)+
      nrow(data4h)/nrow(data2)+nrow(data5h)/nrow(data2))

################### END ########################