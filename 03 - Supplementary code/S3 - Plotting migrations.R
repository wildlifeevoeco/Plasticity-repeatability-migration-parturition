############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn MR Webber, E Vander Wal ####################################################

############# Script S3: Plotting all migrations and calving range points for supplement 2 #################

############################################################################################################
############################################################################################################

library(ggmap)
library(ggplot2)
library(sp)
library(sf)
library(maptools)
library(raster)
library(tidyverse)

data<-readRDS("Output/Locs/CleanDataSeasonal.RDS")

head(data)
dataRed<-data.frame(data$IDyear,data$Herd,data$Time,data$x,data$y,data$JDateTime,data$JDay,data$StartMig,data$EndMig)
data<-dataRed
head(data)
colnames(data)<-c("IDyear","Herd","Time","x","y","JDateTime","JDay","StartMig","EndMig")

#BRN<-readRDS("Output/DataForBNRSept2-22.RDS") ### Only post-migration points
BRN<-readRDS("Output/DataForBRN.RDS")

hist(table(BRN$ID),col="blue",xlab="Years of data")
tab<-table(BRN$ID)

nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 1],,drop=FALSE]))
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 2],,drop=FALSE]))/2
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 3],,drop=FALSE]))/3
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 4],,drop=FALSE]))/4
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 5],,drop=FALSE]))/5
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 6],,drop=FALSE]))/6

order(tab)

BRN <- droplevels(BRN[BRN$ID %in% names(tab)[tab >= 3],,drop=FALSE])

IDyears<-unique(BRN$IDyear)

dataSub<-droplevels(subset(data,IDyear %in% IDyears))

Conifer<-readRDS("Inputs/Landcover/Conifer.tif")
MixedWood<-readRDS("Inputs/Landcover/MixedWood.tif")
Broadleaf<-readRDS("Inputs/Landcover/Broadleaf.tif")
Wetland<-readRDS("Inputs/Landcover/Wetland.tif")
Water<-readRDS("Inputs/Landcover/Water.tif")

Forest<-Broadleaf+Conifer+MixedWood

dataSub$state<-ifelse(dataSub$JDay<dataSub$EndMig,"Mig","After")

ids<-unique(dataSub$IDyear)

png("Figures/MigPlotsPart1.png", width=8, height=10, pointsize = 9, units="in",res=300)
par(mfrow=c(5,5),mar=c(2,2,2,0),oma=c(2,2,0,0))
for(i in 1:25){

sub<-subset(dataSub,IDyear==ids[i])

minx<-min(sub$x)
maxx<-max(sub$x)
miny<-min(sub$y)
maxy<-max(sub$y)
xdif<-maxx-minx
ydif<-maxy-miny

Migr<-subset(sub,state=="Mig")
Post<-subset(sub,state=="After")

plot(Wetland,col=c("transparent","cyan"),legend=FALSE,xlim=c((minx-(xdif*0.1)),(maxx+(xdif*0.1))),
     ylim=c((miny-(ydif*0.1)),(maxy+(ydif*0.1))), main=ids[i])
par(new=TRUE)
plot(Water,col=c("transparent","blue"),legend=FALSE,xlim=c((minx-(xdif*0.1)),(maxx+(xdif*0.1))),
     ylim=c((miny-(ydif*0.1)),(maxy+(ydif*0.1))))
par(new=TRUE)
plot(Forest,col=c("transparent","darkgreen"),legend=FALSE,xlim=c((minx-(xdif*0.1)),(maxx+(xdif*0.1))),
     ylim=c((miny-(ydif*0.1)),(maxy+(ydif*0.1))))
par(new=TRUE)
points(Migr$x,Migr$y,col="red", pch=20)
points(Post$x,Post$y,col="orange",pch=20)
points(Post$x[1],Post$y[1],col="yellow",pch=8,cex=1.5)
print(i)
}
mtext("UTM X, Zone 21 N (m)",side=1,outer=TRUE)
mtext("UTM Y, Zone 21 N (m)",side=2,outer=TRUE)

dev.off()

############################################################################

###################   END   ################################################

############################################################################
