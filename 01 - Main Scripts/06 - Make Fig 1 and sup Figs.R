############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn MR Webber, E Vander Wal ####################################################

############# Script 6: Plots of timing of snow melt, green up, migration and parturition (Fig. 1 and sup) #

############################################################################################################
############################################################################################################

phenol<-readRDS("Output/Phenology/medianDates.RDS")
phenol$Herd<-substr(phenol$Pop_year,1,4)
phenol$Herd<-as.factor(phenol$Herd)

phenol$col<-c("#40F99B","#22577A","#774C60","#F24236","#F6AE2D")[phenol$Herd]

### Correlatin between timing of snowmelt and timing of green-up (Fig S1.5)

pdf("Figures/SnowGreenCor.pdf",width=4.5,height=4.5)

par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(1,1))
plot(phenol$GreenUpDate~phenol$SnowOffDate,col=phenol$col,xlab=NA,ylab=NA,
     xaxt='n',yaxt='n',ylim=c(135,163),pch=16)
months<-c("January","February","March","April","May","June","July","August","September",
          "October","November","Decemeber")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.09)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=1.6)
axis(1,at=c(95,100,105,110,115,120,125,130,135,140,145,150), 
     labels=c("5","10","15","20","25","30","5","10","15","20","25","30"),tck=-0.02,padj=-0.5)
axis(2,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.07)
axis(2,at=c(95,100,105,110,115,120,125,130,135,140,145,150,156,161,166,171), 
     labels=c("5","10","15","20","25","30","5","10","15","20","25","30","5","10","15","20"),tck=-0.02,las=1)
mtext("May",side=2,adj=0.28,padj=-3.5)
mtext("June",side=2,adj=0.88,padj=-3.5)
mtext("Date of snowmelt",side=1,padj=4,cex=1.2)
mtext("Date of peak green-up",side=2,padj=-4.5,cex=1.2)
abline(lm(phenol$GreenUpDate~phenol$SnowOffDate))
legend(c("Buchans","Grey River","Lapoile", "Middle Ridge","Topsails"),
       col=c("#40F99B","#22577A","#774C60","#F24236","#F6AE2D"),pch=16,x=124,y=145,cex=0.8)

dev.off()

summary(lm(phenol$GreenUpDate~phenol$SnowOffDate))
0.303-(0.0962*1.96)
0.303+(0.0962*1.96)

########## Phenology plots ##################

### Figure 1

####### Middle Ridge ####

PhenolSnow<-readRDS("Output/Phenology/SnowOffDates.RDS")
PhenolGreen<-readRDS("Output/Phenology/GreenUpDates.RDS")
BRN<-readRDS("Output/DataForBRN.RDS")

tab<-table(BRN$ID)
BRN <- droplevels(BRN[BRN$ID %in% names(tab)[tab >= 3],,drop=FALSE])

SnowOff<-PhenolSnow[,25:28]
GreenUp<-PhenolGreen[,25:28]


BRNmr<-subset(BRN, Herd=="MIDRIDGE")
length(unique(BRNmr$ID))
BRNmr<-droplevels(BRNmr)
BRNmr$year<-as.character(BRNmr$year)
BRNmr<-BRNmr[order(BRNmr$year),]
BRNmr$year<-as.factor(BRNmr$year)
BRNmr$col <- c("#40F99B","#22577A","#774C60","#F24236")[BRNmr$year]

y<-seq(0,1,by=1/(nrow(BRNmr)-1))
nrow(subset(BRNmr,year=="2010"))
s2010<-0
e2010<-s2010+(nrow(subset(BRNmr,year=="2010"))/nrow(BRNmr))
s2011<-e2010
e2011<-s2011+(nrow(subset(BRNmr,year=="2011"))/nrow(BRNmr))
s2012<-e2011
e2012<-s2012+(nrow(subset(BRNmr,year=="2012"))/nrow(BRNmr))
s2013<-e2012
e2013<-s2013+(nrow(subset(BRNmr,year=="2013"))/nrow(BRNmr))


pdf("Figures/Fig1-PhenologyPlots.pdf",width=5.5,height=5.5,pointsize=12)
#png("Figures/Fig1-PhenologyPlots.png",width=5.5,height=5.5,pointsize=12, units = "in", res=600)

par(mar=c(0.5,0.5,0.5,0.5),oma=c(4,4,0.5,0.5))

m<-cbind(c(1,1,1,1,2,2),c(1,1,1,1,2,2))

layout(m)

plot(1:99~SnowOff$MIDRIDGE_2010,type="l",xlim=c(45,200),col="#40F99B",lwd=3,
     xlab=NA,xaxt="n",ylab=NA,las=1)
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.03)
lines(1:99~SnowOff$MIDRIDGE_2011,type="l",col="#22577A",lwd=3)
lines(1:99~SnowOff$MIDRIDGE_2012,type="l",col="#774C60",lwd=3)
lines(1:99~SnowOff$MIDRIDGE_2013,type="l",col="#F24236",lwd=3)

lines(1:99~GreenUp$MIDRIDGE_2010,type="l",col="#40F99B",lty=3,lwd=3)
lines(1:99~GreenUp$MIDRIDGE_2011,type="l",col="#22577A",lty=3,lwd=3)
lines(1:99~GreenUp$MIDRIDGE_2012,type="l",col="#774C60",lty=3,lwd=3)
lines(1:99~GreenUp$MIDRIDGE_2013,type="l",col="#F24236",lty=3,lwd=3)
legend(legend=c("2010","2011","2012","2013","Snowmelt","Green-up"),
       col=c("#40F99B","#22577A","#774C60","#F24236","black","black"),
       lty=c(1,1,1,1,1,3),
       x=160,y=48, lwd=3, bty='n')

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("a)",x=0.95,y=0.9,cex=1.2)
par(usr = usr) # restore original user coordinates

abline(h=50,lty=2)

mtext("Percent pixels",side=2,padj=-2.5)

plot(c(1,2),c(1,2),xlim=c(45,200),ylim=c(1,0),col="white",ylab=NA,xlab=NA,
     yaxt='n',xaxt='n')
months<-c("January","February","March","April","May","June","July","August","September",
          "October","November","Decemeber")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.15)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=0,cex=2)
arrows(x0=BRNmr$StartMig,x1=BRNmr$EndMig,y0=y,y1=y,length=0,col=BRNmr$col)
points(x=BRNmr$CalfDateHybrid_man,y=y,pch=16,cex=1,col=BRNmr$col)
arrows(x0=SnowOff$MIDRIDGE_2010[50],x1=SnowOff$MIDRIDGE_2010[50],y0=s2010,y1=e2010,length=0,lwd=3)
arrows(x0=SnowOff$MIDRIDGE_2011[50],x1=SnowOff$MIDRIDGE_2011[50],y0=s2011,y1=e2011,length=0,lwd=3)
arrows(x0=SnowOff$MIDRIDGE_2012[50],x1=SnowOff$MIDRIDGE_2012[50],y0=s2012,y1=e2012,length=0,lwd=3)
arrows(x0=SnowOff$MIDRIDGE_2013[50],x1=SnowOff$MIDRIDGE_2013[50],y0=s2013,y1=e2013,length=0,lwd=3)

arrows(x0=GreenUp$MIDRIDGE_2010[50],x1=GreenUp$MIDRIDGE_2010[50],y0=s2010,y1=e2010,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$MIDRIDGE_2011[50],x1=GreenUp$MIDRIDGE_2011[50],y0=s2011,y1=e2011,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$MIDRIDGE_2012[50],x1=GreenUp$MIDRIDGE_2012[50],y0=s2012,y1=e2012,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$MIDRIDGE_2013[50],x1=GreenUp$MIDRIDGE_2013[50],y0=s2013,y1=e2013,length=0,lty=2,lwd=1.5)


usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("b)",x=0.95,y=0.9,cex=1.2)

legend(legend=c("Migration","Parturition","Median snowmelt","Median green-up"),
       x=0.73,y=0.7,lty=c(1,NA,NA,NA),pch=c(NA,16,NA,NA),lwd=c(1,NA,NA,NA),bty="n")
segments(x0=0.756,x1=0.756,y0=0.47,y1=0.4,lwd=2)
segments(x0=0.756,x1=0.756,y0=0.38,y1=0.30,lwd=1,lty=2)

par(usr = usr) # restore original user coordinates

mtext("Date",side=1,padj=3)

dev.off()


######################################################

##### Phenology plots, all other herds ###############

######################################################

## Figure S1.3

SnowOff<-readRDS("Output/Phenology/SnowOffDates.RDS")
GreenUp<-readRDS("Output/Phenology/GreenUpDates.RDS")
BRN<-readRDS("Output/DataForBRN.RDS")

tab<-table(BRN$ID)
BRN <- droplevels(BRN[BRN$ID %in% names(tab)[tab >= 3],,drop=FALSE])

unique(BRN$year)

wes_palettes

BRN<-subset(BRN,Herd!="MIDRIDGE")

BRN<-droplevels(BRN)
BRN$year<-as.character(BRN$year)
BRN<-BRN[order(BRN$year),]
BRN$year<-as.factor(BRN$year)

colors<-c("#F6AE2D","#AD11A6","#0B5B18","#40F99B","#22577A","#774C60")

BRN$col <- colors[BRN$year]


png("Figures/FigS2-PhenologyPlotsOtherHerds.png",width=7.5,height=9,pointsize=12, units="in",res=600)

par(mar=c(1,0.5,0.5,0.5),oma=c(4,4,0.5,0.5))

m<-cbind(c(1,1,1,1,2,2,5,5,5,5,6,6),c(1,1,1,1,2,2,5,5,5,5,6,6),
         c(3,3,3,3,4,4,7,7,7,7,8,8),c(3,3,3,3,4,4,7,7,7,7,8,8))

layout(m)

LetterCEX<-1.8

### BUCHANS 

plot(1:99~SnowOff$BUCHANS_2007,type="l",xlim=c(45,200),col=colors[1],lwd=3,
     xlab=NA,xaxt="n",ylab=NA,las=1)
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.03,adj=0.5)
lines(1:99~SnowOff$BUCHANS_2008,type="l",col=colors[2],lwd=3)
lines(1:99~SnowOff$BUCHANS_2009,type="l",col=colors[3],lwd=3)
lines(1:99~SnowOff$BUCHANS_2010,type="l",col=colors[4],lwd=3)
lines(1:99~SnowOff$BUCHANS_2011,type="l",col=colors[5],lwd=3)
lines(1:99~SnowOff$BUCHANS_2012,type="l",col=colors[6],lwd=3)


lines(1:99~GreenUp$BUCHANS_2007,type="l",col=colors[1],lty=3,lwd=3)
lines(1:99~GreenUp$BUCHANS_2008,type="l",col=colors[2],lty=3,lwd=3)
lines(1:99~GreenUp$BUCHANS_2009,type="l",col=colors[3],lty=3,lwd=3)
lines(1:99~GreenUp$BUCHANS_2010,type="l",col=colors[4],lty=3,lwd=3)
lines(1:99~GreenUp$BUCHANS_2011,type="l",col=colors[5],lty=3,lwd=3)
lines(1:99~GreenUp$BUCHANS_2012,type="l",col=colors[6],lty=3,lwd=3)
legend(legend=c("2007","2008","2009","2010","2011","2012","Snowmelt","Green-up"),
       col=c(colors,"black","black"),
       lty=c(1,1,1,1,1,1,1,3),
       x=160,y=48, lwd=3, bty='n',cex=1)

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("a)",x=0.95,y=0.9,cex=LetterCEX)
text("Buchans",x=0.2,y=0.9,cex=2)
par(usr = usr) # restore original user coordinates

abline(h=50,lty=2)

mtext("Percent pixels",side=2,padj=-2.5)

BRNBuch<-subset(BRN,Herd=="BUCHANS")

y<-seq(0,1,by=1/(nrow(BRNBuch)-1))

s2007<-0
e2007<-s2007+(nrow(subset(BRNBuch,year=="2007"))/nrow(BRNBuch))
s2008<-e2007
e2008<-s2008+(nrow(subset(BRNBuch,year=="2008"))/nrow(BRNBuch))
s2009<-e2008
e2009<-s2009+(nrow(subset(BRNBuch,year=="2009"))/nrow(BRNBuch))
s2010<-e2009
e2010<-s2010+(nrow(subset(BRNBuch,year=="2010"))/nrow(BRNBuch))
s2011<-e2010
e2011<-s2011+(nrow(subset(BRNBuch,year=="2011"))/nrow(BRNBuch))
s2012<-e2011
e2012<-s2012+(nrow(subset(BRNBuch,year=="2012"))/nrow(BRNBuch))




plot(c(1,2),c(1,2),xlim=c(45,200),ylim=c(1,0),col="white",ylab=NA,xlab=NA,
     yaxt='n',xaxt='n')
months<-c("January","February","March","April","May","June","July","August","September",
          "October","November","Decemeber")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.06)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1.3,cex=2)
arrows(x0=BRNBuch$StartMig,x1=BRNBuch$EndMig,y0=y,y1=y,length=0,col=BRNBuch$col)
points(x=BRNBuch$CalfDateHybrid,y=y,pch=16,cex=1,col=BRNBuch$col)

arrows(x0=SnowOff$BUCHANS_2007[50],x1=SnowOff$BUCHANS_2007[50],y0=s2007,y1=e2007,length=0,lwd=3)
arrows(x0=SnowOff$BUCHANS_2008[50],x1=SnowOff$BUCHANS_2008[50],y0=s2008,y1=e2008,length=0,lwd=3)
arrows(x0=SnowOff$BUCHANS_2009[50],x1=SnowOff$BUCHANS_2009[50],y0=s2009,y1=e2009,length=0,lwd=3)
arrows(x0=SnowOff$BUCHANS_2010[50],x1=SnowOff$BUCHANS_2010[50],y0=s2010,y1=e2010,length=0,lwd=3)
arrows(x0=SnowOff$BUCHANS_2011[50],x1=SnowOff$BUCHANS_2011[50],y0=s2011,y1=e2011,length=0,lwd=3)
arrows(x0=SnowOff$BUCHANS_2012[50],x1=SnowOff$BUCHANS_2012[50],y0=s2012,y1=e2012,length=0,lwd=3)

arrows(x0=GreenUp$BUCHANS_2007[50],x1=GreenUp$BUCHANS_2007[50],y0=s2007,y1=e2007,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$BUCHANS_2008[50],x1=GreenUp$BUCHANS_2008[50],y0=s2008,y1=e2008,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$BUCHANS_2009[50],x1=GreenUp$BUCHANS_2009[50],y0=s2009,y1=e2009,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$BUCHANS_2010[50],x1=GreenUp$BUCHANS_2010[50],y0=s2010,y1=e2010,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$BUCHANS_2011[50],x1=GreenUp$BUCHANS_2011[50],y0=s2011,y1=e2011,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$BUCHANS_2012[50],x1=GreenUp$BUCHANS_2012[50],y0=s2012,y1=e2012,length=0,lty=2,lwd=1.5)
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("b)",x=0.95,y=0.9,cex=LetterCEX)

legend(legend=c("Migration","Parturition","Median", "  snowmelt","Median", "  green-up"),
       x=0.73,y=0.8,lty=c(1,NA,NA,NA,NA,NA),pch=c(NA,16,NA,NA,NA,NA),lwd=c(1,NA,NA,NA,NA,NA),bty="n")
xdist<-0.77
segments(x0=xdist,x1=xdist,y0=0.49,y1=0.32,lwd=2)
segments(x0=xdist,x1=xdist,y0=0.27,y1=0.07,lwd=1,lty=2)


par(usr = usr) # restore original user coordinates



### GREY RIVER

plot(1:99~SnowOff$GREY_2007,type="l",xlim=c(45,200),col=colors[1],lwd=3,
     xlab=NA,xaxt="n",ylab=NA,las=1,yaxt="n")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.03)
axis(2,at=c(0,20,40,60,80,100),labels=NA)
lines(1:99~SnowOff$GREY_2008,type="l",col=colors[2],lwd=3)
lines(1:99~SnowOff$GREY_2009,type="l",col=colors[3],lwd=3)
lines(1:99~SnowOff$GREY_2010,type="l",col=colors[4],lwd=3)
lines(1:99~SnowOff$GREY_2011,type="l",col=colors[5],lwd=3)
lines(1:99~SnowOff$GREY_2012,type="l",col=colors[6],lwd=3)


lines(1:99~GreenUp$GREY_2007,type="l",col=colors[1],lty=3,lwd=3)
lines(1:99~GreenUp$GREY_2008,type="l",col=colors[2],lty=3,lwd=3)
lines(1:99~GreenUp$GREY_2009,type="l",col=colors[3],lty=3,lwd=3)
lines(1:99~GreenUp$GREY_2010,type="l",col=colors[4],lty=3,lwd=3)
lines(1:99~GreenUp$GREY_2011,type="l",col=colors[5],lty=3,lwd=3)
lines(1:99~GreenUp$GREY_2012,type="l",col=colors[6],lty=3,lwd=3)

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("c)",x=0.95,y=0.9,cex=LetterCEX)
text("Grey River",x=0.2,y=0.9,cex=2)
par(usr = usr) # restore original user coordinates

abline(h=50,lty=2)

BRNGrey<-subset(BRN,Herd=="GREY")

y<-seq(0,1,by=1/(nrow(BRNGrey)-1))

s2007<-0
e2007<-s2007+(nrow(subset(BRNGrey,year=="2007"))/nrow(BRNGrey))
s2008<-e2007
e2008<-s2008+(nrow(subset(BRNGrey,year=="2008"))/nrow(BRNGrey))
s2009<-e2008
e2009<-s2009+(nrow(subset(BRNGrey,year=="2009"))/nrow(BRNGrey))
s2010<-e2009
e2010<-s2010+(nrow(subset(BRNGrey,year=="2010"))/nrow(BRNGrey))
s2011<-e2010
e2011<-s2011+(nrow(subset(BRNGrey,year=="2011"))/nrow(BRNGrey))
s2012<-e2011
e2012<-s2012+(nrow(subset(BRNGrey,year=="2012"))/nrow(BRNGrey))




plot(c(1,2),c(1,2),xlim=c(45,200),ylim=c(1,0),col="white",ylab=NA,xlab=NA,
     yaxt='n',xaxt='n')
months<-c("January","February","March","April","May","June","July","August","September",
          "October","November","Decemeber")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.06)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1.3,cex=2)
arrows(x0=BRNGrey$StartMig,x1=BRNGrey$EndMig,y0=y,y1=y,length=0,col=BRNGrey$col)
points(x=BRNGrey$CalfDateHybrid,y=y,pch=16,cex=1,col=BRNGrey$col)

arrows(x0=SnowOff$GREY_2007[50],x1=SnowOff$GREY_2007[50],y0=s2007,y1=e2007,length=0,lwd=3)
arrows(x0=SnowOff$GREY_2008[50],x1=SnowOff$GREY_2008[50],y0=s2008,y1=e2008,length=0,lwd=3)
arrows(x0=SnowOff$GREY_2009[50],x1=SnowOff$GREY_2009[50],y0=s2009,y1=e2009,length=0,lwd=3)
arrows(x0=SnowOff$GREY_2010[50],x1=SnowOff$GREY_2010[50],y0=s2010,y1=e2010,length=0,lwd=3)
arrows(x0=SnowOff$GREY_2011[50],x1=SnowOff$GREY_2011[50],y0=s2011,y1=e2011,length=0,lwd=3)
arrows(x0=SnowOff$GREY_2012[50],x1=SnowOff$GREY_2012[50],y0=s2012,y1=e2012,length=0,lwd=3)

arrows(x0=GreenUp$GREY_2007[50],x1=GreenUp$GREY_2007[50],y0=s2007,y1=e2007,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$GREY_2008[50],x1=GreenUp$GREY_2008[50],y0=s2008,y1=e2008,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$GREY_2009[50],x1=GreenUp$GREY_2009[50],y0=s2009,y1=e2009,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$GREY_2010[50],x1=GreenUp$GREY_2010[50],y0=s2010,y1=e2010,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$GREY_2011[50],x1=GreenUp$GREY_2011[50],y0=s2011,y1=e2011,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$GREY_2012[50],x1=GreenUp$GREY_2012[50],y0=s2012,y1=e2012,length=0,lty=2,lwd=1.5)
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("d)",x=0.95,y=0.9,cex=LetterCEX)

par(usr = usr) # restore original user coordinates


### LAPOILE

plot(1:99~SnowOff$LAPOILE_2007,type="l",xlim=c(45,200),col=colors[1],lwd=3,
     xlab=NA,xaxt="n",ylab=NA,las=1)
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.03)
lines(1:99~SnowOff$LAPOILE_2008,type="l",col=colors[2],lwd=3)
lines(1:99~SnowOff$LAPOILE_2009,type="l",col=colors[3],lwd=3)
lines(1:99~SnowOff$LAPOILE_2010,type="l",col=colors[4],lwd=3)
lines(1:99~SnowOff$LAPOILE_2011,type="l",col=colors[5],lwd=3)
lines(1:99~SnowOff$LAPOILE_2012,type="l",col=colors[6],lwd=3)


lines(1:99~GreenUp$LAPOILE_2007,type="l",col=colors[1],lty=3,lwd=3)
lines(1:99~GreenUp$LAPOILE_2008,type="l",col=colors[2],lty=3,lwd=3)
lines(1:99~GreenUp$LAPOILE_2009,type="l",col=colors[3],lty=3,lwd=3)
lines(1:99~GreenUp$LAPOILE_2010,type="l",col=colors[4],lty=3,lwd=3)
lines(1:99~GreenUp$LAPOILE_2011,type="l",col=colors[5],lty=3,lwd=3)
lines(1:99~GreenUp$LAPOILE_2012,type="l",col=colors[6],lty=3,lwd=3)

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("e)",x=0.95,y=0.9,cex=LetterCEX)
text("Lapoile",x=0.2,y=0.9,cex=2)
par(usr = usr) # restore original user coordinates

abline(h=50,lty=2)

mtext("Percent pixels",side=2,padj=-2.5)

BRNLapo<-subset(BRN,Herd=="LAPOILE")

y<-seq(0,1,by=1/(nrow(BRNLapo)-1))

s2007<-0
e2007<-s2007+(nrow(subset(BRNLapo,year=="2007"))/nrow(BRNLapo))
s2008<-e2007
e2008<-s2008+(nrow(subset(BRNLapo,year=="2008"))/nrow(BRNLapo))
s2009<-e2008
e2009<-s2009+(nrow(subset(BRNLapo,year=="2009"))/nrow(BRNLapo))
s2010<-e2009
e2010<-s2010+(nrow(subset(BRNLapo,year=="2010"))/nrow(BRNLapo))
s2011<-e2010
e2011<-s2011+(nrow(subset(BRNLapo,year=="2011"))/nrow(BRNLapo))
s2012<-e2011
e2012<-s2012+(nrow(subset(BRNLapo,year=="2012"))/nrow(BRNLapo))




plot(c(1,2),c(1,2),xlim=c(45,200),ylim=c(1,0),col="white",ylab=NA,xlab=NA,
     yaxt='n',xaxt='n')
months<-c("January","February","March","April","May","June","July","August","September",
          "October","November","Decemeber")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.06)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1.3,cex=2)
arrows(x0=BRNLapo$StartMig,x1=BRNLapo$EndMig,y0=y,y1=y,length=0,col=BRNLapo$col)
points(x=BRNLapo$CalfDateHybrid,y=y,pch=16,cex=1,col=BRNLapo$col)

arrows(x0=SnowOff$LAPOILE_2007[50],x1=SnowOff$LAPOILE_2007[50],y0=s2007,y1=e2007,length=0,lwd=3)
arrows(x0=SnowOff$LAPOILE_2008[50],x1=SnowOff$LAPOILE_2008[50],y0=s2008,y1=e2008,length=0,lwd=3)
arrows(x0=SnowOff$LAPOILE_2009[50],x1=SnowOff$LAPOILE_2009[50],y0=s2009,y1=e2009,length=0,lwd=3)
arrows(x0=SnowOff$LAPOILE_2010[50],x1=SnowOff$LAPOILE_2010[50],y0=s2010,y1=e2010,length=0,lwd=3)
arrows(x0=SnowOff$LAPOILE_2011[50],x1=SnowOff$LAPOILE_2011[50],y0=s2011,y1=e2011,length=0,lwd=3)
arrows(x0=SnowOff$LAPOILE_2012[50],x1=SnowOff$LAPOILE_2012[50],y0=s2012,y1=e2012,length=0,lwd=3)

arrows(x0=GreenUp$LAPOILE_2007[50],x1=GreenUp$LAPOILE_2007[50],y0=s2007,y1=e2007,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$LAPOILE_2008[50],x1=GreenUp$LAPOILE_2008[50],y0=s2008,y1=e2008,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$LAPOILE_2009[50],x1=GreenUp$LAPOILE_2009[50],y0=s2009,y1=e2009,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$LAPOILE_2010[50],x1=GreenUp$LAPOILE_2010[50],y0=s2010,y1=e2010,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$LAPOILE_2011[50],x1=GreenUp$LAPOILE_2011[50],y0=s2011,y1=e2011,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$LAPOILE_2012[50],x1=GreenUp$LAPOILE_2012[50],y0=s2012,y1=e2012,length=0,lty=2,lwd=1.5)
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("f)",x=0.95,y=0.9,cex=LetterCEX)

par(usr = usr) # restore original user coordinates

mtext("Date",side=1,padj=3)



### TOPSAILS

plot(1:99~SnowOff$TOPSAILS_2007,type="l",xlim=c(45,200),col=colors[1],lwd=3,
     xlab=NA,xaxt="n",ylab=NA,las=1,yaxt="n")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.03)
axis(2,at=c(0,20,40,60,80,100),labels=NA)
lines(1:99~SnowOff$TOPSAILS_2008,type="l",col=colors[2],lwd=3)
lines(1:99~SnowOff$TOPSAILS_2009,type="l",col=colors[3],lwd=3)
lines(1:99~SnowOff$TOPSAILS_2010,type="l",col=colors[4],lwd=3)
lines(1:99~SnowOff$TOPSAILS_2011,type="l",col=colors[5],lwd=3)


lines(1:99~GreenUp$TOPSAILS_2007,type="l",col=colors[1],lty=3,lwd=3)
lines(1:99~GreenUp$TOPSAILS_2008,type="l",col=colors[2],lty=3,lwd=3)
lines(1:99~GreenUp$TOPSAILS_2009,type="l",col=colors[3],lty=3,lwd=3)
lines(1:99~GreenUp$TOPSAILS_2010,type="l",col=colors[4],lty=3,lwd=3)
lines(1:99~GreenUp$TOPSAILS_2011,type="l",col=colors[5],lty=3,lwd=3)

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("g)",x=0.95,y=0.9,cex=LetterCEX)
text("Topsails",x=0.2,y=0.9,cex=2)
par(usr = usr) # restore original user coordinates

abline(h=50,lty=2)


BRNTops<-subset(BRN,Herd=="TOPSAILS")

y<-seq(0,1,by=1/(nrow(BRNTops)-1))

s2007<-0
e2007<-s2007+(nrow(subset(BRNTops,year=="2007"))/nrow(BRNTops))
s2008<-e2007
e2008<-s2008+(nrow(subset(BRNTops,year=="2008"))/nrow(BRNTops))
s2009<-e2008
e2009<-s2009+(nrow(subset(BRNTops,year=="2009"))/nrow(BRNTops))
s2010<-e2009
e2010<-s2010+(nrow(subset(BRNTops,year=="2010"))/nrow(BRNTops))
s2011<-e2010
e2011<-s2011+(nrow(subset(BRNTops,year=="2011"))/nrow(BRNTops))





plot(c(1,2),c(1,2),xlim=c(45,200),ylim=c(1,0),col="white",ylab=NA,xlab=NA,
     yaxt='n',xaxt='n')
months<-c("January","February","March","April","May","June","July","August","September",
          "October","November","Decemeber")
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.06)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1.3,cex=2)
arrows(x0=BRNTops$StartMig,x1=BRNTops$EndMig,y0=y,y1=y,length=0,col=BRNTops$col)
points(x=BRNTops$CalfDateHybrid,y=y,pch=16,cex=1,col=BRNTops$col)

arrows(x0=SnowOff$TOPSAILS_2007[50],x1=SnowOff$TOPSAILS_2007[50],y0=s2007,y1=e2007,length=0,lwd=3)
arrows(x0=SnowOff$TOPSAILS_2008[50],x1=SnowOff$TOPSAILS_2008[50],y0=s2008,y1=e2008,length=0,lwd=3)
arrows(x0=SnowOff$TOPSAILS_2009[50],x1=SnowOff$TOPSAILS_2009[50],y0=s2009,y1=e2009,length=0,lwd=3)
arrows(x0=SnowOff$TOPSAILS_2010[50],x1=SnowOff$TOPSAILS_2010[50],y0=s2010,y1=e2010,length=0,lwd=3)
arrows(x0=SnowOff$TOPSAILS_2011[50],x1=SnowOff$TOPSAILS_2011[50],y0=s2011,y1=e2011,length=0,lwd=3)

arrows(x0=GreenUp$TOPSAILS_2007[50],x1=GreenUp$TOPSAILS_2007[50],y0=s2007,y1=e2007,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$TOPSAILS_2008[50],x1=GreenUp$TOPSAILS_2008[50],y0=s2008,y1=e2008,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$TOPSAILS_2009[50],x1=GreenUp$TOPSAILS_2009[50],y0=s2009,y1=e2009,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$TOPSAILS_2010[50],x1=GreenUp$TOPSAILS_2010[50],y0=s2010,y1=e2010,length=0,lty=2,lwd=1.5)
arrows(x0=GreenUp$TOPSAILS_2011[50],x1=GreenUp$TOPSAILS_2011[50],y0=s2011,y1=e2011,length=0,lty=2,lwd=1.5)
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("h)",x=0.95,y=0.9,cex=LetterCEX)

par(usr = usr) # restore original user coordinates

mtext("Date",side=1,padj=3)


dev.off()

##################################### END #######################################
