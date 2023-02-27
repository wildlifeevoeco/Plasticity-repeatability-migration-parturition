############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn MR Webber, E Vander Wal ####################################################

############# Script S4: Correlation in birth dates between observers for poor fitting estimates############

############################################################################################################
############################################################################################################

BRN<-readRDS("Output/DataForBRN.RDS")

tab<-table(BRN$ID)
tab

nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 1],,drop=FALSE]))
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 2],,drop=FALSE]))/2
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 3],,drop=FALSE]))/3
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 4],,drop=FALSE]))/4
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 5],,drop=FALSE]))/5
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 6],,drop=FALSE]))/6

order(tab)

## Dropping all ID-years with <3 years of data

BRN <- droplevels(BRN[BRN$ID %in% names(tab)[tab >= 3],,drop=FALSE])

head(BRN)

## Taking only Julian days at 142 (problematic date for model)
BRN2<-subset(BRN,PBMcalf==142)

head(BRN2)

QW_ests<-read.csv("Inputs/Parturition/Validation/CalvingEstimates_QW.csv")
head(QW_ests)
QW_ests$X<-NULL

BRN2<-data.frame(BRN2$IDyear,BRN2$CalfDateManual)

colnames(BRN2)<-c("IDyear","MLest")
colnames(QW_ests)<-c("IDyear","QWest","Conf")

head(BRN2)
head(QW_ests)

BRN_QW<-merge(BRN2,QW_ests,by="IDyear")

head(BRN_QW)


plot(jitter(BRN_QW$MLest),jitter(BRN_QW$QWest),xlab="Mike's estimate",ylab="Quinn's estimate")
abline(a=0,b=1)
cor(BRN_QW$MLest,BRN_QW$QWest)

BRN_QW$dif<-abs(BRN_QW$MLest-BRN_QW$QWest)

hist(BRN_QW$dif+0.1,breaks=seq(0,26,1))

good<-subset(BRN_QW,dif<=5)
bad<-subset(BRN_QW,dif>5)

cor(good$MLest,good$QWest)

par(mfrow=c(1,1))


## Figure S3.1 

plot(jitter(good$MLest),jitter(good$QWest),xlab="MPL estimate, Julian day",ylab="QMRW estimate, Julian day",xlim=c(135,188),ylim=c(135,188))
points(bad$MLest, bad$QWest, col="red")
abline(a=0,b=1)



bad<-droplevels(bad)
ToDrop<-unique(bad$IDyear)
ToDrop

## List of id-years to drop in supplement 3 to test for influence of outliers on final inference

saveRDS(ToDrop,"Output/ToDrop.RDS")

############################################################################

###################   END   ################################################

############################################################################
