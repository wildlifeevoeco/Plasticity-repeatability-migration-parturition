############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn MR Webber, E Vander Wal ####################################################

############# Script S1: Making table of summary stats (Table S1.3) ########################################

############################################################################################################
############################################################################################################

BRN<-readRDS("Output/DataForBRN.RDS")

hist(table(BRN$ID),col="blue",xlab="Years of data")
tab<-table(BRN$ID)
tab

nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 1],,drop=FALSE]))
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 2],,drop=FALSE]))/2
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 3],,drop=FALSE]))/3
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 4],,drop=FALSE]))/4
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 5],,drop=FALSE]))/5
nrow(droplevels(BRN[BRN$ID %in% names(tab)[tab == 6],,drop=FALSE]))/6

order(tab)

BRN <- droplevels(BRN[BRN$ID %in% names(tab)[tab >= 3],,drop=FALSE])

BRN$MigDur<-BRN$EndMig-BRN$StartMig

Buch<-subset(BRN, Herd=="BUCHANS")
Grey<-subset(BRN, Herd=="GREY")
Lapo<-subset(BRN, Herd=="LAPOILE")
MidR<-subset(BRN, Herd=="MIDRIDGE")
Tops<-subset(BRN, Herd=="TOPSAILS")


NumIDs<-c(length(unique(BRN$ID)),
          length(unique(Buch$ID)),
          length(unique(Grey$ID)),
          length(unique(Lapo$ID)),
          length(unique(MidR$ID)),
          length(unique(Tops$ID)))

NumIDYears<-c(length(unique(BRN$IDyear)),
              length(unique(Buch$IDyear)),
              length(unique(Grey$IDyear)),
              length(unique(Lapo$IDyear)),
              length(unique(MidR$IDyear)),
              length(unique(Tops$IDyear)))

StartMigM<-c(median(BRN$StartMig),
             median(Buch$StartMig),
             median(Grey$StartMig),
             median(Lapo$StartMig),
             median(MidR$StartMig),
             median(Tops$StartMig))

StartMigMmin<-c(min(BRN$StartMig),
                min(Buch$StartMig),
                min(Grey$StartMig),
                min(Lapo$StartMig),
                min(MidR$StartMig),
                min(Tops$StartMig))

StartMigMmax<-c(max(BRN$StartMig),
                max(Buch$StartMig),
                max(Grey$StartMig),
                max(Lapo$StartMig),
                max(MidR$StartMig),
                max(Tops$StartMig))

StartMigMform<-format(as.Date(StartMigM,origin = "1899-12-31"), "%b %d")
StartMigMminform<-format(as.Date(StartMigMmin,origin = "1899-12-31"), "%b %d")
StartMigMmaxform<-format(as.Date(StartMigMmax,origin = "1899-12-31"), "%b %d")

MigrationStart<-paste(StartMigMform, " [",StartMigMminform, ", ",StartMigMmaxform,"]", sep="")


EndMigM<-c(median(BRN$EndMig),
           median(Buch$EndMig),
           median(Grey$EndMig),
           median(Lapo$EndMig),
           median(MidR$EndMig),
           median(Tops$EndMig))

EndMigMmin<-c(min(BRN$EndMig),
              min(Buch$EndMig),
              min(Grey$EndMig),
              min(Lapo$EndMig),
              min(MidR$EndMig),
              min(Tops$EndMig))

EndMigMmax<-c(max(BRN$EndMig),
              max(Buch$EndMig),
              max(Grey$EndMig),
              max(Lapo$EndMig),
              max(MidR$EndMig),
              max(Tops$EndMig))

EndMigMform<-format(as.Date(EndMigM,origin = "1899-12-31"), "%b %d")
EndMigMminform<-format(as.Date(EndMigMmin,origin = "1899-12-31"), "%b %d")
EndMigMmaxform<-format(as.Date(EndMigMmax,origin = "1899-12-31"), "%b %d")

MigrationEnd<-paste(EndMigMform, " [",EndMigMminform, ", ",EndMigMmaxform,"]", sep="")


MigDuration<-c(median(BRN$MigDur),
               median(Buch$MigDur),
               median(Grey$MigDur),
               median(Lapo$MigDur),
               median(MidR$MigDur),
               median(Tops$MigDur))

MigDurationMin<-c(min(BRN$MigDur),
                  min(Buch$MigDur),
                  min(Grey$MigDur),
                  min(Lapo$MigDur),
                  min(MidR$MigDur),
                  min(Tops$MigDur))

MigDurationMax<-c(max(BRN$MigDur),
                  max(Buch$MigDur),
                  max(Grey$MigDur),
                  max(Lapo$MigDur),
                  max(MidR$MigDur),
                  max(Tops$MigDur))

MigrationDuration<-paste(round(MigDuration,1), " [",round(MigDurationMin,1), ", ",round(MigDurationMax,1),"]", sep="")

MigDistance<-c(median(BRN$springMigDst),
               median(Buch$springMigDst),
               median(Grey$springMigDst),
               median(Lapo$springMigDst),
               median(MidR$springMigDst),
               median(Tops$springMigDst))/1000

MigDistanceMin<-c(min(BRN$springMigDst),
                  min(Buch$springMigDst),
                  min(Grey$springMigDst),
                  min(Lapo$springMigDst),
                  min(MidR$springMigDst),
                  min(Tops$springMigDst))/1000

MigDistanceMax<-c(max(BRN$springMigDst),
                  max(Buch$springMigDst),
                  max(Grey$springMigDst),
                  max(Lapo$springMigDst),
                  max(MidR$springMigDst),
                  max(Tops$springMigDst))/1000

MigrationDistance<-paste(round(MigDistance,1), " [",round(MigDistanceMin,1), ", ",round(MigDistanceMax,1),"]", sep="")

ParturitionMed<-c(
  median(BRN$CalfDateHybrid_man),
  median(Buch$CalfDateHybrid_man),
  median(Grey$CalfDateHybrid_man),
  median(Lapo$CalfDateHybrid_man),
  median(MidR$CalfDateHybrid_man),
  median(Tops$CalfDateHybrid_man))

ParturitionMin<-c(
  min(BRN$CalfDateHybrid_man),
  min(Buch$CalfDateHybrid_man),
  min(Grey$CalfDateHybrid_man),
  min(Lapo$CalfDateHybrid_man),
  min(MidR$CalfDateHybrid_man),
  min(Tops$CalfDateHybrid_man))

ParturitionMax<-c(
  max(BRN$CalfDateHybrid_man),
  max(Buch$CalfDateHybrid_man),
  max(Grey$CalfDateHybrid_man),
  max(Lapo$CalfDateHybrid_man),
  max(MidR$CalfDateHybrid_man),
  max(Tops$CalfDateHybrid_man))

CalvingMform<-format(as.Date(ParturitionMed,origin = "1899-12-31"), "%b %d")
Calvingminform<-format(as.Date(ParturitionMin,origin = "1899-12-31"), "%b %d")
Calvingmaxform<-format(as.Date(ParturitionMax,origin = "1899-12-31"), "%b %d")

CalvingRange<-paste(CalvingMform, " [",Calvingminform, ", ",Calvingmaxform,"]", sep="")

Survival<-round(c(mean(BRN$surv),
                  mean(Buch$surv),
                  mean(Grey$surv),
                  mean(Lapo$surv),
                  mean(MidR$surv),
                  mean(Tops$surv)),2)

#### Snow off dates

SnowOff<-readRDS("Output/Phenology/SnowOffDates.RDS")

SnowOffMed<-trunc(c(
  sum(SnowOff[50,])/ncol(SnowOff),
  sum(SnowOff[50,1:7])/7,
  sum(SnowOff[50,8:14])/7,
  sum(SnowOff[50,15:21])/7,
  sum(SnowOff[50,22:28])/7,
  sum(SnowOff[50,29:35])/7
))

SnowOff5p<-trunc(c(
  sum(SnowOff[5,])/ncol(SnowOff),
  sum(SnowOff[5,1:7])/7,
  sum(SnowOff[5,8:14])/7,
  sum(SnowOff[5,15:21])/7,
  sum(SnowOff[5,22:28])/7,
  sum(SnowOff[5,29:35])/7
))


SnowOff95p<-trunc(c(
  sum(SnowOff[95,])/ncol(SnowOff),
  sum(SnowOff[95,1:7])/7,
  sum(SnowOff[95,8:14])/7,
  sum(SnowOff[95,15:21])/7,
  sum(SnowOff[95,22:28])/7,
  sum(SnowOff[95,29:35])/7
))

SnowMedForm<-format(as.Date(SnowOffMed,origin = "1899-12-31"), "%b %d")
SnowMinForm<-format(as.Date(SnowOff5p,origin = "1899-12-31"), "%b %d")
SnowMaxForm<-format(as.Date(SnowOff95p,origin = "1899-12-31"), "%b %d")

SnowRange<-paste(SnowMedForm, " [",SnowMinForm, ", ",SnowMaxForm,"]", sep="")

#### Green up dates

GreenUp<-readRDS("Output/Phenology/GreenUpDates.RDS")

GreenUpMed<-trunc(c(
  sum(GreenUp[50,])/ncol(GreenUp),
  sum(GreenUp[50,1:7])/7,
  sum(GreenUp[50,8:14])/7,
  sum(GreenUp[50,15:21])/7,
  sum(GreenUp[50,22:28])/7,
  sum(GreenUp[50,29:35])/7
))

GreenUp5p<-trunc(c(
  sum(GreenUp[5,])/ncol(GreenUp),
  sum(GreenUp[5,1:7])/7,
  sum(GreenUp[5,8:14])/7,
  sum(GreenUp[5,15:21])/7,
  sum(GreenUp[5,22:28])/7,
  sum(GreenUp[5,29:35])/7
))


GreenUp95p<-trunc(c(
  sum(GreenUp[95,])/ncol(GreenUp),
  sum(GreenUp[95,1:7])/7,
  sum(GreenUp[95,8:14])/7,
  sum(GreenUp[95,15:21])/7,
  sum(GreenUp[95,22:28])/7,
  sum(GreenUp[95,29:35])/7
))

GreenMedForm<-format(as.Date(GreenUpMed,origin = "1899-12-31"), "%b %d")
GreenMinForm<-format(as.Date(GreenUp5p,origin = "1899-12-31"), "%b %d")
GreenMaxForm<-format(as.Date(GreenUp95p,origin = "1899-12-31"), "%b %d")

GreenRange<-paste(GreenMedForm, " [",GreenMinForm, ", ",GreenMaxForm,"]", sep="")

SumTable<-t(data.frame(NumIDs,NumIDYears,MigrationStart,MigrationEnd,
                       MigrationDuration,MigrationDistance,CalvingRange,Survival,
                       SnowRange,GreenRange))

colnames(SumTable)<-c("All Herds","Buchans","Grey River","Lapoile","Middle Ridge","Topsails")
rownames(SumTable)<-c("Number of individuals","Number of ID years","median + [range], start date of migration",
                      "median + [range], end date of migration","median + [range], duration of migration (days)",
                      "median + [range], distance of migration (km)",
                      "median + [range], date of calving", "Proportion calves survive",
                      "Median + [5th and 95th percentile] date of snow melt",
                      "Median + [5th and 95th percentile] date of green-up")


SumTable

write.csv(SumTable, "Output/Tables/SummaryTable.csv")


##################################### END #######################################
