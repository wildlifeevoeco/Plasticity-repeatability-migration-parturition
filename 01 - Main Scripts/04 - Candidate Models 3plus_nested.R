############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, QMR Webber, E Vander Wal #########################################################

############# Script 4: Making BRNs part 1: candidate models ###############################################

############################################################################################################
############################################################################################################


### Packages ----
libs <- c('data.table', 
          'lme4','MCMCglmm',
          'tidyr', 'sqldf', 'gridExtra',
          'ggplot2', 'lmtest','dplyr','standardize')
lapply(libs, require, character.only = TRUE)

### Load in the data

BRN<-readRDS("Output/DataForBRN.RDS")

## Dropping ID_years QW and I didn't agree on - run to fit supplement #3:
#drops<-readRDS("Output/ToDrop.RDS")
#BRN<-subset(BRN,!(IDyear %in% drops))

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

length(unique(BRN$ID))
### 32 individuals with 3+ years


## Some renaming to match analyses
colnames(BRN)[6]<-"MigStart"
colnames(BRN)[7]<-"MigEnd"
colnames(BRN)[9]<-"Surv"
colnames(BRN)[12]<-"SnowOff"
colnames(BRN)[13]<-"GreenUp"
colnames(BRN)[3]<-"Population"

## Add unscaled versions before overwriting with scaled versions
BRN$MigEndus<-BRN$MigEnd
BRN$MigStartus<-BRN$MigStart

## Different parturition types here:
BRN$Parturitionus<-BRN$CalfDateHybrid_man

###### Scaling

BRN$SnowOff<-scale(BRN$SnowOff)
BRN$GreenUp<-scale(BRN$GreenUp)

BRN$MigEnd<-scale(BRN$MigEnd)
BRN$Parturition<-scale(BRN$CalfDateHybrid_man)


## Sample size:

length(unique(BRN$ID))  ## 32 animals
nrow(BRN)  ## 132 animal-years


##### Testing whether older animals give birth earlier


head(BRN)

BRN<-BRN[order(BRN$IDyear),]
BRN
table(BRN$ID)

BRN$minYear<-ave(as.integer(as.character(BRN$year)),BRN$ID,FUN=function(x){min(x)})
BRN$sampYear<-as.integer(as.character(BRN$year))-(BRN$minYear-1)

sampMod<-glm(BRN$Parturitionus~BRN$sampYear)

summary(sampMod)

sampMod2<-glm(BRN$Parturitionus~BRN$sampYear+BRN$SnowOff+BRN$GreenUp)

summary(sampMod2)


## Set the parameters for the BRNs

iter<-4200000
burn<-200000
th<-200


### Model 1: Null

prior1 <- list(R=list(V=diag(10), nu=3))  

prior2 <- list(R=list(V=diag(10), nu=3),
               G=list(G1=list(V=diag(2), nu=3,
                              alpha.V=diag(var(BRN$MigEndus),2,2))))

prior3 <- list(R=list(V=diag(10), nu=4),
               G=list(G1=list(V=diag(4), nu=4,
                              alpha.V=diag(var(BRN$MigEndus),4,4))))

mcmc01MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","gaussian"),
                          prior = prior1,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 2: Herd differences, no plasticity

mcmc02MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1,
                          random =~ idh(trait):Population,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","gaussian"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 3: Individual differences, no plasticity

mcmc03MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1,
                          random =~ idh(trait):ID:Population,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","gaussian"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)


######## Model 4: Plasticity, no differences

mcmc04MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","gaussian"),
                          prior = prior1,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)


######## Model 5: Plasticity, population differences

mcmc05MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                          random =~ idh(trait):Population,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","gaussian"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 6: Plasticity and herd X Environment interaction

mcmc06MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                          random =~ us(trait + SnowOff:trait):Population,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","gaussian"),
                          prior = prior3,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 7: Plasticity and individual differences

mcmc07MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                          random =~ idh(trait):ID:Population,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","gaussian"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 8: Individual X Environment interaction

mcmc08MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                          rcov =~ idh(trait:Population):units,
                          random =~ us(trait + SnowOff:trait):ID:Population,
                          family = c("gaussian","gaussian"),
                          prior = prior3,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)


dDICsMigPart<-c(mcmc01MigPart$DIC,mcmc02MigPart$DIC,mcmc03MigPart$DIC,mcmc04MigPart$DIC,
                mcmc05MigPart$DIC,mcmc06MigPart$DIC,mcmc07MigPart$DIC,mcmc08MigPart$DIC)-
            min(mcmc01MigPart$DIC,mcmc02MigPart$DIC,mcmc03MigPart$DIC,mcmc04MigPart$DIC,
                mcmc05MigPart$DIC,mcmc06MigPart$DIC,mcmc07MigPart$DIC,mcmc08MigPart$DIC)

dDICsMigPart


saveRDS(mcmc01MigPart, "Output/BRN_models/mcmc1MigPart.RDS")
saveRDS(mcmc02MigPart, "Output/BRN_models/mcmc2MigPart.RDS")
saveRDS(mcmc03MigPart, "Output/BRN_models/mcmc3MigPart.RDS")
saveRDS(mcmc04MigPart, "Output/BRN_models/mcmc4MigPart.RDS")
saveRDS(mcmc05MigPart, "Output/BRN_models/mcmc5MigPart.RDS")
saveRDS(mcmc06MigPart, "Output/BRN_models/mcmc6MigPart.RDS")
saveRDS(mcmc07MigPart, "Output/BRN_models/mcmc7MigPart.RDS")
saveRDS(mcmc08MigPart, "Output/BRN_models/mcmc8MigPart.RDS")

summary(mcmc08MigPart)

#### Part 2: Parturition and survival


### Model 1: Null

prior1 <- list(R=list(V=diag(10), nu=3))  

prior2 <- list(R=list(V=diag(10), nu=3),
               G=list(G1=list(V=diag(2), nu=3,
                              alpha.V=diag(var(BRN$Parturitionus),2,2))))

prior3 <- list(R=list(V=diag(10), nu=4),
               G=list(G1=list(V=diag(4), nu=4,
                              alpha.V=diag(var(BRN$Parturitionus),4,4))))

mcmc01PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1,
                           rcov =~ idh(trait:Population):units,
                           family = c("gaussian","categorical"),
                           prior = prior1,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 2: Herd differences, no plasticity

mcmc02PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1,
                           random =~ idh(trait):Population,
                           rcov =~ idh(trait:Population):units,
                           family = c("gaussian","categorical"),
                           prior = prior2,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 3: Individual differences, no plasticity

mcmc03PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1,
                           random =~ idh(trait):ID:Population,
                           rcov =~ idh(trait:Population):units,
                           family = c("gaussian","categorical"),
                           prior = prior2,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)


######## Model 4: Plasticity, no differences

mcmc04PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                           rcov =~ idh(trait:Population):units,
                           family = c("gaussian","categorical"),
                           prior = prior1,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)


######## Model 5: Plasticity, population differences

mcmc05PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                           random =~ idh(trait):Population,
                           rcov =~ idh(trait:Population):units,
                           family = c("gaussian","categorical"),
                           prior = prior2,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 6: Plasticity and herd X Environment interaction

mcmc06PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                           random =~ us(trait + GreenUp:trait):Population,
                           rcov =~ idh(trait:Population):units,
                           family = c("gaussian","categorical"),
                           prior = prior3,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 7: Plasticity and individual differences

mcmc07PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                           random =~ idh(trait):ID:Population,
                           rcov =~ idh(trait:Population):units,
                           family = c("gaussian","categorical"),
                           prior = prior2,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)

######## Model 8: Individual X Environment interaction

mcmc08PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                           rcov =~ idh(trait:Population):units,
                           random =~ us(trait + GreenUp:trait):ID:Population,
                           family = c("gaussian","categorical"),
                           prior = prior3,
                           nitt=iter,
                           burnin=burn,
                           thin=th,
                           verbose = TRUE,
                           data = BRN,
                           pr=T,saveX = TRUE,saveZ = TRUE)


dDICsPartSurv<-c(mcmc01PartSurv$DIC,mcmc02PartSurv$DIC,mcmc03PartSurv$DIC,mcmc04PartSurv$DIC,
                 mcmc05PartSurv$DIC,mcmc06PartSurv$DIC,mcmc07PartSurv$DIC,mcmc08PartSurv$DIC)-
             min(mcmc01PartSurv$DIC,mcmc02PartSurv$DIC,mcmc03PartSurv$DIC,mcmc04PartSurv$DIC,
                 mcmc05PartSurv$DIC,mcmc06PartSurv$DIC,mcmc07PartSurv$DIC,mcmc08PartSurv$DIC)

dDICsPartSurv


saveRDS(mcmc01PartSurv, "Output/BRN_models/mcmc01PartSurv.RDS")
saveRDS(mcmc02PartSurv, "Output/BRN_models/mcmc02PartSurv.RDS")
saveRDS(mcmc03PartSurv, "Output/BRN_models/mcmc03PartSurv.RDS")
saveRDS(mcmc04PartSurv, "Output/BRN_models/mcmc04PartSurv.RDS")
saveRDS(mcmc05PartSurv, "Output/BRN_models/mcmc05PartSurv.RDS")
saveRDS(mcmc06PartSurv, "Output/BRN_models/mcmc06PartSurv.RDS")
saveRDS(mcmc07PartSurv, "Output/BRN_models/mcmc07PartSurv.RDS")
saveRDS(mcmc08PartSurv, "Output/BRN_models/mcmc08PartSurv.RDS")


summary(mcmc08PartSurv)

descriptions<-c("Null",
                "Population differences, no plasticity",
                "Individual differences, no plasticity",
                "Overall plasticity, no population/individual differences",
                "Overall plasticity, population differences",
                "Random slopes, population X environment interaction",
                "Overall plasticity, individual differences",
                "Random slopes, individual X environment interaction")
fixed<-c("-",
         "-",
         "-",
         "Timing of spring (snow melt or green-up)",
         "Timing of spring (snow melt or green-up)",
         "Timing of spring (snow melt or green-up)",
         "Timing of spring (snow melt or green-up)",
         "Timing of spring (snow melt or green-up)")
random<-c("None",
          "Population",
          "Individual|Population",
          "None",
          "Population",
          "Individual|Population",
          "Population X Timing of spring",
          "Individual|Population X Timing of spring")

summaryData<-data.frame(descriptions,fixed,random,round(dDICsMigPart,1),round(dDICsPartSurv,1))

colnames(summaryData)<-c("Model","Fixed effects","Random effects",
                         "dDIC, migration and parturition","dDIC, parturition and survival")

summaryData

write.csv(summaryData, "Output/BRN_models/BirthDate/dDICs.csv")


######################## END ##################################################
