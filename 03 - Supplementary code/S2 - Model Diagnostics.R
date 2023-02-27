############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn MR Webber, E Vander Wal ####################################################

############# Script S2: Model diagnostics #################################################################

############################################################################################################
############################################################################################################

#### Model diagnostics


### Load in the data
BRN<-readRDS("Output/DataForBRN.RDS")

libs <- c('data.table','dplyr', 'MCMCglmm', 
          'ggplot2' ,'coda', 'tidybayes',
          'emmeans', 'standardize')
lapply(libs, require, character.only = TRUE)

tab<-table(BRN$ID)
BRN <- droplevels(BRN[BRN$ID %in% names(tab)[tab >= 3],,drop=FALSE])

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

## Scaling: variables scaled "by herd"
BRN$SnowOff<-scale(BRN$SnowOff)
BRN$GreenUp<-scale(BRN$GreenUp)
BRN$MigEnd<-scale(BRN$MigEnd)
BRN$Parturitionus<-BRN$CalfDateHybrid
BRN$Parturition<-scale(BRN$CalfDateHybrid)


#### Migration + Parturition

mcmcMigPart<-readRDS("Output/BRN_models/mcmc8MigPart.RDS")

plot(mcmcMigPart)

df <- mcmcMigPart %>%
  emmeans(~ Population, data = BRN) %>%
  gather_emmeans_draws() %>%
  median_qi()


## Plot the posterior distributions of the 2 response variables from the bivariate model
ggplot(df, aes(.value, Population)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = .lower, 
                     xmax = .upper, 
                     height = 0),
                 position = position_dodge(width = 0.4)) +
  ylab("") +
  xlab("Posterior distribution") +
  theme_bw() +
  facet_wrap(~trait)


pdf("Results/Diagnostics/mcmcPlotsMigPart.pdf",width=10,height=10)
plot(mcmcMigPart)
dev.off()


## Heidel test
mcmcMigPart_heidel <- heidel.diag(mcmcMigPart$Sol, eps = 0.1, pvalue = 0.05)
mcmcMigPart_heidel2 <- data.table(mcmcMigPart_heidel[1:134,])

par(mfrow=c(1,1))
hist(mcmcMigPart_heidel2$halfwidth)


autocorr.diag(mcmcMigPart$VCV)

##### Gelman:

iter<-4200000
burn<-200000
th<-200

prior3 <- list(R=list(V=diag(10), nu=4),
               G=list(G1=list(V=diag(4), nu=4,
                              alpha.V=diag(var(BRN$MigEndus),4,4))))

mcmcMigPartGel1 <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                         rcov =~ idh(trait:Population):units,
                         random =~ us(trait + SnowOff:trait):ID:Population,
                         family = c("gaussian","gaussian"),
                         prior = prior3,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE,
                         start=list(QUASI=FALSE))

mcmcMigPartGel2 <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                            rcov =~ idh(trait:Population):units,
                            random =~ us(trait + SnowOff:trait):ID:Population,
                            family = c("gaussian","gaussian"),
                            prior = prior3,
                            nitt=iter,
                            burnin=burn,
                            thin=th,
                            verbose = TRUE,
                            data = BRN,
                            pr=T,saveX = TRUE,saveZ = TRUE,
                            start=list(QUASI=FALSE))

mcmcMigPartGel3 <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                            rcov =~ idh(trait:Population):units,
                            random =~ us(trait + SnowOff:trait):ID:Population,
                            family = c("gaussian","gaussian"),
                            prior = prior3,
                            nitt=iter,
                            burnin=burn,
                            thin=th,
                            verbose = TRUE,
                            data = BRN,
                            pr=T,saveX = TRUE,saveZ = TRUE,
                            start=list(QUASI=FALSE))

mcmcMigPartGel4 <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                            rcov =~ idh(trait:Population):units,
                            random =~ us(trait + SnowOff:trait):ID:Population,
                            family = c("gaussian","gaussian"),
                            prior = prior3,
                            nitt=iter,
                            burnin=burn,
                            thin=th,
                            verbose = TRUE,
                            data = BRN,
                            pr=T,saveX = TRUE,saveZ = TRUE,
                            start=list(QUASI=FALSE))

mcmcMigPartGel5 <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:SnowOff,
                            rcov =~ idh(trait:Population):units,
                            random =~ us(trait + SnowOff:trait):ID:Population,
                            family = c("gaussian","gaussian"),
                            prior = prior3,
                            nitt=iter,
                            burnin=burn,
                            thin=th,
                            verbose = TRUE,
                            data = BRN,
                            pr=T,saveX = TRUE,saveZ = TRUE,
                            start=list(QUASI=FALSE))

mh.list<-list(mcmcMigPartGel1$Sol,mcmcMigPartGel2$Sol,mcmcMigPartGel3$Sol,mcmcMigPartGel4$Sol,mcmcMigPartGel5$Sol)

saveRDS(mh.list,"Results/Diagnostics/MigPartGel.RDS")


GR.MigPart<-readRDS("Results/Diagnostics/MigPartGel.RDS")
gelman.diag(GR.MigPart)[1]


max(GR.MigPart)
pdf("Results/Diagnostics/gelmanMigPart.pdf", width=7,height=7)
gelman.plot(GR.MigPart)
dev.off()

#### Parturition + Survival

mcmcPartSurv<-readRDS("Output/BRN_models/mcmc08PartSurv.RDS")

df <- mcmcPartSurv %>%
  emmeans(~ Population, data = BRN) %>%
  gather_emmeans_draws() %>%
  median_qi()


## Plot the posterior distributions of the 2 response variables from the bivariate model
ggplot(df, aes(.value, Population)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = .lower, 
                     xmax = .upper, 
                     height = 0),
                 position = position_dodge(width = 0.4)) +
  ylab("") +
  xlab("Posterior distribution") +
  theme_bw() +
  facet_wrap(~trait)



pdf("Results/Diagnostics/mcmcPlotsPartSurv.pdf",width=10,height=10)
plot(mcmcPartSurv)
dev.off()


## Heidel test
mcmcPartSurv_heidel <- heidel.diag(mcmcPartSurv$Sol, eps = 0.1, pvalue = 0.05)
mcmcPartSurv_heidel

mcmcPartSurv_heidel2 <- data.table(mcmcPartSurv_heidel[1:132,])

par(mfrow=c(1,1))
hist(mcmcPartSurv_heidel2$halfwidth)

## autocorrelation -- we basically want these numbesr to be <0.1
autocorr.diag(mcmcPartSurv$VCV)

## Gelman 

prior3 <- list(R=list(V=diag(10), nu=4),
               G=list(G1=list(V=diag(4), nu=4,
                              alpha.V=diag(var(BRN$Parturitionus),4,4))))


mcmcPartSurvGel1 <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                          rcov =~ idh(trait:Population):units,
                          random =~ us(trait + GreenUp:trait):ID:Population,
                          family = c("gaussian","categorical"),
                          prior = prior3,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE,
                          start=list(QUASI=FALSE))

mcmcPartSurvGel2 <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                             rcov =~ idh(trait:Population):units,
                             random =~ us(trait + GreenUp:trait):ID:Population,
                             family = c("gaussian","categorical"),
                             prior = prior3,
                             nitt=iter,
                             burnin=burn,
                             thin=th,
                             verbose = TRUE,
                             data = BRN,
                             pr=T,saveX = TRUE,saveZ = TRUE,
                             start=list(QUASI=FALSE))

mcmcPartSurvGel3 <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                             rcov =~ idh(trait:Population):units,
                             random =~ us(trait + GreenUp:trait):ID:Population,
                             family = c("gaussian","categorical"),
                             prior = prior3,
                             nitt=iter,
                             burnin=burn,
                             thin=th,
                             verbose = TRUE,
                             data = BRN,
                             pr=T,saveX = TRUE,saveZ = TRUE,
                             start=list(QUASI=FALSE))

mcmcPartSurvGel4 <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                             rcov =~ idh(trait:Population):units,
                             random =~ us(trait + GreenUp:trait):ID:Population,
                             family = c("gaussian","categorical"),
                             prior = prior3,
                             nitt=iter,
                             burnin=burn,
                             thin=th,
                             verbose = TRUE,
                             data = BRN,
                             pr=T,saveX = TRUE,saveZ = TRUE,
                             start=list(QUASI=FALSE))

mcmcPartSurvGel5 <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:GreenUp,
                             rcov =~ idh(trait:Population):units,
                             random =~ us(trait + GreenUp:trait):ID:Population,
                             family = c("gaussian","categorical"),
                             prior = prior3,
                             nitt=iter,
                             burnin=burn,
                             thin=th,
                             verbose = TRUE,
                             data = BRN,
                             pr=T,saveX = TRUE,saveZ = TRUE,
                             start=list(QUASI=FALSE))



mh.list<-list(mcmcPartSurvGel1$Sol,mcmcPartSurvGel2$Sol,mcmcPartSurvGel3$Sol,mcmcPartSurvGel4$Sol,mcmcPartSurvGel5$Sol)

saveRDS(mh.list,"Results/Diagnostics/PartSurvGel.RDS")
PartSurvGel<-readRDS("Results/Diagnostics/PartSurvGel.RDS")

gelman.diag(PartSurvGel)[1]
pdf("Results/Diagnostics/gelmanPartSurv.pdf", width=7,height=7)
gelman.plot(mh.list)
dev.off()

##################################### END #######################################
