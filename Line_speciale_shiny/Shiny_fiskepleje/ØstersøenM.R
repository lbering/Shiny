###########################################
############------østersøen------############
###########################################
library(mgcv)
library(gratia)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gdata)
library(lubridate)

line<-TRUE
#-------load dat-----------#
if(line){
  setwd("~/Documents/DTU/Speciale/R/GAM")
}else{
  setwd("GAM")
}
source("funcs.R")

periods <- seq(2000,2024,6)

#------------Make as factors----------------_#
dat_cod <- read_csv("dat_cod.csv")
dat_cod$loc<-as.factor(dat_cod$loc)
dat_cod$Gear<-as.factor(dat_cod$Gear)
dat_cod$Quarter<-as.factor(dat_cod$Quarter)
dat_cod$SedimentDK<-as.factor(dat_cod$SedimentDK) 
dat_cod <- check.data(dat_cod, periods = periods)

dat_flounder <- read_csv("dat_flounder.csv")
dat_flounder$loc<-as.factor(dat_flounder$loc)
dat_flounder$Gear<-as.factor(dat_flounder$Gear)
dat_flounder$Quarter<-as.factor(dat_flounder$Quarter)
dat_flounder$SedimentDK<-as.factor(dat_flounder$SedimentDK)
dat_flounder <- check.data(dat_flounder, periods = periods)

dat_plaice <- read_csv("dat_plaice.csv")
dat_plaice$loc<-as.factor(dat_plaice$loc)
dat_plaice$Gear<-as.factor(dat_plaice$Gear)
dat_plaice$Quarter<-as.factor(dat_plaice$Quarter)
dat_plaice$SedimentDK<-as.factor(dat_plaice$SedimentDK)
dat_plaice <- check.data(dat_plaice, periods = periods)

dat_herring<- read_csv("dat_herring.csv")
dat_herring$loc<-as.factor(dat_herring$loc)
dat_herring$Gear<-as.factor(dat_herring$Gear)
dat_herring$Quarter<-as.factor(dat_herring$Quarter)
dat_herring$SedimentDK<-as.factor(dat_herring$SedimentDK)
dat_herring <- check.data(dat_herring, periods = periods)

dat_sprat<- read_csv("dat_sprat.csv")
dat_sprat$loc<-as.factor(dat_sprat$loc)
dat_sprat$Gear<-as.factor(dat_sprat$Gear)
dat_sprat$Quarter<-as.factor(dat_sprat$Quarter)
dat_sprat$SedimentDK<-as.factor(dat_sprat$SedimentDK)
dat_sprat <- check.data(dat_sprat, periods = periods)

dat_whiting<- read_csv("dat_whiting.csv")
dat_whiting$loc<-as.factor(dat_whiting$loc)
dat_whiting$Gear<-as.factor(dat_whiting$Gear)
dat_whiting$Quarter<-as.factor(dat_whiting$Quarter)
dat_whiting$SedimentDK<-as.factor(dat_whiting$SedimentDK)
dat_whiting <- check.data(dat_whiting, periods = periods)

##########################################
################ MODELS ##################
##########################################

#--------COD -------#
mod.codM <- gam(cpue ~ #response
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                  data = subset(dat_cod, loc == "østersøen_codM"),
                  family=tw) 
mod.codM <- get.mini.model(mod.codM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------FLOUNDER -------#
mod.flounderM<- gam(cpue ~ 
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                  data = subset(dat_flounder, loc == "østersøen_flounderM"), 
                  family=tw) 
mod.flounderM <- get.mini.model(mod.flounderM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
 
#--------PLAICE -------#
mod.plaiceM <- gam(cpue ~
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                  data = subset(dat_plaice, loc == "østersøen_plaiceM"), 
                  family=tw) 
mod.plaiceM <- get.mini.model(mod.plaiceM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------HERRING -------#
mod.herringM <- gam(cpue ~ 
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter+
                  SedimentDK, 
                data = subset(dat_herring, loc == "østersøen_herringM"),
                family=tw) 
mod.herringM <- get.mini.model(mod.herringM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------SPRAT -------#
mod.spratM <- gam(cpue ~ 
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                  data = subset(dat_sprat, loc == "østersøen_spratM"), 
                  family=tw) 
mod.spratM <- get.mini.model(mod.spratM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------WHITING -------#
mod.whitingM <- gam(cpue ~ 
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+
                  Quarter+ 
                  SedimentDK,
                data = subset(dat_whiting, loc == "østersøen_whitingM"), 
                family=tw) 
mod.whitingM <- get.mini.model(mod.whitingM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

############################### Predict CPUE ############################### 
## Assuming one overall depth for all locations is one approach, alternatively, one could use the median depth for each location

newdat <- expand.grid(period = levels(dat_cod$period),
                      loc = c("østersøen_codM","østersøen_flounderM",
                              "østersøen_plaiceM","østersøen_herringM",
                              "østersøen_spratM","østersøen_whitingM"),
                      depth = 19, #østersøen is 19
                      Gear=c("TVS"),
                      Quarter=c("4"),
                      SedimentDK=c("5")
)
#to check the depth of each location: 
#newdat$depth <- depth.by.loc[match(newdat$loc,names(depth.by.loc))] #overwrite for each location
## individual models
tmp1 <- predict(mod.codM, newdata = newdat, type = "response", se.fit = TRUE)
tmp2 <- predict(mod.flounderM, newdata = newdat, type = "response", se.fit = TRUE)
tmp3 <- predict(mod.plaiceM, newdata = newdat, type = "response", se.fit = TRUE)
tmp4 <- predict(mod.herringM, newdata = newdat, type = "response", se.fit = TRUE)
tmp5 <- predict(mod.spratM, newdata = newdat, type = "response", se.fit = TRUE)
tmp6 <- predict(mod.whitingM, newdata = newdat, type = "response", se.fit = TRUE)

#make new data predictions
newdat$cpue_pred <- NA
newdat$cpue_pred[newdat$loc == "østersøen_codM"] <- tmp1$fit[newdat$loc == "østersøen_codM"]
newdat$cpue_pred[newdat$loc == "østersøen_flounderM"] <- tmp2$fit[newdat$loc == "østersøen_flounderM"]
newdat$cpue_pred[newdat$loc == "østersøen_plaiceM"] <- tmp3$fit[newdat$loc == "østersøen_plaiceM"]
newdat$cpue_pred[newdat$loc == "østersøen_herringM"] <- tmp4$fit[newdat$loc == "østersøen_herringM"]
newdat$cpue_pred[newdat$loc == "østersøen_spratM"] <- tmp5$fit[newdat$loc == "østersøen_spratM"]
newdat$cpue_pred[newdat$loc == "østersøen_whitingM"] <- tmp6$fit[newdat$loc == "østersøen_whitingM"]

newdat$cpue_sd <- NA
newdat$cpue_sd[newdat$loc == "østersøen_codM"] <- tmp1$se.fit[newdat$loc == "østersøen_codM"]
newdat$cpue_sd[newdat$loc == "østersøen_flounderM"] <- tmp2$se.fit[newdat$loc == "østersøen_flounderM"]
newdat$cpue_sd[newdat$loc == "østersøen_plaiceM"] <- tmp3$se.fit[newdat$loc == "østersøen_plaiceM"]
newdat$cpue_sd[newdat$loc == "østersøen_herringM"] <- tmp4$se.fit[newdat$loc == "østersøen_herringM"]
newdat$cpue_sd[newdat$loc == "østersøen_spratM"] <- tmp5$se.fit[newdat$loc == "østersøen_spratM"]
newdat$cpue_sd[newdat$loc == "østersøen_whitingM"] <- tmp6$se.fit[newdat$loc == "østersøen_whitingM"]


newdat$cpue_lo <- newdat$cpue_pred - 1.96 * newdat$cpue_sd
newdat$cpue_up <- newdat$cpue_pred + 1.96 * newdat$cpue_sd

## individual models
tmp1.log <- predict(mod.codM, newdata = newdat, type = "link", se.fit = TRUE)
tmp2.log <- predict(mod.flounderM, newdata = newdat, type = "link", se.fit = TRUE)
tmp3.log <- predict(mod.plaiceM, newdata = newdat, type = "link", se.fit = TRUE)
tmp4.log <- predict(mod.herringM, newdata = newdat, type = "link", se.fit = TRUE)
tmp5.log <- predict(mod.spratM, newdata = newdat, type = "link", se.fit = TRUE)
tmp6.log <- predict(mod.whitingM, newdata = newdat, type = "link", se.fit = TRUE)

## for error propagations
newdat$cpue_pred.log <- NA
newdat$cpue_pred.log[newdat$loc == "østersøen_codM"] <- tmp1.log$fit[newdat$loc == "østersøen_codM"]
newdat$cpue_pred.log[newdat$loc == "østersøen_flounderM"] <- tmp2.log$fit[newdat$loc == "østersøen_flounderM"]
newdat$cpue_pred.log[newdat$loc == "østersøen_plaiceM"] <- tmp3.log$fit[newdat$loc == "østersøen_plaiceM"]
newdat$cpue_pred.log[newdat$loc == "østersøen_herringM"] <- tmp4.log$fit[newdat$loc == "østersøen_herringM"]
newdat$cpue_pred.log[newdat$loc == "østersøen_spratM"] <- tmp5.log$fit[newdat$loc == "østersøen_spratM"]
newdat$cpue_pred.log[newdat$loc == "østersøen_whitingM"] <- tmp6.log$fit[newdat$loc == "østersøen_whitingM"]
newdat$cpue_sd.log <- NA
newdat$cpue_sd.log[newdat$loc == "østersøen_codM"] <- tmp1.log$se.fit[newdat$loc == "østersøen_codM"]
newdat$cpue_sd.log[newdat$loc == "østersøen_flounderM"] <- tmp2.log$se.fit[newdat$loc == "østersøen_flounderM"]
newdat$cpue_sd.log[newdat$loc == "østersøen_plaiceM"] <- tmp3.log$se.fit[newdat$loc == "østersøen_plaiceM"]
newdat$cpue_sd.log[newdat$loc == "østersøen_herringM"] <- tmp4.log$se.fit[newdat$loc == "østersøen_herringM"]
newdat$cpue_sd.log[newdat$loc == "østersøen_spratM"] <- tmp5.log$se.fit[newdat$loc == "østersøen_spratM"]
newdat$cpue_sd.log[newdat$loc == "østersøen_whitingM"] <- tmp6.log$se.fit[newdat$loc == "østersøen_whitingM"]

if(line){
  write_rds(newdat,"~/Documents/DTU/Speciale/R/shiny/Shiny_fiskepleje/newdat/newdat_østersøenM.rds") 
}else{
  write_rds (newdat, "../newdat_østerøsenJ.rds")
}

#ind.per1<-1
#ind.per2<-4
ind.per1 <- as.numeric(newdat$period) == 1
ind.per2<-as.numeric(newdat$period) == 4
#if(input$periode == "2000-2006"){
#                                             period=1
#                                           }else if(input$periode=="2006-2012"){
#                                               period=2
#                                           }else if(input$periode=="2012-2018"){
#                                               period=3} 
#ind.per2 <- as.numeric(newdat$period) == 4

z.score <- qnorm(0.95 + (1 - 0.95)/2)

## Select location (put in loop later)
lows <- ups <- pcs <- sds <- pvals <- rep(NA, length(levels(newdat$loc)))
for(i in 1:length(levels(newdat$loc))){
  ind.loc <- newdat$loc == levels(newdat$loc)[i]
  ## Ratio
  m.per1 <- newdat$cpue_pred[ind.per1 & ind.loc]
  m.per2 <- newdat$cpue_pred[ind.per2 & ind.loc]
  s.per1 <- newdat$cpue_sd[ind.per1 & ind.loc]
  s.per2 <- newdat$cpue_sd[ind.per2 & ind.loc]
  pcs[i] <- (m.per2/m.per1 - 1) * 100
  sds[i] <- abs(pcs[i]) * sqrt((s.per1/m.per1)^2 + (s.per2/m.per2)^2)
  lows[i] <- pcs[i] - z.score * sds[i]
  ups[i] <- pcs[i] + z.score * sds[i]
}

plot(1,1, ty = "n",
     xlim = c(-1e3, 1e3),
     ylim = c(0,7),
     xlab = "", ylab = "",
     yaxt = "n",
     frame.plot=FALSE#TRUE
)

abline(v = 0, col = "grey40", lty = 2, lwd = 1.5)
abline(v = c(seq(-9e2,-1e2,1e2),
             seq(1e2,1e3,1e2)), col = "grey40", lty = 3, lwd = 1)
mtext("Percentage change [%]",1,3)
for(i in 1:length(pcs)){
  ##      if (sds[i]<1.75){
  if(pcs[i]>0 && lows[i]>0 && ups[i]>0){
    coli <- "#008835"
  }else if(pcs[i]<0 && lows[i]<0 && ups[i]<0){
    coli <- "#E83F48"
  }else{
    coli <- "grey30"
  }
  suppressWarnings({
    arrows(lows[i], i,
           ups[i], i,
           code = 3, angle = 90, length = 0.1,
           col = coli,
           lwd = 2)
  })
  points(pcs[i], i,
         col = coli,
         lwd = 2,
         pch = 16, cex = 1.3)
  # text(rats.log[i]+1,cex=0.7,c("hej","2","3","4","5","6"))
  ## text(pcs[i]+ z.score * sds[i] + afstand[i],i,
  ##      lwd = 2,
  ##      pch = 16, cex = 1.3, species[i])
  text(-1e3,i,
       lwd = 2,
       pch = 16, cex = 1.3, species[i])
}
