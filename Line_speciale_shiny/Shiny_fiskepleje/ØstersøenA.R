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
mod.codA <- gam(cpue ~ 
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_cod, loc == "østersøen_codA"), 
                family=nb) 
mod.codA <- get.mini.model(mod.codA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------FLOUNDER -------#
mod.flounderA <- gam(cpue ~ 
                  period + 
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_flounder, loc == "østersøen_flounderA"), 
                family=nb) 
mod.flounderA <- get.mini.model(mod.flounderA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------PLAICE -------#
mod.plaiceA <- gam(cpue ~ 
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_plaice, loc == "østersøen_plaiceA"), #
                family=tw) 
mod.plaiceA <- get.mini.model(mod.plaiceA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------HERRING -------#
mod.herringA <- gam(cpue ~ 
                  period+
                  s(depth,k=5) +
                  s(Gear,bs="re")+ 
                  Quarter+
                  SedimentDK, 
                data = subset(dat_herring, loc == "østersøen_herringA"), 
                family=tw) 
mod.herringA <- get.mini.model(mod.herringA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------SPRAT -------#
mod.spratA <- gam(cpue ~ #response
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_sprat, loc == "østersøen_spratA"), 
                family=tw) 
mod.spratA <- get.mini.model(mod.spratA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------WHITING -------#
mod.whitingA<- gam(cpue ~ #response
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+
                  Quarter+ 
                  SedimentDK,
                data = subset(dat_whiting, loc == "østersøen_whitingA"), 
                family=tw) 
mod.whitingA <- get.mini.model(mod.whitingA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

############################### Predict CPUE ############################### 
## Assuming one overall depth for all locations is one approach, alternatively, one could use the median depth for each location

newdat <- expand.grid(period = levels(dat_cod$period),
                      loc = c("østersøen_codA","østersøen_flounderA",
                              "østersøen_plaiceA","østersøen_herringA",
                              "østersøen_spratA","østersøen_whitingA"),
                      depth = 19, #østersøen is 19
                      Gear=c("TVS"),
                      Quarter=c("4"),
                      SedimentDK=c("5")
)
#to check the depth of each location: 
#newdat$depth <- depth.by.loc[match(newdat$loc,names(depth.by.loc))] #overwrite for each location
## individual models
tmp1 <- predict(mod.codA, newdata = newdat, type = "response", se.fit = TRUE)
tmp2 <- predict(mod.flounderA, newdata = newdat, type = "response", se.fit = TRUE)
tmp3 <- predict(mod.plaiceA, newdata = newdat, type = "response", se.fit = TRUE)
tmp4 <- predict(mod.herringA, newdata = newdat, type = "response", se.fit = TRUE)
tmp5 <- predict(mod.spratA, newdata = newdat, type = "response", se.fit = TRUE)
tmp6 <- predict(mod.whitingA, newdata = newdat, type = "response", se.fit = TRUE)

#make new data predictions
newdat$cpue_pred <- NA
newdat$cpue_pred[newdat$loc == "østersøen_codA"] <- tmp1$fit[newdat$loc == "østersøen_codA"]
newdat$cpue_pred[newdat$loc == "østersøen_flounderA"] <- tmp2$fit[newdat$loc == "østersøen_flounderA"]
newdat$cpue_pred[newdat$loc == "østersøen_plaiceA"] <- tmp3$fit[newdat$loc == "østersøen_plaiceA"]
newdat$cpue_pred[newdat$loc == "østersøen_herringA"] <- tmp4$fit[newdat$loc == "østersøen_herringA"]
newdat$cpue_pred[newdat$loc == "østersøen_spratA"] <- tmp5$fit[newdat$loc == "østersøen_spratA"]
newdat$cpue_pred[newdat$loc == "østersøen_whitingA"] <- tmp6$fit[newdat$loc == "østersøen_whitingA"]

newdat$cpue_sd <- NA
newdat$cpue_sd[newdat$loc == "østersøen_codA"] <- tmp1$se.fit[newdat$loc == "østersøen_codA"]
newdat$cpue_sd[newdat$loc == "østersøen_flounderA"] <- tmp2$se.fit[newdat$loc == "østersøen_flounderA"]
newdat$cpue_sd[newdat$loc == "østersøen_plaiceA"] <- tmp3$se.fit[newdat$loc == "østersøen_plaiceA"]
newdat$cpue_sd[newdat$loc == "østersøen_herringA"] <- tmp4$se.fit[newdat$loc == "østersøen_herringA"]
newdat$cpue_sd[newdat$loc == "østersøen_spratA"] <- tmp5$se.fit[newdat$loc == "østersøen_spratA"]
newdat$cpue_sd[newdat$loc == "østersøen_whitingA"] <- tmp6$se.fit[newdat$loc == "østersøen_whitingA"]


newdat$cpue_lo <- newdat$cpue_pred - 1.96 * newdat$cpue_sd
newdat$cpue_up <- newdat$cpue_pred + 1.96 * newdat$cpue_sd

## individual models
tmp1.log <- predict(mod.codA, newdata = newdat, type = "link", se.fit = TRUE)
tmp2.log <- predict(mod.flounderA, newdata = newdat, type = "link", se.fit = TRUE)
tmp3.log <- predict(mod.plaiceA, newdata = newdat, type = "link", se.fit = TRUE)
tmp4.log <- predict(mod.herringA, newdata = newdat, type = "link", se.fit = TRUE)
tmp5.log <- predict(mod.spratA, newdata = newdat, type = "link", se.fit = TRUE)
tmp6.log <- predict(mod.whitingA, newdata = newdat, type = "link", se.fit = TRUE)

## for error propagations
newdat$cpue_pred.log <- NA
newdat$cpue_pred.log[newdat$loc == "østersøen_codA"] <- tmp1.log$fit[newdat$loc == "østersøen_codA"]
newdat$cpue_pred.log[newdat$loc == "østersøen_flounderA"] <- tmp2.log$fit[newdat$loc == "østersøen_flounderA"]
newdat$cpue_pred.log[newdat$loc == "østersøen_plaiceA"] <- tmp3.log$fit[newdat$loc == "østersøen_plaiceA"]
newdat$cpue_pred.log[newdat$loc == "østersøen_herringA"] <- tmp4.log$fit[newdat$loc == "østersøen_herringA"]
newdat$cpue_pred.log[newdat$loc == "østersøen_spratA"] <- tmp5.log$fit[newdat$loc == "østersøen_spratA"]
newdat$cpue_pred.log[newdat$loc == "østersøen_whitingA"] <- tmp6.log$fit[newdat$loc == "østersøen_whitingA"]
newdat$cpue_sd.log <- NA
newdat$cpue_sd.log[newdat$loc == "østersøen_codA"] <- tmp1.log$se.fit[newdat$loc == "østersøen_codA"]
newdat$cpue_sd.log[newdat$loc == "østersøen_flounderA"] <- tmp2.log$se.fit[newdat$loc == "østersøen_flounderA"]
newdat$cpue_sd.log[newdat$loc == "østersøen_plaiceA"] <- tmp3.log$se.fit[newdat$loc == "østersøen_plaiceA"]
newdat$cpue_sd.log[newdat$loc == "østersøen_herringA"] <- tmp4.log$se.fit[newdat$loc == "østersøen_herringA"]
newdat$cpue_sd.log[newdat$loc == "østersøen_spratA"] <- tmp5.log$se.fit[newdat$loc == "østersøen_spratA"]
newdat$cpue_sd.log[newdat$loc == "østersøen_whitingA"] <- tmp6.log$se.fit[newdat$loc == "østersøen_whitingA"]

if(line){
  write_rds(newdat,"~/Documents/DTU/Speciale/R/shiny/Shiny_fiskepleje/newdat/newdat_østersøenA.rds") 
}else{
  write_rds (newdat, "../newdat_østerøsenA.rds")
}
