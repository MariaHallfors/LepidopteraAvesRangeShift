
###################
#Subsampling Lepidoptera data
#M. Hällfors 20201126
###################


#load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)

setwd("C:/Users/hallfors/Documents/Lat-effect/")


#################
#load and manage data
#################
LEP=read.csv("LepiSpsDistrRaw_20201126.csv")
head(LEP)


#divide into timeperiods
#Make new variable called timeperiod, as numeric
LEP$TP=0
LEP$TP[LEP$Year>=1974 & LEP$Year<=1989]=1
LEP$TP[LEP$Year>=2006 & LEP$Year<=2010]=2
#make Kymppiruutu -variable
LEP$KymppiR=paste(LEP$Y10, LEP$X10)

#remove the observations that aren't from TP 1 or 2
LEP12=LEP%>%
  filter(TP!=0)

#how many observations do we have?
LEPinfo <- LEP %>% 
  group_by(TP) %>%
  dplyr::mutate(NobsTot=length(TP)) %>% #total obs for all species per TP
  dplyr::mutate(PresNTot=length(unique(KymppiR))) %>% #Tot presence in kymppiruutu per TP
  ungroup()

Nobs1=LEPinfo$NobsTot[LEPinfo$TP==1][1] #116304
Nobs2=LEPinfo$NobsTot[LEPinfo$TP==2][1] #547742


length(unique(LEP12$KymppiR[LEP12$TP==1])) #1011
length(unique(LEP12$KymppiR[LEP12$TP==2])) #1839 
#or
#LEPinfo$PresNTot[LEPinfo$TP==1][1] 
#LEPinfo$PresNTot[LEPinfo$TP==2][1] 
#--> in time period 1 the observations come from fewer individual 10*10 cells


#check that we have all sps in both TPs
length(unique(LEP12$Species[LEP12$TP==1])) #289
length(unique(LEP12$Species[LEP12$TP==2])) #289


#how much do the mean latitudes difference between the TPs
mt1=mean(LEP12$YEUREF[LEP12$TP==1], na.rm=TRUE)
mt2=mean(LEP12$YEUREF[LEP12$TP==2], na.rm=TRUE)
mt2-mt1
#-44933.99
#the average latitude is about 4 kilometers more south in TP2



###################
#how many species per kymppiruutu/TP
sps1=LEP12%>%
  group_by(KymppiR, TP)%>%
  dplyr::mutate(spsRich=length(unique(Species)))

#remove duplicated Kymppiruutus per TP
sps1$Kymppi_TP=paste(sps1$KymppiR, sps1$TP)
dat=sps1[!duplicated(sps1$Kymppi_TP), ]

hist(dat$spsRich)

#make categorical variable for maps
dat$NspsXY=">100"
dat$NspsXY[dat$spsRich<=100]="51-100"
dat$NspsXY[dat$spsRich<=50]="11-50"
dat$NspsXY[dat$spsRich<=10]="1-10"

#put in correct order
dat$NspsXY=factor(dat$NspsXY, levels=c("1-10", "11-50", "51-100", ">100"))

#aquire basemap of finland
fin  <- raster::getData("GADM",country="Finland",level=0)
fin=spTransform(fin, CRS("+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs"))


#separate TPs to make maps
dat1=dat%>%
  filter(TP==1)
dat2=dat%>%
  filter(TP==2)


#Maps on species richness in thw two time periods
T1=ggplot() + 
  geom_polygon(data= fin, aes(x=long,y=lat,group=group), colour='darkgrey', fill=NA)+
  geom_point(data=dat1%>%filter(NspsXY=="1-10"), 
             aes(x=XEUREF, y=YEUREF), col="#fee5d9", shape=16, size=2)+ # I plot them separaltely like this so that they would plot on top of each other as groups, not in the order they are in the df
  geom_point(data=dat1%>%filter(NspsXY=="11-50"), 
             aes(x=XEUREF, y=YEUREF), col="#fcae91", shape=16, size=2)+
  geom_point(data=dat1%>%filter(NspsXY=="51-100"), 
             aes(x=XEUREF, y=YEUREF), col="#fb6a4a", shape=16, size=2, alpha=0.6)+
  geom_point(data=dat1%>%filter(NspsXY==">100"), 
             aes(x=XEUREF, y=YEUREF), col="#cb181d", shape=16, size=2, alpha=0.6)+
  theme_classic()+
  xlab("Longitude") + ylab("Latitude")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

T2=ggplot() + 
  geom_polygon(data= fin, aes(x=long,y=lat,group=group), colour='darkgrey', fill=NA)+
  geom_point(data=dat2%>%filter(NspsXY=="1-10"), 
             aes(x=XEUREF, y=YEUREF), col="#fee5d9", shape=16, size=2)+ # I plot them separaltely like this so that they would plot on top of each other as groups, not in the order they are in the df
  geom_point(data=dat2%>%filter(NspsXY=="11-50"), 
             aes(x=XEUREF, y=YEUREF), col="#fcae91", shape=16, size=2)+
  geom_point(data=dat2%>%filter(NspsXY=="51-100"), 
             aes(x=XEUREF, y=YEUREF), col="#fb6a4a", shape=16, size=2, alpha=0.6)+
  geom_point(data=dat2%>%filter(NspsXY==">100"), 
             aes(x=XEUREF, y=YEUREF), col="#cb181d", shape=16, size=2, alpha=0.6)+
  theme_classic()+
  xlab("Longitude") + ylab("Latitude")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

plot_grid(T1, T2, ncol = 2, 
          labels = c('A', 'B'))



#corresponding map on observation numbers, that is how many observations per kymppiruutu/TP

#make variable of unique KymppiR*TP combo
LEP12$Kymppi_TP=paste(LEP12$KymppiR, LEP12$TP)

#calculate observations per KymppiR*TP combo
sps1=LEP12%>%
  group_by(Kymppi_TP)%>%
  dplyr::mutate(Obs=length(Kymppi_TP))

#remove duplicated Kymppiruutus per TP
dat=sps1[!duplicated(sps1$Kymppi_TP), ]

#make categorical variable for map
dat$ObsC=">100"
dat$ObsC[dat$Obs<=100]="51-100"
dat$ObsC[dat$Obs<=50]="11-50"
dat$ObsC[dat$Obs<=10]="1-10"

#put in correct order
dat$ObsC=factor(dat$ObsC, levels=c("1-10", "11-50", "51-100", ">100"))



##aquire basemap of finland, if needed
# fin  <- raster::getData("GADM",country="Finland",level=0)
# fin=spTransform(fin, CRS("+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs"))


#separate TPs to make maps
dat1=dat%>%
  filter(TP==1)
dat2=dat%>%
  filter(TP==2)

p1=ggplot() + 
  geom_polygon(data= fin, aes(x=long,y=lat,group=group), colour='darkgrey', fill="#f0f0f0")+
  geom_point(data=dat1%>%filter(ObsC=="1-10"), 
             aes(x=XEUREF, y=YEUREF), col="#a1d99b", shape=16, size=2)+ # I plot them separaltely like this so that they would plot on top of each other as groups, not in the order they are in the df
  geom_point(data=dat1%>%filter(ObsC=="11-50"), 
             aes(x=XEUREF, y=YEUREF), col="#41ab5d", shape=16, size=2)+
  geom_point(data=dat1%>%filter(ObsC=="51-100"), 
             aes(x=XEUREF, y=YEUREF), col="#006d2c", shape=16, size=2)+
  geom_point(data=dat1%>%filter(ObsC==">100"), 
             aes(x=XEUREF, y=YEUREF), col="#00441b", shape=16, size=2)+
  theme_classic()+
  xlab("Longitude") + ylab("Latitude")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

p2=ggplot() + 
  geom_polygon(data= fin, aes(x=long,y=lat,group=group), colour='darkgrey', fill="#f0f0f0")+
  geom_point(data=dat2%>%filter(ObsC=="1-10"), 
             aes(x=XEUREF, y=YEUREF), col="#a1d99b", shape=16, size=2)+ # I plot them separaltely like this so that they would plot on top of each other as groups, not in the order they are in the df
  geom_point(data=dat2%>%filter(ObsC=="11-50"), 
             aes(x=XEUREF, y=YEUREF), col="#41ab5d", shape=16, size=2)+
  geom_point(data=dat2%>%filter(ObsC=="51-100"), 
             aes(x=XEUREF, y=YEUREF), col="#006d2c", shape=16, size=2)+
  geom_point(data=dat2%>%filter(ObsC==">100"), 
             aes(x=XEUREF, y=YEUREF), col="#00441b", shape=16, size=2)+
  theme_classic()+
  xlab("Longitude") + ylab("Latitude")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

plot_grid(p1, p2, ncol = 2, 
          labels = c('A', 'B'))




###################
#SUB-SAMPLING
###################


###Divide into latitudinal groups

#Define groups
str(LEP12)
max(LEP12$YEUREF, na.rm=TRUE) #7774755
min(LEP12$YEUREF, na.rm=TRUE) #6605839
7774755-6605839
1168916/5 #233783.2 --> i use this for the increments

6605839+233783.2 #6839622
6605839+(233783.2*2) #7073405
6605839+(233783.2*3) #7307189
6605839+(233783.2*4) #7540972


#make group variable
LEP12=LEP12%>%
  filter(YEUREF!="NA")
LEP12$Latgroup=1
LEP12$Latgroup[LEP12$YEUREF>=6839622 & LEP12$YEUREF<=7073405]=2
LEP12$Latgroup[LEP12$YEUREF>=7073405 & LEP12$YEUREF<=7307189]=3
LEP12$Latgroup[LEP12$YEUREF>=7307189 & LEP12$YEUREF<=7540972]=4
LEP12$Latgroup[LEP12$YEUREF>=7540972]=5

dev.off()
#visualize
ggplot() + 
  geom_polygon(data= fin, aes(x=long,y=lat,group=group), colour='darkgrey', fill="#f0f0f0")+
  # geom_point(data=LEP12%>%filter(TP==1), 
  #            aes(x=XEUREF, y=YEUREF), col="#a1d99b", shape=16, size=2)+ 
  # geom_point(data=LEP12%>%filter(TP==2), 
  #            aes(x=XEUREF, y=YEUREF), col="#41ab5d", shape=16, size=2)+
  geom_hline(yintercept=6839622, linetype="dashed", color = "darkgrey")+
  geom_hline(yintercept=7073405, linetype="dashed", color = "darkgrey")+
  geom_hline(yintercept=7307189, linetype="dashed", color = "darkgrey")+
  geom_hline(yintercept=7540972, linetype="dashed", color = "darkgrey")+
  theme_classic()+
  xlab("Longitude") + ylab("Latitude")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))






#make variable that is a combination of TP and Lat-group
LEP12$TP_Lat=paste(LEP12$TP, LEP12$Latgroup)

#how many observations do we have per TP_lat?
LEPinfo <- LEP12 %>% 
  group_by(TP_Lat) %>%
  dplyr::mutate(Obs_TP_Lat=length(TP_Lat)) %>% 
  ungroup()

table(LEPinfo$TP_Lat)

Nobs11=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="1 1"][1] #78454
Nobs12=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="1 2"][1] #21042
Nobs13=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="1 3"][1] #10050
Nobs14=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="1 4"][1] #5264 --> there's more observations in TP 1 for this lat group
Nobs15=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="1 5"][1] #1264 --> there's more observations in TP 1 for this lat group


Nobs21=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="2 1"][1] #374977
Nobs22=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="2 2"][1] #143250
Nobs23=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="2 3"][1] #25667
Nobs24=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="2 4"][1] #2630
Nobs25=LEPinfo$Obs_TP_Lat[LEPinfo$TP_Lat=="2 5"][1] #1207

(78454 + 21042+ 10050+ 2630 +1207)*2
#113383 is the number of observatiosn I should end up with after resampling

#do the resampling, separaetly for each latgroup

#resample Lat1 in TP2
TP2LG1=LEP12 %>%
  filter(Latgroup==1 & TP==2) %>%                                 # for every group
  nest() %>%          # nest data
  dplyr::mutate(v = map2(data, Nobs11, sample_n, replace=FALSE)) %>%  # sample using id values and (unique) frq value
  unnest(v)
dim(TP2LG1)

#resample Lat2 in TP2
TP2LG2=LEP12 %>%
  filter(Latgroup==2 & TP==2) %>%                                 # for every group
  nest() %>%          # nest data
  dplyr::mutate(v = map2(data, Nobs12, sample_n, replace=FALSE)) %>%  # sample using id values and (unique) frq value
  unnest(v)
dim(TP2LG2)

#resample Lat3 in TP2
TP2LG3=LEP12 %>%
  filter(Latgroup==3 & TP==2) %>%                                 # for every group
  nest() %>%          # nest data
  dplyr::mutate(v = map2(data, Nobs13, sample_n, replace=FALSE)) %>%  # sample using id values and (unique) frq value
  unnest(v)
dim(TP2LG3)

#resample Lat4 in TP1
TP1LG4=LEP12 %>%
  filter(Latgroup==4 & TP==1) %>%                                 # for every group
  nest() %>%          # nest data
  dplyr::mutate(v = map2(data, Nobs24, sample_n, replace=FALSE)) %>%  # sample using id values and (unique) frq value
  unnest(v)
dim(TP1LG4)

#resample Lat5 in TP1
TP1LG5=LEP12 %>%
  filter(Latgroup==5 & TP==1) %>%                                 # for every group
  nest() %>%          # nest data
  dplyr::mutate(v = map2(data, Nobs25, sample_n, replace=FALSE)) %>%  # sample using id values and (unique) frq value
  unnest(v)
dim(TP1LG5)

#seperate the observations you want to include from the original data
TP1=LEP12%>%
  filter(TP==1 & Latgroup==1 | TP==1 & Latgroup==2 | TP==1 & Latgroup==3)
dim(TP1)

TP2=LEP12%>%
  filter(TP==2 & Latgroup==4 | TP==2 & Latgroup==5)
dim(TP2)

#combine it all
resampdata=rbind(TP1, TP2, TP2LG1, TP2LG2, TP2LG3, TP1LG4, TP1LG5)
dim(resampdata) ##113383 --> perfect!




####################
#make map on the resampled data
####################

#how many obs per kymppiruutu/TP
sps1=resampdata%>%
  group_by(Kymppi_TP)%>%
  dplyr::mutate(Obs=length(Kymppi_TP))


#remove duplicated Kymppiruutus per TP
dat=sps1[!duplicated(sps1$Kymppi_TP), ]

ggplot(data=dat, aes(x=Obs))+
  geom_histogram()+
  facet_wrap(~TP)

View(dat) #we have a six kymppiruutus from TP1  with 3600-9000 observations from each!!

dat$ObsC=">100"
dat$ObsC[dat$Obs<=100]="51-100"
dat$ObsC[dat$Obs<=50]="11-50"
dat$ObsC[dat$Obs<=10]="1-10"

#put in cporrect order
dat$ObsC=factor(dat$ObsC, levels=c("1-10", "11-50", "51-100", ">100"))



# #aquire basemap of finland, if needed
# fin  <- raster::getData("GADM",country="Finland",level=0)
# fin=spTransform(fin, CRS("+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs"))


dat1=dat%>%
  filter(TP==1)

dat2=dat%>%
  filter(TP==2)


p1=ggplot() + 
  geom_polygon(data= fin, aes(x=long,y=lat,group=group), colour='darkgrey', fill="#f0f0f0")+
  geom_point(data=dat1%>%filter(ObsC=="1-10"), 
             aes(x=XEUREF, y=YEUREF), col="#a1d99b", shape=16, size=2)+ # I plot them separaltely like this so that they would plot on top of each other as groups, not in the order they are in the df
  geom_point(data=dat1%>%filter(ObsC=="11-50"), 
             aes(x=XEUREF, y=YEUREF), col="#41ab5d", shape=16, size=2)+
  geom_point(data=dat1%>%filter(ObsC=="51-100"), 
             aes(x=XEUREF, y=YEUREF), col="#006d2c", shape=16, size=2)+
  geom_point(data=dat1%>%filter(ObsC==">100"), 
             aes(x=XEUREF, y=YEUREF), col="#00441b", shape=16, size=2)+
  theme_classic()+
  xlab("Longitude") + ylab("Latitude")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


#map figure c
p2=ggplot() + 
  geom_polygon(data= fin, aes(x=long,y=lat,group=group), colour='darkgrey', fill="#f0f0f0")+
  geom_point(data=dat2%>%filter(ObsC=="1-10"), 
             aes(x=XEUREF, y=YEUREF), col="#a1d99b", shape=16, size=2)+ # I plot them separaltely like this so that they would plot on top of each other as groups, not in the order they are in the df
  geom_point(data=dat2%>%filter(ObsC=="11-50"), 
             aes(x=XEUREF, y=YEUREF), col="#41ab5d", shape=16, size=2)+
  geom_point(data=dat2%>%filter(ObsC=="51-100"), 
             aes(x=XEUREF, y=YEUREF), col="#006d2c", shape=16, size=2)+
  geom_point(data=dat2%>%filter(ObsC==">100"), 
             aes(x=XEUREF, y=YEUREF), col="#00441b", shape=16, size=2)+
  theme_classic()+
  xlab("Longitude") + ylab("Latitude")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))



plot_grid(p1, p2, ncol = 2, 
          labels = c('A', 'B'))
