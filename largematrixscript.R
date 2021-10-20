#to make giant matrix of values for entire month by grid cell


library(raster)
library(tidyverse)

filename<-"data/WRF_T_SFC2.nc"
zz1<-brick(filename) #has guessed that temperature is the variable of interest--way to get multiple variables 
zz1
#is there a way to get lat/long data from brick or is temperature the only one preserved? 
zzlat<-brick(filename, varname="XLAT")#brick with lats
zzlon<-brick(filename, varname="XLONG") #brick with longs 

lat1<-as.matrix(zzlat)
lon1<-as.matrix(zzlon)


wholeLL<-(cbind(lat1,lon1))

colnames(wholeLL)<-c("Lat","Long")

wholeLL<-as.tibble(wholeLL)

wholematrix<-as.matrix(zz1)
wholematrix<-as_tibble(wholematrix)

All1<-cbind(wholeLL,wholematrix)
