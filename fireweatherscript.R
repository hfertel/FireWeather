## ---------------------------
##
## Script name: Fire weather analysis
##
## Purpose of script: analysis for fire weather project 
##
## Author: Hannah Fertel
##
## Date Created: 2021-10-20
##
## Copyright (c) Hannah Fertel, 2021
## Email: hmfertel@berkeley.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## ---------------------------



library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(raster)
library(stars)



z<-nc_open("data/WRF_T_SFC.nc")
zz<-raster::raster("data/WRF_T_SFC2.nc")

print(zz)
zz<-nc_open("data/WRF_T_SFC2.nc")

zzZ1<-z$var$XLAT$id


zz#trying in ncdf4
time<-ncvar_get(zz,varid="time")
summary(time)

zz$dim$time$units
# units is "seconds since 1970-01-01 00:00:00"

zz$dim$south_north$units
zz$dim$west_east
zz$var$T_SFC$units
zz$var$T_SFC
zz$var$XLONG





####trying with stack####
library(raster)
filename<-"data/WRF_T_SFC2.nc"
zz1<-brick(filename) #has guessed that temperature is the variable of interest--way to get multiple variables 
zz1
#is there a way to get lat/long data from brick or is temperature the only one preserved? 
zzlat<-brick(filename, varname="XLAT")#brick with lats
zzlon<-brick(filename, varname="XLONG") #brick with longs 

#extract one of the rasters for each, then can make matrix, then can join to other??? 

zzlat1<-raster(zzlat,layer=1)
zzlon1<-raster(zzlat,layer=1)
#can put same extent, then extract values 



#extract one raster $`for a single hour
zz2<-zz1$X2015.01.28.15.00.00

#extracting a single raster layer 
zz3<-raster(zz1,layer=100)


plot(zz3)


#create an extent
#ext<-extent(200,250,300,350) #somewhere in the Sierras 
ext<-extent(142,168,187,213) #Monterey unit area--can get area of interest by selecting points in arc!  

#have to figure out order, I think it's x min x max, y min, y max) 



#could also identify grid cells of interest and extract that way? 

plot(zz2)
plot(ext, add=T) #check extent out 


#extract values within extent 
#extracted<-extract(zz1,ext)

#clip raster brick to an extent 
clip<-crop(zz1,ext,snap="out")
#is there a way to get lat/long or north/south east/west grid cell ids to be part of the matrix below? 




matrix<-as.matrix(clip)
matrix_t<-t(matrix)

matrix<-as_tibble(matrix)


#same as extracting...need to fgure out how to select specific rows of 
#interest maybe using and "ends in type of select for columns?)


#extract lat and longs for extent, then try to combine them somehow

latclip<-crop(zzlat,ext,snap="out")
lonclip<-crop(zzlon,ext,snap="out")



latm<-as.matrix(latclip)
lonm<-as.matrix(lonclip)


latm<-as.tibble(latm)
lonm<-as.tibble(lonm)

LL<-cbind(latm,lonm)

colnames(LL)<-c("Lat","Long")


LL$location<-paste(LL$Lat,",",LL$Long) #created data frame with location for each cell

#now need to make the row names in the extent temperature value 

colnames(matrix_t)<-LL$location

row.names(matrix)<-LL$location #connect location to each cell in extent with row names as location by extent 

#select only times of interest

matrix2<-as.tibble(matrix)
  

mycols<-paste0(c("09.00.00","10.00.00","11.00.00","12.00.00","13.00.00","14.00.00","15.00.00","16.00.00","17.00.00","18.00.00"),'$',collapse="|")         

matrix2<-matrix2 %>% 
  select(matches(mycols))
         
matrix2$location<-LL$location
matrix2$Lat<-LL$Lat
matrix2$Long<-LL$Long
#might want to do separate morning and evening ones, then combine? or find a way to code the averaging by the day?


####whole brick as matrix---instead of the lapply function####

lat1<-as.matrix(zzlat)
lon1<-as.matrix(zzlon)


wholeLL<-(cbind(lat1,lon1))

colnames(wholeLL)<-c("Lat","Long")

wholeLL<-as.tibble(wholeLL)

wholematrix<-as.matrix(zz1)
wholematrix<-as_tibble(wholematrix)

All1<-cbind(wholeLL,wholematrix)

#make a data frame with all the layers as rows 

d = data.frame(hr = names(zz1)) %>% 
  mutate(n = row_number())

d2 = slice(d, 1:10)

#somehow this gets 740 hrs for one grid cell 
zz1.2 = zz1[300,251,1]




#trying in stars package
t_file=system.file("data/WRF_T_SFC2.nc", package="stars")
temp=read_ncdf("data/WRF_T_SFC2.nc", regular = c("west_east","north_south"),ignore_bounds = TRUE)
#creates matrix of values--now we just need to take a slice to get values of interest
#what I'm seeing is the temperature at the first hour 

temp_slice=temp[1,2,1]#created slice that is temperature for one grid cell for every hour of the month of interest 
#values are in x and y, so there ARE lat/long...but how to get to it? 
#could extract grid cells of interest if there was an easy way to identify them...
#reset dimensions to lat long rather than west_east etc. 
temp_slice
#ugh IDK 


###MODIS DATA####
#potential to use MODIS data or ecostress data to use the daily temperature rather than hourly
#downloaded on file from MODIS, but probably need lengthier tutorial...
library(ncdf4)
library(rgdal)
library(gdalUtils)
library(raster)

gdalinfo("data/MOD11A1.A2020061.h07v05.061.2021006190235.hdf")
modis1<-get_subdatasets("data/MOD11A1.A2020061.h07v05.061.2021006190235.hdf")
modis1<-nc_open("data/MOD11A1.A2021091.h07v05.006.2021092231756.hdf")


gdal_translate(modis1[1],dst_dataset = "MODIS.tif")

r<-raster("MODIS.tif")
plot(r)


#using NCDF4
M<-nc_open("data/MOD11A1.A2020061.h07v05.061.2021006190235.hdf")
