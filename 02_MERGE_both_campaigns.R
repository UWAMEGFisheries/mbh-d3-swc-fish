### Merge EventMeasure database output tables into maxn and length files

### OBJECTIVES ###
# combine database tables into single Metadata, MaxN and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to GitHub
library(RCurl)
library(rgdal)
library(R.utils)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
# to connect to googlesheets
library(googlesheets4)
library(sp)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"2020_south-west_stereo-BRUVs"  # BG changed this from 2020-06 on 27/01/21

## Folder Structure ----
# This script uses one main folder ('working directory')
# Three subfolders will be created within the 'working directory'. They are 'EM Export','Staging' and 'Tidy data'
# Save the database exports into the 'EM Export' folder
# The 'Staging' folder is used to save the combined files (e.g. metadata, maxn or length) NOTE: These initial outputs have not gone through any check (e.g. checks against the life-history sheet)

# Naming in the script is extremely important! Names need to be consistent or the script will break

# **The only folder you will need to create is your working directory**

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
data.dir<-paste(working.dir,"Data",sep="/")

download.dir<-paste(data.dir,"Raw",sep="/")

tidy.dir<-paste(data.dir,"Tidy",sep="/")
staging.dir<-paste(data.dir,"Staging",sep="/") 

setwd(working.dir)

## Create Staging and Tidy data folders ----
dir.create(file.path(data.dir, "Staging"))
dir.create(file.path(data.dir, "Tidy"))

# Combine all data----

# Metadata ----
metadata <- ga.list.files("_Metadata.csv") %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_em.csv(.)) %>% # combine into dataframe
  dplyr::select(campaignid, sample, dataset, planned.or.exploratory, latitude, longitude, date, time, location, status, site, depth, observer, successful.count, successful.length, commonwealth.zone, state.zone)%>% 
  glimpse()

names(metadata)

unique(metadata$campaignid) # check the number of campaigns in metadata, and the campaign name
unique(metadata$sample) # 316

double.ups <- metadata %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::filter(n>1) # One double up where the BRUV wasn't retrieved

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine Points and Count files into maxn ----
points.files <-ga.list.files("_Points.txt") # list all files ending in "Lengths.txt"
points.files$lines<-sapply(points.files,countLines) # Count lines in files (to avoid empty files breaking the script)

points<-as.data.frame(points.files)%>%
  dplyr::mutate(campaign=row.names(.))%>%
  filter(lines>1)%>% # filter out all empty text files
  dplyr::select(campaign)%>%
  as_vector(.)%>% # remove all empty files
  purrr::map_df(~ga.read.files_txt(.))%>%
  dplyr::mutate(campaignid=str_replace_all(.$project,c("_Points.txt"="")))%>%
  dplyr::select(-c(project))

maxn<-points%>%
  dplyr::mutate(species=if_else(genus%in%c("Orectolobus","Caesioperca","Platycephalus","Squalus"),"spp",species))%>%
  dplyr::group_by(campaignid,sample,filename,period,periodtime,frame,family,genus,species,comment)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::filter(maxn>0)%>%
  dplyr::inner_join(metadata)#%>%
  #dplyr::filter(successful.count=="Yes") # This will need to be turned on once  we have cleaned the metadata

unique(maxn$sample) #287 (this should drop)

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

## Combine Length, Lengths and 3D point files into length3dpoints----
length3dpoints<-ga.create.em.length3dpoints()%>%
  dplyr::mutate(species=if_else(genus%in%c("Orectolobus","Caesioperca","Platycephalus","Squalus"),"spp",species))%>%
  dplyr::select(-c(time,comment))%>% # take time out as there is also a time column in the metadata
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.length=="Yes")%>%
  glimpse()

## Save length files ----
setwd(staging.dir)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)

# TO GET CO-ORDS for metadata
# setwd(download.dir)
# dir()
# 
# in.out.locs<-read.csv("in_and_out_locations.csv")
# 
# fh.locs<-read.csv("fishing_hwy_locations.csv")
# 
# locs<-bind_rows(in.out.locs,fh.locs)%>%
#   select(Sample,Latitude,Longitude)%>%
#   rename(Latitude.1=Longitude,Longitude.1=Latitude) # fix error i made
# 
# 
# test<-locs%>%
#   group_by(Sample)%>%
#   summarise(n=n())
# 
# labsheet<-read.csv("2020-10_south-west_stereo-BRUVs_Metadata.csv")%>%
#   left_join(locs)%>%
#   mutate(Latitude=ifelse(is.na(Latitude),Latitude.1,Latitude))%>%
#   mutate(Longitude=ifelse(is.na(Longitude),Longitude.1,Longitude))
# 
# test<-labsheet%>%filter(is.na(Latitude))
# 
# test<-labsheet%>%
#   group_by(Sample)%>%
#   summarise(n=n())
# 
# write.csv(labsheet,"new.metadata.csv")


# # Get zone/status for 2020-10
# setwd(download.dir)
# dir()
# 
# metadata.2020.10 <- read.csv("2020-10_south-west_stereo-BRUVs_Metadata.csv")
# raw.metadata<-metadata.2020.10
# 
# # Spatial files ----
# setwd(working.dir)
# wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# commonwealth.marineparks <- readOGR(dsn="shapefiles/AustraliaNetworkMarineParks.shp")
# proj4string(commonwealth.marineparks)
# 
# wa.marineparks <- readOGR(dsn="shapefiles/test1.shp")
# proj4string(wa.marineparks)
# 
# proj4string(commonwealth.marineparks)<-CRS(wgs.84)
# proj4string(wa.marineparks)<-CRS(wgs.84)
# 
# coordinates(metadata.2020.10) <- c('Longitude', 'Latitude')
# proj4string(metadata.2020.10)<-CRS(wgs.84)
# 
# metadata.commonwealth.marineparks <- over(metadata.2020.10, commonwealth.marineparks) %>%
#   dplyr::select(ZoneName)
# 
# unique(metadata.commonwealth.marineparks$ZoneName)
# 
# metadata.state.marineparks <- over(metadata.2020.10, wa.marineparks) %>%
#   dplyr::select(Name)
# 
# unique(metadata.state.marineparks$Name)
# 
# names(metadata.commonwealth.marineparks)
# 
# metadata<-bind_cols(raw.metadata,metadata.commonwealth.marineparks)%>%
#   bind_cols(.,metadata.state.marineparks)%>%
#   dplyr::rename(Commonwealth.zone=ZoneName, State.zone=Name)%>%
#   mutate(Status = if_else((Commonwealth.zone%in%c("National Park Zone")|
#                             State.zone%in%c("Injidup Sanctuary Zone","Cape Freycinet Sanctuary Zone")),"No-take","Fished"))
# 
# write.csv(metadata, "metadata.with.status.csv")
#          