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

# BEFORE CONTINUING ----
# You should now copy your database tables into the EM Export folder 

# Combine all data----

# Metadata ----
metadata <-ga.list.files("_Metadata.csv")%>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_em.csv(.))%>% # combine into dataframe
  dplyr::select(campaignid,dataset,planned.or.exploratory,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length)%>% # This line ONLY keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

unique(metadata$campaignid) # check the number of campaigns in metadata, and the campaign name

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine Points and Count files into maxn ----
maxn<-ga.create.em.maxn()%>%
  dplyr::inner_join(metadata)%>%
  #dplyr::filter(successful.count=="Yes")%>% # This will need to be turned on once  we have cleaned the metadata
  dplyr::filter(maxn>0)

unique(maxn$sample)

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)


## Combine Length, Lengths and 3D point files into length3dpoints----
length3dpoints<-ga.create.em.length3dpoints()%>%
  dplyr::select(-c(time,comment))%>% # take time out as there is also a time column in the metadata
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.length=="Yes")%>%
  glimpse()

## Save length files ----
setwd(staging.dir)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)






# TO GET CO-ORDS
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