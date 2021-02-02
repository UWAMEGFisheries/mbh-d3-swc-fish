### script to Join cluster sites to maxn data ####
# use the complete.maxn data, it should  have metadata already ##


library(ggplot2)
library(tidyr)
library(dplyr)
library(rstudioapi)
library(stringr)
library(sf)
library(rgdal)
library(sp)
library(raster)
library(viridis)
library(RColorBrewer)

# clear workspace ----
rm(list = ls())

## Set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <-  paste(w.dir, "Data/Tidy", sep='/')
s.dir <- paste(w.dir, 'shapefiles', sep='/')
r.dir <- paste(w.dir, 'Data/Raw',sep='/')

# Study name---
study <- "2020_south-west_stereo-BRUVs"

## Load metadata with cluster ID ----
mdc <- read.csv(paste(dt.dir, "2020_south-west_stereo_BRUVs.metadata.clusters.csv", sep='/')) %>%
  glimpse() # 490 bruvs planned

mdc.multiples <- mdc %>% # There are three deployments with multiple rows
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n()) %>%
  filter(n>1)

mdc.clean <- mdc %>%
  filter(!(sample  %in% c("IO235")&X%in%c("487"))) %>%
  filter(!(sample  %in% c("IO28"))) %>%
  filter(!(sample  %in% c("IO312")&X%in%c("478"))) %>%
  dplyr::select(cluster.new,sample)%>%
  dplyr::rename(Site=cluster.new,Sample=sample)

names(mdc.clean)

mdc.multiples <- mdc.clean %>% # 
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n()) %>%
  filter(n>1)

# Load metadata file -----
setwd(r.dir)
dir()

raw.metadata <- read.csv("2020-10_south-west_stereo-BRUVs_Metadata.csv")%>%
  dplyr::select(-c(Site))%>%
  dplyr::left_join(mdc.clean)

write.csv(raw.metadata,"metadata.with.clusters.csv")

# Load BRUV maxn data ---
bfile <- "2020_south-west_stereo-BRUVs.complete.maxn.csv"

b <- read.csv(paste(dt.dir, bfile, sep='/')) %>%
  mutate(unique.name  = paste(family, genus, species)) %>%
  mutate(full.name = paste(genus, species)) %>%
  mutate_at(vars(sample, family, genus, species, unique.name, full.name, campaignid, dataset, location, status), list(~as.factor(.))) %>%
  glimpse()


levels(b$sample) # 278 bruvs deployed (this number will drop BG)
str(b) # 45198  obs
names(b)

# Merge deployed bruv data with metadata ----
bwc <- b %>%
  dplyr::left_join(mdc.clean)

str(bwc) # 45198  obs -> 426 more
names(bwc)

# SAVE complete maxn with cluster info ----
setwd(dt.dir)

write.csv(bwc, paste(study, "complete.maxn.with.clusters.csv", sep='.'))





