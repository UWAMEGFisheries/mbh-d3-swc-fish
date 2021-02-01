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



## Load metadata with cluster ID ----

mdc <- read.csv(paste(dt.dir, "2020_south-west_stereo_BRUVs.metadata.clusters.csv", sep='/')) %>%
  glimpse() # 490 bruvs planned

mdc.multiples <- mdc %>% # There are three deployments with multiple rows
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n()) %>%
  filter(n>1)

# Load BRUV maxn data ---

bfile <- "2020_south-west_stereo-BRUVs.complete.maxn.csv"

b <- read.csv(paste(dt.dir, bfile, sep='/')) %>%
  mutate(unique.name  = paste(family, genus, species)) %>%
  mutate(full.name = paste(genus, species)) %>%
  mutate_at(vars(sample, family, genus, species, unique.name, full.name, campaignid, dataset, location, status), list(~as.factor(.))) %>%
  glimpse()


levels(b$sample) # 277 bruvs deployed
str(b) # 47260 obs
names(b)

# Merge deployed bruv data with metadata ----

bwc <- merge(b, mdc, by = 'sample', all=T)
str(bwc) # 47686 obs -> 426 more
levels(bwc$sample) # 531

# Remove rows with NA's in maxn column --
bwc2  <- bwc[!is.na(bwc$maxn),]
str(bwc2) # 39950 obs -> 7310 less
levels(bwc2$sample) # 277

# SAVE complete maxn with cluster info ----
#write.csv(bwc2, paste(dt.dir, "2020_sw_complete_maxn-clusters.csv", sep='/'))





