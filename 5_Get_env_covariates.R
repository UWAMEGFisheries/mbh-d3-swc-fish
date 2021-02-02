### Get environmental covariates for Species Archetype Models ----

# Load libraries ----

#library(devtools)
#devtools::install_github('skiptoniam/ecomix')
#install_github("twitter/AnomalyDetection")
#ibrary(ecomix)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(spastat)


# clear workspace ----
rm(list = ls())


# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "Data/Tidy", sep='/'))
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')


# Load data ----

study <- "2020_south-west_stereo-BRUVs"

df <- read.csv(paste(dt.dir, paste(study, "complete.maxn.with.clusters.csv", sep='.'), sep = '/'))%>%
  mutate_at(vars(sample, family, unique.name, genus, full.name, species, status, cluster, cluster.new, number, n, dataset), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude 

# Get bathy and derivatives ----
b <- raster(paste(r.dir, "SW_bathy-to-260m.tif", sep='/'))
plot(b)
points(dfs)
d <- stack(paste(r.dir, "SW_detrendend.derivatives-to-260m.tif", sep='/'))
plot(d)
names(d)
n <- read.csv(paste(r.dir, "names.det.bath.csv", sep='/'))
n
n$covs <- c("detrended.bathy", "slope", "flowdir", "tri", "tpi", "aspect")
names(d) <- n[,3]


# Extract bathy derivatives from data points --
dfs <- raster::extract(b, dfs, sp = T)
str(dfs)

dfs <- raster::extract(d, dfs, sp = T)
str(dfs)
head(dfs)


## so far just using bathy covariates ##

# save up to here ----
write.csv(dfs, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))


###       ###       ###       ###

# Still working on this section ----

# Get SST covariates ----
t1 <- raster(paste(r.dir, "SSTmean_SSTARRS.tif", sep='/'))
t2 <- raster(paste(r.dir, "SSTsterr_SSTARRS.tif", sep='/'))
t3 <- raster(paste(r.dir, "SSTtrend_SSTARRS.tif", sep='/'))

ts <- stack(t1, t2, t3)
plot(ts$SSTmean_SSTARRS)
points(dfs)

dfs <- raster::extract(ts$SSTmean_SSTARRS, dfs, sp = T)
head(dfs)
dfs@data

dist <- distance(dfs)


head(df)
names(df)
xy <- df[,c(12,11)]


## Remove NA's by replacing with nearest neighbor ----
# https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel

# use normal extract function to show that NAs are extracted for some points
extracted <- raster::extract(x = ts$SSTmean_SSTARRS, y = xy)


# then take the raster value with lowest distance to point AND non-NA value in the raster
dfs$sampled <-  apply(X = xy, MARGIN = 1, FUN = function(xy) ts$SSTmean_SSTARRS@data@values[which.min(replace(distanceFromPoints(ts$SSTmean_SSTARRS, xy), is.na(ts$SSTmean_SSTARRS), NA))])
str(dfs)
levels(dfs$sample)


### ADD this to use habitat coveriates or save ----

# save without habitat covs ----
write.csv(df2, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))


###       ###       ###       ###       ###

## Get habitat covariates (BRUV style) ----
df2 <- as.data.frame(dfs)
head(df2)
str(df2)
levels(df2$sample)
names(df2)

# get habitat data - used detailed but can use coarse --
h <- read.csv("G:/My Drive/Anita/NESP_D3/2020-06._detailed.habitat.csv")%>%
  mutate_at(vars(sample, site, location), list(as.factor)) %>% # make these columns as factors
  mutate(sample = recode(sample,
                         "01" = "1", "02" = "2", "03" = "3", "04" ="4", "05" = "5",
                         "06" = "6", "07" = "7", "08" = "8", "09" = "9")) %>% # make sure sample factor names are the same as in df2
  glimpse()
head(h)
str(h)
levels(h$sample)
names(h)

# Merge df2 with habitat data by sample --

dfh <- merge(df2 %>% dplyr::select(-c(X, campaignid.x, campaignid.y, site, observer, successful.count, successful.length, deployment, notes)),
             h %>% dplyr::select(-c(latitude, longitude, date, time, site, location, successful.count, fov.Limited, fov.Open, type)),
             by = 'sample') %>%
  glimpse()

str(dfh)
names(dfh)

dfh <- dfh %>% rename(bathy = SW_bathy.to.260m, slope = detr.slope, flowdir =  detr.flowdir, tri = detr.tri,
                      tpi = detr.tpi, aspect = detr.aspect,
                      temp_mean = SSTmean_SSTARRS, temp_sterr = SSTsterr_SSTARRS, temp_trend = SSTtrend_SSTARRS)

# Save csv of MaxN and covariates ----
write.csv(dfh, paste(dt.dir, "2020-06_sw_maxn.meta.cov.csv", sep='/'))
