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
df <- read.csv(paste(dt.dir, "2020_sw_complete_maxn-clusters.csv", sep = '/'))%>%
  mutate_at(vars(sample, family, unique.name, genus, full.name, species, status, cluster, cluster.new, number, n, dataset.x), list(as.factor)) %>% # make these columns as factors
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
head(n)
names(d) <- n[,2]


# Extract bathy derivatives from data points --
dfs <- raster::extract(b, dfs, sp = T)
str(dfs)

dfs <- raster::extract(d, dfs, sp = T)
str(dfs)

# Get SST covariates ----
t1 <- raster(paste(r.dir, "SSTmean_SSTARRS.tif", sep='/'))
t2 <- raster(paste(r.dir, "SSTsterr_SSTARRS.tif", sep='/'))
t3 <- raster(paste(r.dir, "SSTtrend_SSTARRS.tif", sep='/'))

ts <- stack(t1, t2, t3)
plot(ts$SSTmean_SSTARRS)
points(dfs)

dfs <- raster::extract(ts, dfs, sp = T)
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
