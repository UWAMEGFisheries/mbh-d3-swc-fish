### Testing SpeciesMix script ####
# Fit Mixtures of Archetype Species # 

# Load libraries ----

#library(devtools)
#devtools::install_github('skiptoniam/ecomix')
#install_github("twitter/AnomalyDetection")
library(ecomix)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)

# clear workspace ----
rm(list = ls())


# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- paste(w.dir, "Data/Tidy", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')


# Load data ----
df <- read.csv(paste(dt.dir, "2020-06_sw_maxn.meta.cov.csv", sep = '/'))%>%
  mutate_at(vars(sample, family, unique.name, genus, full.name, species, location, status, cluster), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)
names(df)


# if want to plot ----
dfs <- df
coordinates(dfs) <- ~longitude+latitude 

# Prepare data ----
pd <- table_to_species_data(
  df,
  site_id = "cluster", # use cluster? or status?
  species_id = "full.name",
  measurement_id = "maxn"
)

pd

cd <- make_mixture_data(species_data = df$maxn,
  covariate_data = c(mean.relief, sd.relief, depth)
)



