### script to Join maxN to metadata and cluster names ####


library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)
library(ggvegan)
library(FactoMineR)
library(factoextra)
library(rstudioapi)
library(stringr)



## Set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <-  paste(w.dir, "Data/Tidy", sep='/')
s.dir <- paste(w.dir, 'shapefiles', sep='/')



## Load data ----
file.name <- "2020-06_south-west_stereo-BRUVs.checked.maxn.csv"

df <- read.csv(paste(dt.dir, file.name, sep='/')) %>% 
  glimpse()

# prepare df --
df <- df %>% 
  unite(full.name, c(family, genus, species), sep = " ", remove = FALSE) %>% # make full name
  mutate_at(vars(campaignid, sample, full.name), funs(as.factor)) %>%  # define factors
  glimpse()


# Load metadata ----
metadata <- "2020-06_south-west_stereo-BRUVs.checked.metadata.csv"

md <- read.csv(paste(dt.dir, metadata, sep='/')) %>% 
  #mutate(sample = str_remove(sample, "0"))
  mutate(sample = str_replace(sample, "^0*", "")) %>% # remove zeros in front of numbers so it matches the df data - stringr::str_replace
  mutate_at(vars(campaignid, sample, location, status), funs(as.factor)) %>%
  #mutate(sample = recode(sample, '01'='1', '02'='2')) %>%
  glimpse()  # METADATA IS MISSING CLUSTER COLUMN
  

# Merge with metadata ----
fishdf <- merge(df, md, by = 'sample') %>%
  glimpse()
str(fishdf) # 39 BRUVs
levels(fishdf$sample)

# add cluster numbers ----
# load data
cs <- read.csv(paste(dt.dir, '2020-06_sw_deployed-clusters-samples.csv', sep='/')) %>% # this csv was done manually with Q GIS
  mutate_at(vars(sample, cluster), funs(as.factor)) %>%
  glimpse()

# Merge --
fishdf <- merge(fishdf, cs, by = 'sample') %>% 
  glimpse()

# save --
write.csv(fishdf, paste(dt.dir, "2020-06_sw_maxn.metadata.csv", sep='/'))





