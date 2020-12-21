
library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)
library(ggvegan)
library(FactoMineR)
library(factoextra)
library(rstudioapi)


## Set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <-  paste(w.dir, "Data/Tidy", sep='/')



## Load data ----
file.name <- "2020-06_south-west_stereo-BRUVs.checked.maxn.csv"

df <- read.csv(paste(dt.dir, file.name, sep='/')) %>% 
  glimpse()

# define factors 
test <- df %>% 
  unite(full.name, c(family, genus, species), sep = " ", remove = FALSE) %>% 
  glimpse()

test <- test %>%
  mutate_at(,c(1,2,3), as.factor)
