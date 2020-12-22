## Script to do fish plots ----

# libraries ----
#install.packages("ggplot2")
library(ggplot2)
library(ggthemes)
library(extrafont)
library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
library(raster)
library(sp)
library(sf)
library(rgdal)
library(plyr)
library(maptools)
#library(mapmisc)
#install.packages("ggsn")
#devtools::install_github("3wen/legendMap")
library(ggsn)
library(grid)
library(maps)
library(broom)
library(tidyverse)
library(dplyr)
library(ggspatial)
library(colorspace)
library(RColorBrewer)
#install.packages("magritrr")
library(magritrr)
#install.packages("tidytext")
library(tidytext)
#install.packages("ggtextures")
library(ggtextures) # to add images to ggplot
#install.packages("magick")
library(magick) # to add images to ggplot


# clear workspace ----
rm(list = ls())

# functions ----
se <- function(x) sd(x)/sqrt(length(x))

# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- paste(w.dir, "Data/Tidy", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "plots", sep='/')


# load maxn data----
file.name <- "2020-06_sw_maxn.metadata.csv"
mxn <- read.csv(paste(dt.dir, file.name, sep='/')) %>%
  mutate_at(vars(full.name, sample, cluster), funs(as.factor)) %>%
  glimpse()


# calculate mean, sd and se for each species, per cluster --
summ.mxn <- mxn %>%
  dplyr::group_by(cluster, full.name) %>%
  dplyr::summarise(mean = mean(maxn), sd = sd(maxn), se = se(maxn)) %>%
  #ungroup() %>%
  glimpse()

## calculate summary and get top 10 mean max n per cluster --
top.mxn <- mxn %>%
  dplyr::group_by(full.name) %>%
  dplyr::summarise(mean = mean(maxn), sd = sd(maxn), se = se(maxn)) %>%
  top_n(10, mean) %>% # get largest 10 means per cluster
  #dplyr::summarise(mean = mean(maxn), sd = sd(maxn), se = se(maxn)) %>%
  arrange(desc(mean)) %>%
  arrange(desc(full.name)) %>%
  #arrange(desc(cluster)) %>% # arrange from largest to smallest
  ungroup() %>%
  # 2. Arrange by mean maxn
  arrange(mean, full.name) %>%
  # 3. Add order column of row numbers
  dplyr::mutate(order = dplyr::row_number()) %>%
  glimpse()


## PLOT ----

# choose colors --
bluepal <- choose_palette()

theme_set(theme_bw())

pd <-ggplot(data=top.mxn, aes(order, mean), y=mean) +
  geom_bar(stat="identity", color = "black", aes(fill = full.name)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~cluster, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  scale_fill_manual(values = bluepal(10)) +
  #scale_fill_manual(values = zonecolors) +
  labs(x = "Species", y = "mean MaxN") +
  # Add categories to axis
  scale_x_continuous(
    breaks = top.mxn$order,
    labels = top.mxn$full.name,
    expand = c(0,0)
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 10, face="italic"), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(color = 'black', fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip()

pd
