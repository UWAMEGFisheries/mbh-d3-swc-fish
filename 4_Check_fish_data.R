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
library(sf)
library(maps)
library(viridis)
library(RColorBrewer)



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
  unite(unique.name, c(family, genus, species), sep = " ", remove = FALSE) %>% # make full name
  unite(full.name, c(genus, species), sep = " ", remove = FALSE) %>% # make full name
  mutate_at(vars(campaignid, sample, unique.name, full.name), funs(as.factor)) %>%  # define factors
  glimpse()

str(df)


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
summary(fishdf)

# add cluster numbers ----
# load data
cs <- read.csv(paste(dt.dir, '2020-06_sw_deployed-clusters-samples.csv', sep='/')) %>% # this csv was done manually with Q GIS
  mutate_at(vars(sample, cluster), funs(as.factor)) %>%
  glimpse()

summary(cs)
str(cs)

# Merge --
fishdf <- merge(fishdf, cs, by = 'sample') %>% 
  glimpse()
summary(fishdf)
str(fishdf)
levels(fishdf$cluster)

# save --
write.csv(fishdf, paste(dt.dir, "2020-06_sw_maxn.metadata.csv", sep='/'))

# Map BRUVS ----
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
#bluepal <- choose_palette()
#pal1 <- c("#D6E0FF" ,"#99A5E0" ,"#666E9A", "#373D58" ,"#111111", "#2D4424", "#557A47" ,"#82B56F" ,"#C2EFB4")

# see palettes 
display.brewer.all()
display.brewer.all(colorblindFriendly = TRUE)
# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 9, name = "RdYlBu")
# Hexadecimal color specification 
pal2 <- brewer.pal(n = 9, name = "RdYlBu")


fs <- fishdf

coordinates(fs) <- ~longitude + latitude
plot(fs, pc =20, col=fs$cluster)

fs2 <- st_as_sf(fs, coords = c('longitude', 'latitude'), crs=4462, agr = "constant")
summary (fs2)
str(fs2)

# different waysto plot the same thing
fs2 %>% ggplot() + geom_sf(aes(col=cluster)) + scale_colour_manual(values = bluepal(9)) +theme_bw()

fs2 %>% ggplot() + geom_sf(aes(col=cluster)) + scale_colour_manual(values = pal2) +theme_bw()

fs2 %>% ggplot() + geom_sf(aes(col=cluster)) + scale_colour_viridis(discrete = TRUE, option = "D") +theme_bw()

### NEED TO CHECK WITH BROOKE AND KYE: THERE ARE SEVERAL BRUVS THAT ARE TOO CLOSE TOGETHER
### nc means the BRUv does not belong to a cluster
