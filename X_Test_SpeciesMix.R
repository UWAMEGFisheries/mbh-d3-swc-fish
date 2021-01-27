### Testing SpeciesMix script ####
# Fit Mixtures of Archetype Species # 

# Load libraries ----

#library(devtools)
#devtools::install_github('skiptoniam/ecomix')
library(ecomix)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(reshape2)
library(tidyr)
library(devtools)



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


# 1. Load data ----
df <- read.csv(paste(dt.dir, "2020-06_sw_maxn.meta.cov.csv", sep = '/'))%>%
  mutate_at(vars(sample, family, unique.name, genus, full.name, species, location, status, cluster), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)
names(df)
summary(df)


# 2. Remove sp that are encountered less than 2.5% of the time ----
# as per Foster et al 2015 ----
# To test and because we only have 40 BRUVs so far, going to work with 2.5% which is more than 2 BRUVs
head(df)

# Species from wide to long --
sp.to.remove <- df %>% 
  dplyr::mutate(count = 1) %>% # create new column with count = 1 
  tidyr::pivot_wider(names_from = sample, values_from = count, values_fill = 0) %>% # spread and if NA then = 0
  dplyr::group_by(full.name) %>%
  dplyr::summarise_at(vars(37:75), funs(sum)) %>%
  dplyr::mutate(total.counts=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>% # get occurrence no of BRUVS at which each sp. occurred
  dplyr::ungroup() %>%
  dplyr::arrange(total.counts) %>% # arrange ascending to see if any species found in less than 2 BRUVs
  dplyr::filter(total.counts < 2) %>%
  dplyr::select(full.name) %>%
  glimpse() # in this case, no species found in less than 2 BRUVs

glimpse(sp.to.remove)
names(sp.to.remove)
to.remove <- sp.to.remove$full.name
length(sp.to.remove$full.name)

# Remove species from df -- UP TO HERE -----
df2 <- df %>% dplyr::filter(!full.name %in% to.remove) # remove rare sp
df2 %>% count(full.name)
df2 <- df2 %>% dplyr::filter(full.name != "Unknown spp") # remove unknowns, there are 10 unknowns

df2 <- droplevels(df2)
str(df2)  # 471 obs

names(df2)

# 3. Make covariates in long formate using reshape2 package --
dfl <- melt(df2,
            id.vars = names(df)[c(2:14, 16)],
            measure.vars = names(df)[c(15, 17:41)],
            variable.name = "covariate",
            value.name = "value"
)
head(dfl)
str(dfl)



# 4. Species data into matrix ----
pd <- table_to_species_data(
  dfl,
  site_id = "cluster", # use cluster? or status?
  species_id = "full.name",
  measurement_id = "maxn"
)

pd
class(pd)

# 5. Covariate data into matrix ----
cd <- table_to_species_data(
  dfl,
  site_id = "cluster", # use cluster? or status?
  species_id = "covariate",
  measurement_id = "value"
)

cd
class(cd)


# 6. Make matrix of species and covariates ----
dd <- make_mixture_data(species_data = pd,
                        covariate_data = cd)
dd # I think this is what I need to use for the models


# 7. Try fiting a species mix model ----
# this works but need to figure out what is should be the archetype formula ----

test_model <- species_mix(
  archetype_formula = pd~1+depth+slope,
  species_formula = stats::as.formula(~1),
  all_formula = NULL,
  data=dd,
  nArchetypes = 4,
  family = "negative.binomial",
  #offset = NULL,
  #weights = NULL,
  #bb_weights = NULL,
  #size = NULL, # for presence absence - benthic point data
  #power = NULL, # for tweedie : eg. biomass data
  control = list(), # for tuning the model if needed
  #inits = NULL, # if you have fitted the model previously: use the same values
  #standardise = FALSE, # has been removed in new update it scales
  #titbits = TRUE # could turn this off
)

summary.species_mix(test_model, digits = 4)# this is not working
print(test_model)
coef(test_model)
class(test_model)
test_model$taus
BIC(test_model) # this gives a valie of BIC

sp.boot <- species_mix.bootstrap(
  test_model,
  nboot = 100,
  type = "BayesBoot",
  mc.cores = 2,
  quiet = FALSE
)

sp.var <- vcov(
  test_model,
  sp.boot = NULL,
  method = "BayesBoot",
  nboot = 10,
  mc.cores = 2
)

controls <- as.list(c(1,2,3))
class(controls)

test2 <- species_mix.fit(
  y = pd,
  X = cd,
  W = NULL,
  G = 6,
  S = NULL,
  disty = 'negative.binomial',
  control = controls
)
