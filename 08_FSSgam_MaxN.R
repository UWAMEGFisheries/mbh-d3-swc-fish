require(rstanarm)
require(tidyverse)
require(dplyr)
require(mgcv)
require(FSSgam)
require(MuMIn)
require(doBy)
require(GlobalArchive)
require(googlesheets4)

rm(list=ls())

# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

## Set sub directories----
d.dir <- paste(working.dir,"Data/Tidy",sep="/") 
h.dir <- paste(working.dir, "Data/Habitat/BRUV Style annotation/tidy data",sep="/") 
s.dir <- paste(working.dir,"shapefiles",sep="/")
p.dir <- paste(working.dir,"Plots",sep="/")
m.dir <- paste(working.dir,"Model Out GAM", sep="/")

# Bring in and format the data----
setwd(d.dir)
dir()

# MaxN ----
maxn <-read.csv(paste(name, 'complete.maxn.csv',sep=".")) %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::glimpse()

# Metadata ----
metadata <- read.csv(paste(name, 'checked.metadata.csv',sep=".")) %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::glimpse()

# Bathymetry derivatives ----
bathy <- read.csv(paste(name, 'bathymetry.derivatives.csv',sep=".")) %>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv(paste(name, 'distance.to.ramp.csv',sep=".")) %>%
  dplyr::glimpse()

# Habitat ----
habitat.2020.10 <- read.csv("2020-10_south-west_stereo-BRUVs_BRUV_style.broad.habitat.csv") %>%
  dplyr::select(-c(rowid.x,rowid.y)) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.10)

habitat.2020.06 <- read.csv("2020-06._broad.habitat_BRUV_Style.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count)) %>%
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.06) # 0-100

habitat <-bind_rows(habitat.2020.06, habitat.2020.10) %>%
  tidyr::replace_na(list(broad.Consolidated=0,
                         broad.Macroalgae=0,
                         broad.Seagrasses=0,
                         broad.Sponges=0,
                         broad.Unconsolidated=0,
                         broad.Bryozoa=0,
                         broad.Hydroids=0,
                         broad.Octocoral.Black=0,
                         broad.Stony.corals=0,
                         fov.Facing.Up=0)) %>%
  ga.clean.names() %>%
  dplyr::mutate(broad.reef = broad.bryozoa + broad.consolidated + broad.hydroids + broad.macroalgae + broad.octocoral.black + broad.seagrasses + broad.sponges + broad.stony.corals) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(campaignid,sample,everything()) %>% # re-ordering hab columns 
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness=rowSums(.[,2:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample,total.abundance,species.richness) %>%
  gather(.,"scientific","maxn",2:3) %>%
  dplyr::glimpse()

# Create abundance of all recreational fished species ----
setwd(d.dir)
dir()

# Had to download and use csv as Nectar can't connect to googlesheets :(
master <- read.csv("australia.life.history.csv") %>%
  ga.clean.names()%>%
  dplyr::filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,fishing.type,australian.common.name)%>%
  dplyr::distinct()%>%
  dplyr::glimpse()

unique(master$fishing.type)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))
  
unique(fished.species$scientific)

list.for.tim <- fished.species %>% 
  distinct(scientific, australian.common.name, fishing.type)

write.csv(list.for.tim,"fished.species.list.csv")

# Come back to maybe getting rid of some of these, but for now we continue on
fished.maxn <- fished.species %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,targeted.abundance) %>%
  gather(.,"scientific","maxn",2:2) %>%
  dplyr::glimpse()

# Select species of interest to model ----
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Sparidae Chrysophrys auratus",
                           "Glaucosomatidae Glaucosoma hebraicum",
                           "Labridae Coris auricularis",
                           "Scorpididae Neatypus obliquus",
                           "Labridae Ophthalmolepis lineolatus",
                           "Heterodontidae Heterodontus portusjacksoni",
                           "Monacanthidae Nelusetta ayraud"))%>%
  dplyr::select(sample,scientific,maxn)

unique(species.maxn$scientific)

centroberyx <- maxn %>%
  dplyr::filter(genus%in%c("Centroberyx")) %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(centroberyx=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,centroberyx) %>%
  gather(.,"scientific","maxn",2:2) %>%
  dplyr::glimpse() # there is actually not that many

unique(maxn$scientific)

# Split metadata into Fishing HWY and In/Out dataframes
summary(metadata)

metadata.fh <- metadata %>%
  dplyr::filter(depth<50)

metadata.io <- metadata %>%
  dplyr::filter(latitude<=(-33.96))

plot(metadata$longitude, metadata$latitude)

plot(metadata.fh$longitude, metadata.fhwy$latitude)
plot(metadata.io$longitude, metadata.io$latitude)

## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(fished.maxn, species.maxn, ta.sr, centroberyx)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat)

maxn.fh <- combined.maxn %>%
  semi_join(., metadata.fh)

unique(maxn.fh$sample)

maxn.io <- combined.maxn %>%
  semi_join(., metadata.io)

unique(combined.maxn$scientific)

names(maxn.fh)

# Set predictor variables---
pred.vars=c("depth", "slope", "aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa", "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief" )

dat <- maxn.fh
dat <- combined.maxn

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars], use = "complete.obs"),2)
# reef and sand correlated

# Plot of likely transformations
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# use 
# sd.relief - use non-transformed
# mean.relief - use non-transformed
# sponges - log
# macroalgae - use non-transformed
# distance.to.ramp - use non-transformed
# aspect - use non-transformed
# depth - use non-transformed
# broad.reef - use non-transformed
# tpi - log
# roughness log
# slope log

# remove
# sand - correlated with reef and mean relief
# stony corals - too few
# seagrasses - too few
# octocoral - too few
# hydroids - too few
# consolidated - too few
# bryozoa - too few

# StART OF CHARLOTTES A4
names(maxn.fh)

maxn.fh <- maxn.fh %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))

maxn.io <- maxn.io %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))


# Set predictor variables 
pred.vars=c("mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
            "distance.to.ramp",
            "aspect", "log.tpi","log.roughness","log.slope",
            "depth")

#### FSSgam using lme4 + random site ####
setwd(m.dir)

names(maxn.fh)

# Remove any unused columns from the dataset 
dat <- maxn.fh%>%
  dplyr::select(sample, status, site, planned.or.exploratory, scientific, maxn,
                "mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
                "distance.to.ramp",
                "aspect", "log.tpi","log.roughness","log.slope",
                "depth") %>%
  #filter(scientific=="total.abundance")%>% # Need to figure out how to fix this up
  as.data.frame()

unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use  

resp.vars <- unique.vars.use
factor.vars <- c("status","planned.or.exploratory")
out.all <- list()
var.imp <- list()


for(i in 1:length(resp.vars)){
  
  use.dat=dat[which(dat$scientific==resp.vars[i]),]

Model1 <- uGamm(maxn~s(mean.relief, k=5, bs='cr'),
                family=poisson, random=~(1|site),
                data=use.dat,
                lme4=TRUE)


model.set <- generate.model.set(use.dat=use.dat,
                                test.fit=Model1,
                                pred.vars.cont=pred.vars,
                                pred.vars.fact=factor.vars,
                                smooth.smooth.interactions=FALSE,
                                max.predictors=3,
                                k=5,
                                null.terms= "")



out.list=fit.model.set(model.set,
                       max.models=600,
                       parallel=T)

names(out.list)

out.list$failed.models # examine the list of failed models
mod.table=out.list$mod.data.out  # look at the model selection table
mod.table=mod.table[order(mod.table$AICc),]
mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
out.i=head(mod.table,10)
out.i=mod.table[which(mod.table$delta.AICc<=3),]
out.all=c(out.all,list(out.i))
var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) 

# Model fits and importance---
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)

write.csv(all.mod.fits[,-2],file=paste(name,resp.vars[i],"lme4.random.all.mod.fits.nofov.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,resp.vars[i],"lme4.all.var.imp.nofov.csv",sep="_"))

}

# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.8),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


#### Model 1 - ramp effect ####
# Predict from the model 

effect.dat <- expand.grid(cube.Aspect=seq(min(legal.dat$cube.Aspect),max(legal.dat$cube.Aspect),length.out = 20),
                          distance.to.ramp=seq(min(legal.dat$distance.to.ramp),max(legal.dat$distance.to.ramp),length.out = 20),
                          bathymetry=seq(min(legal.dat$bathymetry),max(legal.dat$bathymetry), length.out=20))
#site=lme4.site$gam$model$site)%>%

use.dat <- legal.dat%>%
  dplyr::select(response, distance.to.ramp, cube.Aspect, sqrt.slope, log.roughness, status, FlowDir,
                bathymetry, site)
factor.vars <- c("status")


Model.1 <- uGamm(response~s(bathymetry,k=5,bs='cr')+ s(distance.to.ramp, k=5, bs='cr')
                 + s(cube.Aspect,k=5,bs='cr'),random=~(1|site), 
                 family=poisson(), data=use.dat, lme4=TRUE)



# now fit with shrinkage - to generate predicted values
Model.1 <- uGamm(response~s(bathymetry,k=5,bs='cs')+ s(distance.to.ramp, k=5, bs='cs') 
                 + s(cube.Aspect,k=5,bs='cs'), random=~(1|site), 
                 family=poisson(), data=use.dat, lme4=TRUE)

stangam.s  <- stan_gamm4(response~s(bathymetry, k = 5, bs = 'cs')+ s(distance.to.ramp, k=5, bs='cs') 
                         + s(cube.Aspect,k=5,bs='cr'), random=~(1|site), adapt_delta = 0.99,
                         data=use.dat, chains=3, cores=3, iter=41000, warmup=40000, thin=3,
                         family=poisson())

# calculate frequentist effects - link function is log 
predicted <- predict.gam(Model.1$gam, newdata=effect.dat, type='response', se.fit=T)
effect.dat <- cbind(effect.dat, predicted)


# bathy effect 
bathy.P <- effect.dat%>%
  dplyr::group_by(bathymetry)%>%
  summarise_at(vars(predicted), list(mean))

bathy.E <- diff(range(bathy.P$predicted)) 
bathy.E

# plot bathy effect 
predicts.legal.bathy = effect.dat%>%data.frame(predicted)%>%
  group_by(bathymetry)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

ggmod.legal.bathy<- ggplot() +
  ylab("Predicted Abundance of Legal Sized Fish")+
  xlab('Bathymetry (m)')+
  #   ggtitle(substitute(italic(name)))+
  #scale_color_manual(labels = c("Fished", "No-take"),values=c("royalblue2", "slategrey"))+
  geom_jitter(width = 0.25,height = 0)+
  geom_point(data=legal.dat,aes(x=bathymetry,y=response), colour="lightblue", alpha=0.75, size=2,show.legend=F)+
  geom_line(data=predicts.legal.bathy,aes(x=bathymetry,y=response), colour='darkblue', alpha=0.75)+
  geom_line(data=predicts.legal.bathy,aes(x=bathymetry,y=response - se.fit), colour='darkblue', linetype="dashed",alpha=0.75)+
  geom_line(data=predicts.legal.bathy,aes(x=bathymetry,y=response + se.fit), colour='darkblue', linetype="dashed",alpha=0.75)+
  geom_rug(data=legal.dat, aes(x=bathymetry),colour="slategrey")+
  theme_classic()+
  xlim(-180,-52)+
  Theme1
#annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
ggmod.legal.bathy


# aspect effect 
aspect.P <- effect.dat%>%
  group_by(cube.Aspect)%>%
  summarise_at(vars(predicted), list(mean))

aspect.E <- diff(range(aspect.P$predicted))
aspect.E

predicts.legal.aspect = effect.dat%>%data.frame(predicted)%>%
  group_by(cube.Aspect)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

ggmod.legal.aspect<- ggplot() +
  ylab("Predicted Abundance of Legal Sized Fish")+
  xlab('Cubed Aspect (degrees)')+
  #   ggtitle(substitute(italic(name)))+
  #scale_color_manual(labels = c("Fished", "No-take"),values=c("royalblue2", "slategrey"))+
  geom_jitter(width = 0.25,height = 0)+
  geom_point(data=legal.dat,aes(x=cube.Aspect,y=response), colour="lightgreen", alpha=0.75, size=2,show.legend=F)+
  geom_line(data=predicts.legal.aspect,aes(x=cube.Aspect,y=response), colour='darkgreen', alpha=0.75)+
  geom_line(data=predicts.legal.aspect,aes(x=cube.Aspect,y=response - se.fit), colour='darkgreen', linetype="dashed",alpha=0.75)+
  geom_line(data=predicts.legal.aspect,aes(x=cube.Aspect,y=response + se.fit), colour='darkgreen', linetype="dashed",alpha=0.75)+
  geom_rug(data=legal.dat, aes(x=cube.Aspect),colour="slategrey")+
  theme_classic()+
  Theme1
#annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
ggmod.legal.aspect

# ramp effect 
ramp.P <- effect.dat%>%
  group_by(distance.to.ramp)%>%
  summarise_at(vars(predicted), list(mean))

ramp.E <- diff(range(ramp.P$predicted))
ramp.E

predicts.legal.ramp = effect.dat%>%data.frame(predicted)%>%
  group_by(distance.to.ramp)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

ggmod.legal.ramp<- ggplot() +
  ylab("Predicted Abundance of Legal Sized Fish")+
  xlab('Distance to Boat Ramp (km)')+
  #   ggtitle(substitute(italic(name)))+
  #scale_color_manual(labels = c("Fished", "No-take"),values=c("royalblue2", "slategrey"))+
  geom_jitter(width = 0.25,height = 0)+
  geom_point(data=legal.dat,aes(x=distance.to.ramp,y=response), colour="lightcoral", alpha=0.75, size=2,show.legend=F)+
  geom_line(data=predicts.legal.ramp,aes(x=distance.to.ramp,y=response), colour='darkred', alpha=0.75)+
  geom_line(data=predicts.legal.ramp,aes(x=distance.to.ramp,y=response - se.fit), colour='darkred', linetype="dashed",alpha=0.75)+
  geom_line(data=predicts.legal.ramp,aes(x=distance.to.ramp,y=response + se.fit), colour='darkred', linetype="dashed",alpha=0.75)+
  geom_rug(data=legal.dat, aes(x=distance.to.ramp),colour="slategrey")+
  theme_classic()+
  Theme1
#annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
ggmod.legal.ramp

# use the bayesian models to calculate a posterior distribution of the effect size
effect.dat.sim <- effect.dat
gg <- t(posterior_predict(stangam.s, effect.dat.sim, re.form=NA)) # Returning in fish
#colnames(gg) <- paste("sim",1:ncol(gg),sep="_")
gg.dat <- cbind(effect.dat.sim,gg)


gg.dat.long <- gg.dat%>%
  gather(sim, predicted.sim, 4:1005)

gg.dat.long$sim <- as.factor(gg.dat.long$sim)
str(gg.dat.long)

# bathy effect 
bathy.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("Bathymetry (m)")
colnames(bathy.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long
  bathy.sim.2 <- dplyr::filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- dplyr::group_by(bathy.sim.2, bathymetry)
  bathy.sim.4 <- dplyr::summarise_at(bathy.sim.3, vars(predicted.sim), list(mean))
  print(bathy.sim.5 <- diff(range(bathy.sim.4$predicted.sim)))
  bathy.sim <- rbind(bathy.sim, bathy.sim.5)
  
}

bathy.sim

write.csv(bathy.sim, "bayesian.bathy.predictions.csv")

# aspect effect 
aspect.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("Cubed Aspect (degrees)")
colnames(aspect.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean))
  print(aspect.sim.5 <- diff(range(aspect.sim.4$predicted.sim)))
  aspect.sim <- rbind(aspect.sim, aspect.sim.5)
  
}

aspect.sim

write.csv(aspect.sim, "bayesian.aspect.predictions.csv")

# ramp effect 
ramp.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("Distance to Boat Ramp (km)")
colnames(ramp.sim) <- x

for(i in 1:1002){
  ramp.sim.1 <- gg.dat.long
  ramp.sim.2 <- filter(ramp.sim.1, sim==i)
  ramp.sim.3 <- group_by(ramp.sim.2, distance.to.ramp)
  ramp.sim.4 <- summarise_at(ramp.sim.3, vars(predicted.sim), list(mean))
  print(ramp.sim.5 <- diff(range(ramp.sim.4$predicted.sim)))
  ramp.sim <- rbind(ramp.sim, ramp.sim.5)
  
}

ramp.sim

write.csv(ramp.sim, "bayesian.ramp.predictions.csv")

setwd(m.dir)
# Plot effect sizes 
setwd(b.dir)
ramp.sim <- read.csv("bayesian.ramp.predictions.csv")
ramp.sim <- ramp.sim[,-1]
ramp.sim <- ramp.sim[-1,]
ramp.sim <- as.data.frame(ramp.sim)
x <- c("Distance to Boat Ramp (km)")
colnames(ramp.sim) <- x

bathy.sim <- read.csv("bayesian.bathy.predictions.csv")
bathy.sim <- bathy.sim[-1,]
bathy.sim <- bathy.sim[,-1]
bathy.sim <- as.data.frame(bathy.sim)
x <- c("Bathymetry (m)")
colnames(bathy.sim) <- x

aspect.sim <- read.csv("bayesian.aspect.predictions.csv")
aspect.sim <- aspect.sim[-1,]
aspect.sim <- aspect.sim[,-1]
aspect.sim <- as.data.frame(aspect.sim)
x <- c("Cube Aspect (Degrees)")
colnames(aspect.sim) <- x

full.data <- as.data.frame(cbind(ramp.sim, aspect.sim, bathy.sim))

full.data.long <- full.data%>%
  gather(variable, effect.size, 1:3)

colours <- c('#619CFF', '#00BA38', '#F8766D') #9590FF

colours <- c('#619CFF', NA, NA) 
effect.plot <- ggplot(full.data.long, aes(x = effect.size, fill = variable, colour = variable)) + geom_density(alpha = 0.5) +
  scale_fill_manual(values=colours)+
  scale_colour_manual(values=colours)+
  geom_vline(xintercept = 1.859307, color = "steelblue", size=0.75)+
  #geom_vline(xintercept = 1.418643, color = "tomato3", size=0.75)+
  #geom_vline(xintercept = 1.175649, color = "springgreen4", size=0.7)+
  xlim(-1,8)+
  theme_classic()+
  labs(y="Density", x="Effect Size")+
  Theme1
effect.plot

## Make prediction plots

# bathy effect 
bathy.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.mean")
colnames(bathy.means) <- x

for(i in unique(gg.dat.long$bathymetry)){
  bathy.sim.1 <- gg.dat.long
  bathy.sim.2 <- dplyr::filter(bathy.sim.1, bathymetry==i)
  print(bathy.sim.3 <- dplyr::summarise_at(bathy.sim.2, vars(predicted.sim), list(mean)))
  bathy.means <- rbind(bathy.means, bathy.sim.3$predicted.sim)
}

bathy.means <- bathy.means[-1,]
depths <- unique(gg.dat$bathymetry)
bathy.means <- as.data.frame(cbind(bathy.means,depths))

# Get means for each value of bathymetry for each simulation 
bathy.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("bathy.mean.sim")
colnames(bathy.mean.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  print(bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean)))
  bathy.mean.sim <- cbind(bathy.mean.sim, bathy.sim.4)
}

bathy.mean.full <- bathy.mean.sim[ ,seq(3, ncol(bathy.mean.sim), 2)]
bathy.mean.full$bathymetry <- bathy.mean.sim[,2]

bathy.mean.long <- bathy.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results bathy
predict.plot.bathy <- ggplot() +
  geom_line(data=bathy.mean.long, aes(x = bathymetry, y = mean, group=sim), colour='lightblue'
            , alpha=0.2)+
  geom_line(data=bathy.means, aes(x = depths, y = bathy.means), colour='darkblue') +
  labs(y="Predicted Abundance Legal of Sized Fish", x="Bathymetry (m)")+
  theme_classic()+
  xlim(-180,-51)+
  Theme1
predict.plot.bathy

# aspect effect 
aspect.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("aspect.mean")
colnames(aspect.means) <- x

for(i in unique(gg.dat.long$cube.Aspect)){
  aspect.sim.1 <- gg.dat.long
  aspect.sim.2 <- dplyr::filter(aspect.sim.1, cube.Aspect==i)
  print(aspect.sim.3 <- dplyr::summarise_at(aspect.sim.2, vars(predicted.sim), list(mean)))
  aspect.means <- rbind(aspect.means, aspect.sim.3$predicted.sim)
}

aspect.means <- aspect.means[-1,]
aspect <- unique(gg.dat$cube.Aspect)
aspect.means <- as.data.frame(cbind(aspect.means,aspect))

# Get means for each value of aspect for each simulation 
aspect.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("aspect.mean.sim")
colnames(aspect.mean.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  print(aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean)))
  aspect.mean.sim <- cbind(aspect.mean.sim, aspect.sim.4)
}

aspect.mean.full <- aspect.mean.sim[ ,seq(3, ncol(aspect.mean.sim), 2)]
aspect.mean.full$cube.Aspect <- aspect.mean.sim[,2]

aspect.mean.long <- aspect.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results aspect
predict.plot.aspect <- ggplot() +
  geom_line(data=aspect.mean.long, aes(x = cube.Aspect, y = mean, group=sim), colour='lightgreen',
            alpha=0.2) +
  geom_line(data=aspect.means, aes(x = aspect, y = aspect.means), colour='darkgreen') +
  labs(y="Predicted Abundance of Legal Sized Fish", x="Cube Aspect (degrees)")+
  theme_classic()+
  Theme1
predict.plot.aspect

# ramp effect 
ramp.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("ramp.mean")
colnames(ramp.means) <- x

for(i in unique(gg.dat.long$distance.to.ramp)){
  ramp.sim.1 <- gg.dat.long
  ramp.sim.2 <- dplyr::filter(ramp.sim.1, distance.to.ramp==i)
  print(ramp.sim.3 <- dplyr::summarise_at(ramp.sim.2, vars(predicted.sim), list(mean)))
  ramp.means <- rbind(ramp.means, ramp.sim.3$predicted.sim)
}

ramp.means <- ramp.means[-1,]
ramps <- unique(gg.dat$distance.to.ramp)
ramp.means <- as.data.frame(cbind(ramp.means,ramps))

# Get means for each value of ramp for each simulation 
ramp.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("ramp.mean.sim")
colnames(ramp.mean.sim) <- x

for(i in 1:1002){
  ramp.sim.1 <- gg.dat.long
  ramp.sim.2 <- filter(ramp.sim.1, sim==i)
  ramp.sim.3 <- group_by(ramp.sim.2, distance.to.ramp)
  print(ramp.sim.4 <- summarise_at(ramp.sim.3, vars(predicted.sim), list(mean)))
  ramp.mean.sim <- cbind(ramp.mean.sim, ramp.sim.4)
}

ramp.mean.full <- ramp.mean.sim[ ,seq(3, ncol(ramp.mean.sim), 2)]
ramp.mean.full$distance.to.ramp <- ramp.mean.sim[,2]

ramp.mean.long <- ramp.mean.full%>%
  gather(sim, mean, 1:1002)

# Plot the results ramps
predict.plot.ramps <- ggplot() +
  geom_line(data=ramp.mean.long, aes(x = distance.to.ramp, y = mean, group=sim), colour='lightcoral',
            alpha=0.2) +
  geom_line(data=ramp.means, aes(x = ramps, y = ramp.means), colour='darkred') +
  labs(y="Predicted Abundance Legal of Sized Fish", x="Distance to Boat Ramp (km)")+
  theme_classic()+
  Theme1
predict.plot.ramps

#### Model 2 - status model ####
# Predict from the model

use.dat <- legal.dat%>%
  dplyr::select(response, distance.to.ramp, cube.Aspect, sqrt.slope, log.roughness, status, FlowDir,
                bathymetry, site)
factor.vars <- c("status")


Model.2 <- uGamm(response~s(bathymetry,k=5,bs='cr')+ factor(status)
                 + s(cube.Aspect,k=5,bs='cr'),random=~(1|site), 
                 family=poisson(), data=use.dat, lme4=TRUE)



# now fit with shrinkage - to generate predicted values
Model.2 <- uGamm(response~s(bathymetry,k=5,bs='cs')+ factor(status)
                 + s(cube.Aspect,k=5,bs='cs'), random=~(1|site), 
                 family=poisson(), data=use.dat, lme4=TRUE)

stangam.s.2  <- stan_gamm4(response~s(bathymetry, k = 5, bs = "cs")+ status
                           + s(cube.Aspect,k=5,bs='cs'), random=~(1|site), adapt_delta = 0.99,
                           data=use.dat, chains=3, cores=3, iter=41000, warmup=40000, thin=3,
                           family=poisson())


effect.dat.2 <- expand.grid(cube.Aspect=seq(min(legal.dat$cube.Aspect),max(legal.dat$cube.Aspect),length.out = 20),
                            bathymetry=seq(min(legal.dat$bathymetry),max(legal.dat$bathymetry), length.out = 20),
                            status=unique(legal.dat$status))



# calcualte frequentist effects
predicted <- predict.gam(Model.2$gam, newdata=effect.dat.2, type='response')
effect.dat.2 <- cbind(effect.dat.2, predicted)

# bathy effect 
bathy.P.2 <- effect.dat.2%>%
  group_by(bathymetry)%>%
  summarise_at(vars(predicted), list(mean))

bathy.E.2 <- diff(range(bathy.P.2$predicted))
bathy.E.2

# Plot predicted
predicted <- predict.gam(Model.2$gam, newdata=effect.dat.2, type='response', se.fit=T)
effect.dat.2 <- cbind(effect.dat.2, predicted)

predicts.legal.bathy = effect.dat.2%>%data.frame(predicted)%>%
  group_by(bathymetry)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

ggmod.legal.bathy<- ggplot() +
  ylab("Predicted Abundance of Legal Sized Fish")+
  xlab('Bathymetry (m)')+
  #   ggtitle(substitute(italic(name)))+
  #scale_color_manual(labels = c("Fished", "No-take"),values=c("royalblue2", "slategrey"))+
  geom_jitter(width = 0.25,height = 0)+
  geom_point(data=legal.dat,aes(x=bathymetry,y=response), colour="lightblue", alpha=0.75, size=2,show.legend=F)+
  geom_line(data=predicts.legal.bathy,aes(x=bathymetry,y=response), colour='darkblue', alpha=0.75)+
  geom_line(data=predicts.legal.bathy,aes(x=bathymetry,y=response - se.fit), colour='darkblue', linetype="dashed",alpha=0.75)+
  geom_line(data=predicts.legal.bathy,aes(x=bathymetry,y=response + se.fit), colour='darkblue', linetype="dashed",alpha=0.75)+
  geom_rug(data=legal.dat, aes(x=bathymetry),colour="slategrey")+
  theme_classic()+
  Theme1
#annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
ggmod.legal.bathy


# aspect effect 
predicted <- predict.gam(Model.2$gam, newdata=effect.dat.2, type='response')
effect.dat.2 <- cbind(effect.dat.2, predicted)

aspect.P.2 <- effect.dat.2%>%
  group_by(cube.Aspect)%>%
  summarise_at(vars(predicted), list(mean))

aspect.E.2 <- diff(range(aspect.P.2$predicted))
aspect.E.2

# Plot it 
predicts.legal.aspect = effect.dat.2%>%data.frame(predicted)%>%
  group_by(cube.Aspect)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

ggmod.legal.aspect<- ggplot() +
  ylab("Predicted Abundance of Legal Sized Fish")+
  xlab('Aspect (cubed)')+
  #   ggtitle(substitute(italic(name)))+
  #scale_color_manual(labels = c("Fished", "No-take"),values=c("royalblue2", "slategrey"))+
  geom_jitter(width = 0.25,height = 0)+
  geom_point(data=legal.dat,aes(x=cube.Aspect,y=response), colour="lightgreen", alpha=0.75, size=2,show.legend=F)+
  geom_line(data=predicts.legal.aspect,aes(x=cube.Aspect,y=response), colour='darkgreen', alpha=0.75)+
  geom_line(data=predicts.legal.aspect,aes(x=cube.Aspect,y=response - se.fit), colour='darkgreen', linetype="dashed",alpha=0.75)+
  geom_line(data=predicts.legal.aspect,aes(x=cube.Aspect,y=response + se.fit), colour='darkgreen', linetype="dashed",alpha=0.75)+
  geom_rug(data=legal.dat, aes(x=cube.Aspect),colour="slategrey")+
  theme_classic()+
  Theme1
#annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
ggmod.legal.aspect

# status effect 
status.P.2 <- effect.dat.2%>%
  group_by(status)%>%
  summarise_at(vars(predicted), list(mean))

status.E.2 <- diff(range(status.P.2$predicted))
status.E.2

# Plot it 
predicts.legal.status = effect.dat.2%>%data.frame(predicted)%>%
  group_by(status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

status.order <- c("F", "NT")
ggmod.gam.status<- ggplot(aes(x=status,y=response,colour=status), data=legal.dat) +
  ylab("Predicted Abundance of Legal Sized Fish")+
  xlab('Status')+
  #   ggtitle(substitute(italic(name)))+
  #scale_fill_manual(labels = c("Fished", "No-take"),values=c("royalblue2", "slategrey"))+
  #scale_color_manual(labels = c("Fished", "No-take"),values=c("royalblue2", "slategrey"))+
  scale_x_discrete(limits = status.order)+
  #geom_bar(stat = "identity")+
  geom_errorbar(data=predicts.legal.status,aes(ymin = response-se.fit,ymax = response+se.fit),colour="lightcoral",width = 0.5) +
  theme_classic()+
  Theme1
ggmod.gam.status

# use the bayesian models to calculate a posterior distribution of the effect size
effect.dat.2 <- expand.grid(cube.Aspect=seq(min(legal.dat$cube.Aspect),max(legal.dat$cube.Aspect),length.out = 20),
                            bathymetry=seq(min(legal.dat$bathymetry),max(legal.dat$bathymetry), length.out = 20),
                            status=unique(legal.dat$status))
effect.dat.sim.2 <- effect.dat.2
gg <- t(posterior_predict(stangam.s.2, effect.dat.sim.2, re.form=NA))
#colnames(gg) <- paste("sim",1:ncol(gg),sep="_")
gg.dat <- cbind(effect.dat.sim.2,gg)

head(gg.dat)

gg.dat.long.2 <- gg.dat%>%
  gather(sim, predicted.sim, 4:1005)

gg.dat.long.2$sim <- as.factor(gg.dat.long.2$sim)
head(gg.dat.long)

str(gg.dat.long.2)
# bathy effect 
bathy.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.mean")
colnames(bathy.sim) <- x


for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.2
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean))
  print(bathy.sim.5 <- diff(range(bathy.sim.4$predicted.sim)))
  bathy.sim <- rbind(bathy.sim, bathy.sim.5)
  
}

bathy.sim

write.csv(bathy.sim, "bayesian.bathy.predictions.2.csv")

# aspect effect 
aspect.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("aspect.mean")
colnames(aspect.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long.2
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean))
  print(aspect.sim.5 <- diff(range(aspect.sim.4$predicted.sim)))
  aspect.sim <- rbind(aspect.sim, aspect.sim.5)
  
}

aspect.sim

write.csv(aspect.sim, "bayesian.aspect.predictions.2.csv")

# status effect 
status.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("status.mean")
colnames(status.sim) <- x

for(i in 1:1002){
  status.sim.1 <- gg.dat.long.2
  status.sim.2 <- filter(status.sim.1, sim==i)
  status.sim.3 <- group_by(status.sim.2, status)
  status.sim.4 <- summarise_at(status.sim.3, vars(predicted.sim), list(mean))
  print(status.sim.5 <- diff(range(status.sim.4$predicted.sim)))
  status.sim <- rbind(status.sim, status.sim.5)
  
}

status.sim

write.csv(status.sim, "bayesian.status.predictions.2.csv")

## Create density plot
setwd(d.dir)
status.sim <- read.csv("bayesian.status.predictions.2.csv")
status.sim <- as.data.frame(status.sim[,-1])
status.sim <- as.data.frame(status.sim[-1,])
status.sim <- as.data.frame(status.sim)
x <- 'Status'
colnames(status.sim) <- x

bathy.sim <- read.csv("bayesian.bathy.predictions.2.csv")
bathy.sim <- as.data.frame(bathy.sim[,-1])
bathy.sim <- as.data.frame(bathy.sim[-1,])
bathy.sim <- as.data.frame(bathy.sim)
x <- 'Bathymetry (m)'
colnames(bathy.sim) <- x

aspect.sim <- read.csv("bayesian.aspect.predictions.2.csv")
aspect.sim <- as.data.frame(aspect.sim[,-1])
aspect.sim <- as.data.frame(aspect.sim[-1,])
aspect.sim <- as.data.frame(aspect.sim)
x <- 'Cube Aspect (degrees)'
colnames(aspect.sim) <- x


status.sim <- status.sim[-1,]
bathy.sim <- bathy.sim[-1,]
aspect.sim <- aspect.sim[-1,]

full.data <- as.data.frame(cbind(bathy.sim, aspect.sim, status.sim))

full.data.long <- full.data%>%
  gather(variable, predicted, 1:3)

colours <- c('#619CFF', '#00BA38', '#F8766D') #9590FF
colours <- c('#619CFF', NA, NA) 
effect.plot <- ggplot(full.data.long, aes(x = predicted, fill=variable, colour=variable))+ 
  geom_density(alpha = 0.5)+
  scale_color_manual(values=colours)+
  scale_fill_manual(values=colours)+
  geom_vline(xintercept = 2.348731, color = "steelblue", size=0.75)+
  #geom_vline(xintercept = 1.236803, color = "tomato3", size=0.75)+
  #geom_vline(xintercept = 1.544218, color = "springgreen4", size=0.7)+
  labs(y="Density", x="Effect Size")+
  xlim(-1,6)+
  theme_classic()+
  Theme1
effect.plot

## Plot the predicted results for each of the variables 
# Bathy effect
bathy.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.means")
colnames(bathy.means) <- x

for(i in unique(gg.dat.long.2$bathymetry)){
  bathy.sim.1 <- gg.dat.long.2
  bathy.sim.2 <- dplyr::filter(bathy.sim.1, bathymetry==i)
  print(bathy.sim.3 <- dplyr::summarise_at(bathy.sim.2, vars(predicted.sim), list(mean)))
  bathy.means <- rbind(bathy.means, bathy.sim.3$predicted.sim)
}

bathy.means <- bathy.means[-1,]
depths <- unique(gg.dat$bathymetry)
bathy.means <- as.data.frame(cbind(bathy.means,depths))

# Get means for each value of bathymetry for each simulation 
bathy.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("bathy.mean.sim")
colnames(bathy.mean.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.2
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  print(bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean)))
  bathy.mean.sim <- cbind(bathy.mean.sim, bathy.sim.4)
}

bathy.mean.full <- bathy.mean.sim[ ,seq(3, ncol(bathy.mean.sim), 2)]
bathy.mean.full$bathymetry <- bathy.mean.sim[,2]

bathy.mean.long <- bathy.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results bathy
predict.plot.bathy <- ggplot() +
  geom_line(data=bathy.mean.long, aes(x = bathymetry, y = mean, group=sim), colour='lightblue'
            , alpha=0.2)+
  geom_line(data=bathy.means, aes(x = depths, y = bathy.means), colour='darkblue') +
  labs(y="Predicted Abundance Legal of Sized Fish", x="Bathymetry (m)")+
  theme_classic()+
  xlim(-180,-51)+
  Theme1
predict.plot.bathy

# aspect effect 
aspect.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("aspect.mean")
colnames(aspect.means) <- x

for(i in unique(gg.dat.long.2$cube.Aspect)){
  aspect.sim.1 <- gg.dat.long.2
  aspect.sim.2 <- dplyr::filter(aspect.sim.1, cube.Aspect==i)
  print(aspect.sim.3 <- dplyr::summarise_at(aspect.sim.2, vars(predicted.sim), list(mean)))
  aspect.means <- rbind(aspect.means, aspect.sim.3$predicted.sim)
}

aspect.means <- aspect.means[-1,]
aspect <- unique(gg.dat$cube.Aspect)
aspect.means <- as.data.frame(cbind(aspect.means,aspect))

# Get means for each value of aspect for each simulation 
aspect.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("aspect.mean.sim")
colnames(aspect.mean.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long.2
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  print(aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean)))
  aspect.mean.sim <- cbind(aspect.mean.sim, aspect.sim.4)
}

aspect.mean.full <- aspect.mean.sim[ ,seq(3, ncol(aspect.mean.sim), 2)]
aspect.mean.full$cube.Aspect <- aspect.mean.sim[,2]

aspect.mean.long <- aspect.mean.full%>%
  gather(sim, mean, 1:1002)

# Plot the results aspect
predict.plot.aspect <- ggplot() +
  geom_line(data=aspect.mean.long, aes(x = cube.Aspect, y = mean, group=sim), colour='lightgreen',
            alpha=0.2) +
  geom_line(data=aspect.means, aes(x = aspect, y = aspect.means), colour='darkgreen') +
  labs(y="Predicted Abundance Legal of Sized Fish", x="Cube Aspect (degrees)")+
  theme_classic()+
  Theme1
predict.plot.aspect

# status effect 
status.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("status.mean")
colnames(status.means) <- x

for(i in unique(gg.dat.long.2$status)){
  status.sim.1 <- gg.dat.long.2
  status.sim.2 <- dplyr::filter(status.sim.1, status==i)
  print(status.sim.3 <- dplyr::summarise_at(status.sim.2, vars(predicted.sim), list(mean)))
  status.means <- rbind(status.means, status.sim.3$predicted.sim)
}

status.means <- status.means[-1,]
status <- unique(gg.dat$status)
status.means <- as.data.frame(cbind(status.means,status))

# Get means for each value of aspect for each simulation 
status.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("status.mean.sim")
colnames(status.mean.sim) <- x

for(i in 1:1002){
  status.sim.1 <- gg.dat.long.2
  status.sim.2 <- filter(status.sim.1, sim==i)
  status.sim.3 <- group_by(status.sim.2, status)
  print(status.sim.4 <- summarise_at(status.sim.3, vars(predicted.sim), list(mean)))
  status.mean.sim <- cbind(status.mean.sim, status.sim.4)
}

status.mean.full <- status.mean.sim[ ,seq(3, ncol(status.mean.sim), 2)]
status.mean.full$status <- status.mean.sim[,2]

status.mean.long <- status.mean.full%>%
  gather(sim, mean, 1:1002)

# Plot the results status
jitter <- position_jitter(width = 0.06, height = 0.1)
predict.plot.status <- ggplot() +
  geom_point(data=status.mean.long,  position = jitter, aes(x = status, y = mean, group=sim), colour='lightcoral',
             alpha=0.2) +
  geom_boxplot(data=status.mean.long, aes(x=status, y=mean), colour='black', fill=NA, width=0.14)+
  geom_point(data=status.means, aes(x = status, y = status.means), colour='darkred', shape=17, size=3) +
  labs(y="Predicted Abundance Legal Sized Fish", x="Status")+
  #geom_jitter()+
  theme_classic()+
  Theme1
predict.plot.status

###### Model 3 - Just ramp ######
use.dat <- legal.dat%>%
  dplyr::select(response, distance.to.ramp, cube.Aspect, sqrt.slope, log.roughness, status, FlowDir,
                bathymetry, site)
factor.vars <- c("status")


Model.3 <- uGamm(response~s(bathymetry,k=5,bs='cs')+ s(distance.to.ramp, k=5, bs='cs'), 
                 random=~(1|site), 
                 family=poisson(), data=use.dat, lme4=TRUE)

stangam.s.3  <- stan_gamm4(response~s(bathymetry, k = 5, bs = "cs") + s(distance.to.ramp, k=5, bs='cs'), 
                           random=~(1|site), adapt_delta = 0.99,
                           data=use.dat, chains=3, cores=3, iter=41000, warmup=40000, thin=3,
                           family=poisson())

effect.dat.3 <- expand.grid(distance.to.ramp=seq(min(legal.dat$distance.to.ramp),max(legal.dat$distance.to.ramp),length.out = 20),
                            bathymetry=seq(min(legal.dat$bathymetry),max(legal.dat$bathymetry), length.out=20))

# calcualte frequentist effects
predicted <- predict.gam(Model.3$gam, newdata=effect.dat.3, type='response')
effect.dat.3 <- cbind(effect.dat.3, predicted)

# bathy effect 
bathy.P.3 <- effect.dat.3%>%
  group_by(bathymetry)%>%
  summarise_at(vars(predicted), list(mean))

bathy.E.3 <- diff(range(bathy.P.3$predicted))
bathy.E.3

# ramp effect 
ramp.P.3 <- effect.dat.3%>%
  group_by(distance.to.ramp)%>%
  summarise_at(vars(predicted), list(mean))

ramp.E.3 <- diff(range(ramp.P.3$predicted))
ramp.E.3

# use the bayesian models to calculate a posterior distribution of the effect size
effect.dat.sim.3 <- effect.dat.3
gg.3 <- t(posterior_predict(stangam.s.3, effect.dat.sim.3, re.form=NA)) # Returning in fish
#colnames(gg) <- paste("sim",1:ncol(gg),sep="_")
gg.dat.3 <- cbind(effect.dat.sim.3,gg.3)


gg.dat.long.3 <- gg.dat.3%>%
  gather(sim, predicted.sim, 4:1005)

gg.dat.long.3$sim <- as.factor(gg.dat.long.3$sim)

# bathy effect 
bathy.3.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.mean")
colnames(bathy.3.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.3
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean))
  print(bathy.sim.5 <- diff(range(bathy.sim.4$predicted.sim)))
  bathy.3.sim <- rbind(bathy.3.sim, bathy.sim.5)
  
}

bathy.3.sim

write.csv(bathy.3.sim, "bayesian.bathy.predictions.3.csv")

# ramp effect 
ramp.3.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("ramp.mean")
colnames(ramp.3.sim) <- x

for(i in 1:1002){
  ramp.sim.1 <- gg.dat.long.3
  ramp.sim.2 <- filter(ramp.sim.1, sim==i)
  ramp.sim.3 <- group_by(ramp.sim.2, distance.to.ramp)
  ramp.sim.4 <- summarise_at(ramp.sim.3, vars(predicted.sim), list(mean))
  print(rampsim.5 <- diff(range(ramp.sim.4$predicted.sim)))
  ramp.3.sim <- rbind(ramp.3.sim, ramp.sim.5)
  
}

ramp.3.sim

write.csv(ramp.3.sim, "bayesian.ramp.predictions.3.csv")

## Plot effect sizes 
bathy.3.sim <- bathy.3.sim[-1,]
x <- 'Bathymetry'
bathy.3.sim <- as.data.frame(bathy.3.sim)
colnames(bathy.3.sim) <- x
ramp.3.sim <- ramp.3.sim[-1,]
ramp.3.sim <- as.data.frame(ramp.3.sim)
x <- "Distance to Boat Ramp"
colnames(ramp.3.sim) <- x

full.data <- as.data.frame(cbind(ramp.3.sim, bathy.3.sim))

full.data.long <- full.data%>%
  gather(variable, predicted, 1:2)

effect.plot <- ggplot(full.data.long, aes(x = predicted, fill = variable, color=variable)) + geom_density(alpha = 0.5)+
  xlim(0,6)+
  geom_vline(xintercept = 1.966352, color = "darkblue", size=0.75)+
  geom_vline(xintercept = 2.439751, color = "darkred", size=0.7)+
  labs(y="Density", x="Effect Size")+
  Theme1
effect.plot

## Plot predicted values 
# Bathy effect
bathy.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.means")
colnames(bathy.means) <- x

for(i in unique(gg.dat.long.3$bathymetry)){
  bathy.sim.1 <- gg.dat.long.3
  bathy.sim.2 <- dplyr::filter(bathy.sim.1, bathymetry==i)
  print(bathy.sim.3 <- dplyr::summarise_at(bathy.sim.2, vars(predicted.sim), list(mean)))
  bathy.means <- rbind(bathy.means, bathy.sim.3$predicted.sim)
}

bathy.means <- bathy.means[-1,]
depths <- unique(gg.dat.3$bathymetry)
bathy.means <- as.data.frame(cbind(bathy.means,depths))

# Get means for each value of bathymetry for each simulation 
bathy.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("bathy.mean.sim")
colnames(bathy.mean.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.3
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  print(bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean)))
  bathy.mean.sim <- cbind(bathy.mean.sim, bathy.sim.4)
}

bathy.mean.full <- bathy.mean.sim[ ,seq(3, ncol(bathy.mean.sim), 2)]
bathy.mean.full$bathymetry <- bathy.mean.sim[,2]

bathy.mean.long <- bathy.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results bathy
predict.plot.bathy <- ggplot() +
  geom_line(data=bathy.mean.long, aes(x = bathymetry, y = mean, group=sim), colour='lightblue'
            , alpha=0.2)+
  geom_line(data=bathy.means, aes(x = depths, y = bathy.means), colour='darkblue') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Bathymetry")+
  Theme1
predict.plot.bathy

# Ramp effect
ramp.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("ramp.means")
colnames(ramp.means) <- x

for(i in unique(gg.dat.long.3$distance.to.ramp)){
  ramp.sim.1 <- gg.dat.long.3
  ramp.sim.2 <- dplyr::filter(ramp.sim.1,distance.to.ramp==i)
  print(ramp.sim.3 <- dplyr::summarise_at(ramp.sim.2, vars(predicted.sim), list(mean)))
  ramp.means <- rbind(ramp.means, ramp.sim.3$predicted.sim)
}

ramp.means <- ramp.means[-1,]
ramp <- unique(gg.dat.3$distance.to.ramp)
ramp.means <- as.data.frame(cbind(ramp.means,ramps))

# Get means for each value of bathymetry for each simulation 
ramp.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("ramp.mean.sim")
colnames(ramp.mean.sim) <- x

for(i in 1:1002){
  ramp.sim.1 <- gg.dat.long.3
  ramp.sim.2 <- filter(ramp.sim.1, sim==i)
  ramp.sim.3 <- group_by(ramp.sim.2, distance.to.ramp)
  print(ramp.sim.4 <- summarise_at(ramp.sim.3, vars(predicted.sim), list(mean)))
  ramp.mean.sim <- cbind(ramp.mean.sim, ramp.sim.4)
}

ramp.mean.full <- ramp.mean.sim[ ,seq(3, ncol(ramp.mean.sim), 2)]
ramp.mean.full$distance.to.ramp <- ramp.mean.sim[,2]

ramp.mean.long <- ramp.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results ramp
predict.plot.ramp <- ggplot() +
  geom_line(data=ramp.mean.long, aes(x = distance.to.ramp, y = mean, group=sim), colour='lightcoral'
            , alpha=0.2)+
  geom_line(data=ramp.means, aes(x = ramps, y = ramp.means), colour='darkred') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Distance to Boat Ramp")+
  Theme1
predict.plot.ramp



##### Model 4 - Just status #####
Model.4 <- uGamm(response~s(bathymetry,k=5,bs='cs') + factor(status), 
                 random=~(1|site), 
                 family=poisson(), data=use.dat, lme4=TRUE)

stangam.s.4  <- stan_gamm4(response~s(bathymetry, k = 5, bs = "cs")+ status,
                           random=~(1|site), adapt_delta = 0.99,
                           data=use.dat, chains=3, cores=3, iter=41000, warmup=40000, thin=3,
                           family=poisson())


effect.dat.4 <- expand.grid(bathymetry=seq(min(legal.dat$bathymetry),max(legal.dat$bathymetry), length.out = 20),
                            status=unique(legal.dat$status))

# calcualte frequentist effects
predicted <- predict.gam(Model.4$gam, newdata=effect.dat.4, type='response')
effect.dat.4 <- cbind(effect.dat.4, predicted)

# bathy effect 
bathy.P.4 <- effect.dat.4%>%
  group_by(bathymetry)%>%
  summarise_at(vars(predicted), list(mean))

bathy.E.4 <- diff(range(bathy.P.4$predicted))
bathy.E.4

# Status effect
status.P.4 <- effect.dat.4%>%
  group_by(status)%>%
  summarise_at(vars(predicted), list(mean))

status.E.4 <- diff(range(status.P.4$predicted))
status.E.4

## Calcuate Bayesian effects 
effect.dat.sim.4 <- effect.dat.4
gg.4 <- t(posterior_predict(stangam.s.4, effect.dat.sim.4, re.form=NA)) # Returning in fish
#colnames(gg) <- paste("sim",1:ncol(gg),sep="_")
gg.dat.4 <- cbind(effect.dat.sim.4,gg.4)

gg.dat.long.4 <- gg.dat.4%>%
  gather(sim, predicted.sim, 4:1005)

gg.dat.long.4$sim <- as.factor(gg.dat.long.4$sim)


# bathy effect 
bathy.4.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.mean")
colnames(bathy.4.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.4
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean))
  print(bathy.sim.5 <- diff(range(bathy.sim.4$predicted.sim)))
  bathy.4.sim <- rbind(bathy.4.sim, bathy.sim.5)
  
}

bathy.4.sim

write.csv(bathy.4.sim, "bayesian.bathy.predictions.4.csv")


# status effect 
status.4.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("status.mean")
colnames(status.4.sim) <- x

for(i in 1:1002){
  status.sim.1 <- gg.dat.long.4
  status.sim.2 <- filter(status.sim.1, sim==i)
  status.sim.3 <- group_by(status.sim.2, status)
  status.sim.4 <- summarise_at(status.sim.3, vars(predicted.sim), list(mean))
  print(status.sim.5 <- diff(range(status.sim.4$predicted.sim)))
  status.4.sim <- rbind(status.4.sim, status.sim.5)
  
}
status.4.sim

write.csv(status.4.sim, "bayesian.status.predictions.4.csv")

## Plot effect sizes 
bathy.4.sim <- bathy.4.sim[-1,]
bathy.4.sim <- as.data.frame(bathy.4.sim)
x <- "Bathymetry"
colnames(bathy.4.sim) <- x

status.4.sim <- status.4.sim[-1,]
status.4.sim <- as.data.frame(status.4.sim)
x <- "Status"
colnames(status.4.sim) <- x

full.data <- as.data.frame(cbind(bathy.4.sim, status.4.sim))

full.data.long <- full.data%>%
  gather(variable, predicted, 1:2)

effect.plot <- ggplot(full.data.long, aes(x = predicted, fill = variable, color=variable)) + geom_density(alpha = 0.5)+
  xlim(-1,10)+
  geom_vline(xintercept = 1.462369, color = "darkblue", size=0.75)+
  geom_vline(xintercept = 2.999757, color = "darkred", size=0.7)+
  labs(y="Density", x="Effect Size")+
  Theme1
effect.plot

## Plot predicted values 
# Bathy effect
bathy.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.means")
colnames(bathy.means) <- x

for(i in unique(gg.dat.long.4$bathymetry)){
  bathy.sim.1 <- gg.dat.long.4
  bathy.sim.2 <- dplyr::filter(bathy.sim.1, bathymetry==i)
  print(bathy.sim.3 <- dplyr::summarise_at(bathy.sim.2, vars(predicted.sim), list(mean)))
  bathy.means <- rbind(bathy.means, bathy.sim.3$predicted.sim)
}

bathy.means <- bathy.means[-1,]
depths <- unique(gg.dat.4$bathymetry)
bathy.means <- as.data.frame(cbind(bathy.means,depths))

# Get means for each value of bathymetry for each simulation 
bathy.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("bathy.mean.sim")
colnames(bathy.mean.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.4
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  print(bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean)))
  bathy.mean.sim <- cbind(bathy.mean.sim, bathy.sim.4)
}

bathy.mean.full <- bathy.mean.sim[ ,seq(3, ncol(bathy.mean.sim), 2)]
bathy.mean.full$bathymetry <- bathy.mean.sim[,2]

bathy.mean.long <- bathy.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results bathy
predict.plot.bathy <- ggplot() +
  geom_line(data=bathy.mean.long, aes(x = bathymetry, y = mean, group=sim), colour='lightblue'
            , alpha=0.2)+
  geom_line(data=bathy.means, aes(x = depths, y = bathy.means), colour='darkblue') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Bathymetry")+
  Theme1
predict.plot.bathy

# status effect
status.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("status.means")
colnames(status.means) <- x

for(i in unique(gg.dat.long.4$status)){
  status.sim.1 <- gg.dat.long.4
  status.sim.2 <- dplyr::filter(status.sim.1, status==i)
  print(status.sim.3 <- dplyr::summarise_at(status.sim.2, vars(predicted.sim), list(mean)))
  status.means <- rbind(status.means, status.sim.3$predicted.sim)
}

status.means <- status.means[-1,]
status <- unique(gg.dat.4$status)
status.means <- as.data.frame(cbind(status.means,status))

# Get means for each value of bathymetry for each simulation 
status.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("status.mean.sim")
colnames(status.mean.sim) <- x

for(i in 1:1002){
  status.sim.1 <- gg.dat.long.4
  status.sim.2 <- filter(status.sim.1, sim==i)
  status.sim.3 <- group_by(status.sim.2, status)
  print(status.sim.4 <- summarise_at(status.sim.3, vars(predicted.sim), list(mean)))
  status.mean.sim <- cbind(status.mean.sim, status.sim.4)
}

status.mean.full <- status.mean.sim[ ,seq(3, ncol(status.mean.sim), 2)]
status.mean.full$status <- status.mean.sim[,2]

status.mean.long <- status.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results bathy
predict.plot.status <- ggplot() +
  geom_line(data=status.mean.long, aes(x = status, y = mean, group=sim), colour='lightcoral'
            , alpha=0.2)+
  geom_line(data=status.means, aes(x = status, y = status.means), colour='darkred') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Status")+
  Theme1
predict.plot.status

##### Model 5 - Ramp model without site as a random factor ####
# Predict from the model #

effect.dat.5 <- expand.grid(cube.Aspect=seq(min(legal.dat$cube.Aspect),max(legal.dat$cube.Aspect),length.out = 20),
                            distance.to.ramp=seq(min(legal.dat$distance.to.ramp),max(legal.dat$distance.to.ramp),length.out = 20),
                            bathymetry=seq(min(legal.dat$bathymetry),max(legal.dat$bathymetry), length.out=20))
#site=lme4.site$gam$model$site)%>%

use.dat <- legal.dat%>%
  dplyr::select(response, distance.to.ramp, cube.Aspect, sqrt.slope, log.roughness, status, FlowDir,
                bathymetry, site)
factor.vars <- c("status")

# now fit with shrinkage - to generate predicted values
Model.5 <- uGamm(response~s(bathymetry,k=5,bs='cs')+ s(distance.to.ramp, k=5, bs='cs') 
                 + s(cube.Aspect,k=5,bs='cs'),
                 family=poisson(), data=use.dat, lme4=TRUE)

stangam.s.5  <- stan_gamm4(response~s(bathymetry, k = 5, bs = 'cs')+ s(distance.to.ramp, k=5, bs='cs') 
                           + s(cube.Aspect,k=5,bs='cs'), adapt_delta = 0.99,
                           data=use.dat, chains=3, cores=3, iter=41000, warmup=40000, thin=3,
                           family=poisson())

# calculate frequentist effects - link function is log 
predicted <- predict.gam(Model.5$gam, newdata=effect.dat.5, type='response')
effect.dat.5 <- cbind(effect.dat.5, predicted)


# bathy effect 
bathy.P.5 <- effect.dat.5%>%
  group_by(bathymetry)%>%
  summarise_at(vars(predicted), list(mean))

bathy.E.5 <- diff(range(bathy.P.5$predicted)) 
bathy.E.5
# aspect effect 
aspect.P.5 <- effect.dat.5%>%
  group_by(cube.Aspect)%>%
  summarise_at(vars(predicted), list(mean))

aspect.E.5 <- diff(range(aspect.P.5$predicted))
aspect.E.5

# ramp effect 
ramp.P.5 <- effect.dat.5%>%
  group_by(distance.to.ramp)%>%
  summarise_at(vars(predicted), list(mean))

ramp.E.5 <- diff(range(ramp.P.5$predicted))
ramp.E.5

# use the bayesian models to calculate a posterior distribution of the effect size
effect.dat.sim.5 <- effect.dat.5
gg <- t(posterior_predict(stangam.s.5, effect.dat.sim.5)) # Returning in fish
#colnames(gg) <- paste("sim",1:ncol(gg),sep="_")
gg.dat.5 <- cbind(effect.dat.sim.5,gg)


gg.dat.long.5 <- gg.dat.5%>%
  gather(sim, predicted.sim, 5:1006)

gg.dat.long.5$sim <- as.factor(gg.dat.long.5$sim)

# bathy effect 
bathy.5.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.mean")
colnames(bathy.5.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.5
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean))
  print(bathy.sim.5 <- diff(range(bathy.sim.4$predicted.sim)))
  bathy.5.sim <- rbind(bathy.5.sim, bathy.sim.5)
  
}

bathy.5.sim

write.csv(bathy.5.sim, "bayesian.bathy.predictions.5.csv")

# aspect effect 
aspect.5.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("aspect.mean")
colnames(aspect.5.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long.5
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean))
  print(aspect.sim.5 <- diff(range(aspect.sim.4$predicted.sim)))
  aspect.5.sim <- rbind(aspect.5.sim, aspect.sim.5)
  
}

aspect.5.sim

write.csv(aspect.5.sim, "bayesian.aspect.predictions.5.cvs")

# ramp effect 
ramp.5.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("ramp.mean")
colnames(ramp.5.sim) <- x

for(i in 1:1002){
  ramp.sim.1 <- gg.dat.long.5
  ramp.sim.2 <- filter(ramp.sim.1, sim==i)
  ramp.sim.3 <- group_by(ramp.sim.2, distance.to.ramp)
  ramp.sim.4 <- summarise_at(ramp.sim.3, vars(predicted.sim), list(mean))
  print(rampsim.5 <- diff(range(ramp.sim.4$predicted.sim)))
  ramp.5.sim <- rbind(ramp.5.sim, ramp.sim.5)
  
}

ramp.5.sim

write.csv(ramp.5.sim, "bayesian.ramp.predictions.5.csv")

# Plot effect sizes 
ramp.5.sim <- ramp.5.sim[-1,]
ramp.5.sim <- as.data.frame(ramp.5.sim)
x <- c("Distance to Boat Ramp")
colnames(ramp.5.sim) <- x

bathy.5.sim <- bathy.5.sim[-1,]
bathy.5.sim <- as.data.frame(bathy.5.sim)
x <- c("Bathymetry")
colnames(bathy.5.sim) <- x

aspect.5.sim <- aspect.5.sim[-1,]
aspect.5.sim <- as.data.frame(aspect.5.sim)
x <- c("Aspect")
colnames(aspect.5.sim) <- x

full.data <- as.data.frame(cbind(ramp.5.sim, bathy.5.sim, aspect.5.sim))

full.data.long <- full.data%>%
  gather(variable, effect.size, 1:3)


effect.plot <- ggplot(full.data.long, aes(x = effect.size, fill = variable, colour = variable)) + geom_density(alpha = 0.5) +
  xlim(-1,8)+
  geom_vline(xintercept = 2.346501, color = "darkgreen", size=0.75)+
  geom_vline(xintercept = 2.5651, color = "darkblue", size=0.75)+
  geom_vline(xintercept = 1.386464, color = "darkred", size=0.7)+
  labs(y="Density", x="Effect Size")+
  Theme1
effect.plot

## Make prediction plots

# bathy effect 
bathy.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.median")
colnames(bathy.median) <- x

for(i in unique(gg.dat.long$bathymetry)){
  bathy.sim.1 <- gg.dat.long
  bathy.sim.2 <- dplyr::filter(bathy.sim.1, bathymetry==i)
  print(bathy.sim.3 <- dplyr::summarise_at(bathy.sim.2, vars(predicted.sim), list(mean)))
  bathy.means <- rbind(bathy.means, bathy.sim.3$predicted.sim)
}

bathy.means <- bathy.means[-1,]
depths <- unique(gg.dat$bathymetry)
bathy.means <- as.data.frame(cbind(bathy.means,depths))

# Get means for each value of bathymetry for each simulation 
bathy.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("bathy.mean.sim")
colnames(bathy.mean.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.5
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  print(bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean)))
  bathy.mean.sim <- cbind(bathy.mean.sim, bathy.sim.4)
}

bathy.mean.full <- bathy.mean.sim[ ,seq(3, ncol(bathy.mean.sim), 2)]
bathy.mean.full$bathymetry <- bathy.mean.sim[,2]

bathy.mean.long <- bathy.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results bathy
predict.plot.bathy <- ggplot() +
  geom_line(data=bathy.mean.long, aes(x = bathymetry, y = mean, group=sim), colour='lightblue'
            , alpha=0.2)+
  geom_line(data=bathy.means, aes(x = depths, y = bathy.means), colour='darkblue') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Bathymetry")+
  Theme1
predict.plot.bathy

# aspect effect 
aspect.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("aspect.mean")
colnames(aspect.means) <- x

for(i in unique(gg.dat.long.5$cube.Aspect)){
  aspect.sim.1 <- gg.dat.long.5
  aspect.sim.2 <- dplyr::filter(aspect.sim.1, cube.Aspect==i)
  print(aspect.sim.3 <- dplyr::summarise_at(aspect.sim.2, vars(predicted.sim), list(mean)))
  aspect.means <- rbind(aspect.means, aspect.sim.3$predicted.sim)
}

aspect.means <- aspect.means[-1,]
aspect <- unique(gg.dat.5$cube.Aspect)
aspect.means <- as.data.frame(cbind(aspect.means,aspect))

# Get means for each value of aspect for each simulation 
aspect.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("aspect.mean.sim")
colnames(aspect.mean.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long.5
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  print(aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean)))
  aspect.mean.sim <- cbind(aspect.mean.sim, aspect.sim.4)
}

aspect.mean.full <- aspect.mean.sim[ ,seq(3, ncol(aspect.mean.sim), 2)]
aspect.mean.full$cube.Aspect <- aspect.mean.sim[,2]

aspect.mean.long <- aspect.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results aspect
predict.plot.aspect <- ggplot() +
  geom_line(data=aspect.mean.long, aes(x = cube.Aspect, y = mean, group=sim), colour='lightgreen',
            alpha=0.2) +
  geom_line(data=aspect.means, aes(x = aspect, y = aspect.means), colour='darkgreen') +
  labs(y="Predicted Abundance Legal Sized Fish", x="cube.Aspect")+
  Theme1
predict.plot.aspect

# ramp effect 
ramp.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("ramp.mean")
colnames(ramp.means) <- x

for(i in unique(gg.dat.long.5$distance.to.ramp)){
  ramp.sim.1 <- gg.dat.long.5
  ramp.sim.2 <- dplyr::filter(ramp.sim.1, distance.to.ramp==i)
  print(ramp.sim.3 <- dplyr::summarise_at(ramp.sim.2, vars(predicted.sim), list(mean)))
  ramp.means <- rbind(ramp.means, ramp.sim.3$predicted.sim)
}

ramp.means <- ramp.means[-1,]
ramps <- unique(gg.dat.5$distance.to.ramp)
ramp.means <- as.data.frame(cbind(ramp.means,ramps))

# Get means for each value of ramp for each simulation 
ramp.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("ramp.mean.sim")
colnames(ramp.mean.sim) <- x

for(i in 1:1002){
  ramp.sim.1 <- gg.dat.long.5
  ramp.sim.2 <- filter(ramp.sim.1, sim==i)
  ramp.sim.3 <- group_by(ramp.sim.2, distance.to.ramp)
  print(ramp.sim.4 <- summarise_at(ramp.sim.3, vars(predicted.sim), list(mean)))
  ramp.mean.sim <- cbind(ramp.mean.sim, ramp.sim.4)
}

ramp.mean.full <- ramp.mean.sim[ ,seq(3, ncol(ramp.mean.sim), 2)]
ramp.mean.full$distance.to.ramp <- ramp.mean.sim[,2]

ramp.mean.long <- ramp.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results ramps
predict.plot.ramps <- ggplot() +
  geom_line(data=ramp.mean.long, aes(x = distance.to.ramp, y = mean, group=sim), colour='lightcoral',
            alpha=0.2) +
  geom_line(data=ramp.means, aes(x = ramps, y = ramp.means), colour='darkred') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Distance to Boat Ramp")+
  Theme1
predict.plot.ramps



#### Model 6 - status model no random effect ####
# Predict from the model 

use.dat <- legal.dat%>%
  dplyr::select(response, distance.to.ramp, cube.Aspect, sqrt.slope, log.roughness, status, FlowDir,
                bathymetry, site)
factor.vars <- c("status")


Model.6 <- uGamm(response~s(bathymetry,k=5,bs='cr')+ factor(status)
                 + s(cube.Aspect,k=5,bs='cr'),
                 family=poisson(), data=use.dat, lme4=TRUE)



# now fit with shrinkage - to generate predicted values
Model.6 <- uGamm(response~s(bathymetry,k=5,bs='cs') + factor(status) +
                   + s(cube.Aspect,k=5,bs='cs'),
                 family=poisson(), data=use.dat, lme4=TRUE)

stangam.s.6  <- stan_gamm4(response~s(bathymetry, k = 5, bs = "cs")+ status
                           + s(cube.Aspect,k=5,bs='cs'), adapt_delta = 0.99,
                           data=use.dat, chains=3, cores=3, iter=41000, warmup=40000, thin=3,
                           family=poisson())


effect.dat.6 <- expand.grid(cube.Aspect=seq(min(legal.dat$cube.Aspect),max(legal.dat$cube.Aspect),length.out = 20),
                            bathymetry=seq(min(legal.dat$bathymetry),max(legal.dat$bathymetry), length.out = 20),
                            status=unique(legal.dat$status))



# calcualte frequentist effects
predicted <- predict.gam(Model.6$gam, newdata=effect.dat.6, type='response')
effect.dat.6 <- cbind(effect.dat.6, predicted)

# bathy effect 
bathy.P.6 <- effect.dat.6%>%
  group_by(bathymetry)%>%
  summarise_at(vars(predicted), list(mean))

bathy.E.6 <- diff(range(bathy.P.6$predicted))
bathy.E.6

# aspect effect 
aspect.P.6 <- effect.dat.6%>%
  group_by(cube.Aspect)%>%
  summarise_at(vars(predicted), list(mean))

aspect.E.6 <- diff(range(aspect.P.6$predicted))
aspect.E.6

# status effect 
status.P.6 <- effect.dat.6%>%
  group_by(status)%>%
  summarise_at(vars(predicted), list(mean))

status.E.6 <- diff(range(status.P.6$predicted))
status.E.6

#use the bayesian models to calculate a posterior distribution of the effect size
effect.dat.sim.6 <- effect.dat.6
gg.6 <- t(posterior_predict(stangam.s.6, effect.dat.sim.6, re.form=NA))
#colnames(gg) <- paste("sim",1:ncol(gg),sep="_")
gg.dat.6 <- cbind(effect.dat.sim.6,gg.6)

gg.dat.long.6 <- gg.dat.6%>%
  gather(sim, predicted.sim, 5:1006)

gg.dat.long.6$sim <- as.factor(gg.dat.long.6$sim)
head(gg.dat.long.6)

# bathy effect 
bathy.6.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.mean")
colnames(bathy.6.sim) <- x


for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.6
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean))
  print(bathy.sim.5 <- diff(range(bathy.sim.4$predicted.sim)))
  bathy.6.sim <- rbind(bathy.6.sim, bathy.sim.5)
  
}

bathy.6.sim

write.csv(bathy.6.sim, "bayesian.bathy.predictions.6.csv")

# aspect effect 
aspect.6.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("aspect.mean")
colnames(aspect.6.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long.6
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean))
  print(aspect.sim.5 <- diff(range(aspect.sim.4$predicted.sim)))
  aspect.6.sim <- rbind(aspect.6.sim, aspect.sim.5)
  
}

aspect.6.sim

write.csv(aspect.6.sim, "bayesian.aspect.predictions.6.csv")

# status effect 
status.6.sim <- data.frame(matrix(ncol=1, nrow=1))
x <- c("status.mean")
colnames(status.6.sim) <- x

for(i in 1:1002){
  status.sim.1 <- gg.dat.long.6
  status.sim.2 <- filter(status.sim.1, sim==i)
  status.sim.3 <- group_by(status.sim.2, status)
  status.sim.4 <- summarise_at(status.sim.3, vars(predicted.sim), list(mean))
  print(status.sim.5 <- diff(range(status.sim.4$predicted.sim)))
  status.6.sim <- rbind(status.6.sim, status.sim.5)
  
}

status.6.sim

write.csv(status.6.sim, "bayesian.status.predictions.6.csv")

## Create density plot
status.6.sim <- as.data.frame(status.6.sim[-1,])
x <- 'Status'
colnames(status.6.sim) <- x

bathy.6.sim <- as.data.frame(bathy.6.sim[-1,])
x <- 'Bathymetry'
colnames(bathy.6.sim) <- x

aspect.6.sim <- as.data.frame(aspect.6.sim[-1,])
x <- 'cube.Aspect'
colnames(aspect.6.sim) <- x

full.data <- as.data.frame(cbind(status.6.sim, bathy.6.sim, aspect.6.sim))

full.data.long <- full.data%>%
  gather(variable, predicted, 1:3)

effect.plot <- ggplot(full.data.long, aes(x = predicted, fill = variable, color=variable)) + geom_density(alpha = 0.5)+
  geom_vline(xintercept = 2.039047, color = "darkgreen", size=0.75)+
  geom_vline(xintercept = 1.863953, color = "darkblue", size=0.75)+
  geom_vline(xintercept = 3.114589, color = "darkred", size=0.7)+
  labs(y="Density", x="Effect Size")+
  Theme1
effect.plot


## Plot the predicted results for each of the variables 
# Bathy effect
bathy.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("bathy.means")
colnames(bathy.means) <- x

for(i in unique(gg.dat.long.6$bathymetry)){
  bathy.sim.1 <- gg.dat.long.6
  bathy.sim.2 <- dplyr::filter(bathy.sim.1, bathymetry==i)
  print(bathy.sim.3 <- dplyr::summarise_at(bathy.sim.2, vars(predicted.sim), list(mean)))
  bathy.means <- rbind(bathy.means, bathy.sim.3$predicted.sim)
}

bathy.means <- bathy.means[-1,]
depths <- unique(gg.dat.6$bathymetry)
bathy.means <- as.data.frame(cbind(bathy.means,depths))

# Get means for each value of bathymetry for each simulation 
bathy.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("bathy.mean.sim")
colnames(bathy.mean.sim) <- x

for(i in 1:1002){
  bathy.sim.1 <- gg.dat.long.6
  bathy.sim.2 <- filter(bathy.sim.1, sim==i)
  bathy.sim.3 <- group_by(bathy.sim.2, bathymetry)
  print(bathy.sim.4 <- summarise_at(bathy.sim.3, vars(predicted.sim), list(mean)))
  bathy.mean.sim <- cbind(bathy.mean.sim, bathy.sim.4)
}

bathy.mean.full <- bathy.mean.sim[ ,seq(3, ncol(bathy.mean.sim), 2)]
bathy.mean.full$bathymetry <- bathy.mean.sim[,2]

bathy.mean.long <- bathy.mean.full%>%
  gather(sim, mean, 1:1002)


# Plot the results bathy
predict.plot.bathy <- ggplot() +
  geom_line(data=bathy.mean.long, aes(x = bathymetry, y = mean, group=sim), colour='lightblue'
            , alpha=0.2)+
  geom_line(data=bathy.means, aes(x = depths, y = bathy.means), colour='darkblue') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Bathymetry")+
  Theme1
predict.plot.bathy

# aspect effect 
aspect.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("aspect.mean")
colnames(aspect.means) <- x

for(i in unique(gg.dat.long.6$cube.Aspect)){
  aspect.sim.1 <- gg.dat.long.6
  aspect.sim.2 <- dplyr::filter(aspect.sim.1, cube.Aspect==i)
  print(aspect.sim.3 <- dplyr::summarise_at(aspect.sim.2, vars(predicted.sim), list(mean)))
  aspect.means <- rbind(aspect.means, aspect.sim.3$predicted.sim)
}

aspect.means <- aspect.means[-1,]
aspect <- unique(gg.dat.6$cube.Aspect)
aspect.means <- as.data.frame(cbind(aspect.means,aspect))

# Get means for each value of aspect for each simulation 
aspect.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("aspect.mean.sim")
colnames(aspect.mean.sim) <- x

for(i in 1:1002){
  aspect.sim.1 <- gg.dat.long.6
  aspect.sim.2 <- filter(aspect.sim.1, sim==i)
  aspect.sim.3 <- group_by(aspect.sim.2, cube.Aspect)
  print(aspect.sim.4 <- summarise_at(aspect.sim.3, vars(predicted.sim), list(mean)))
  aspect.mean.sim <- cbind(aspect.mean.sim, aspect.sim.4)
}

aspect.mean.full <- aspect.mean.sim[ ,seq(3, ncol(aspect.mean.sim), 2)]
aspect.mean.full$cube.Aspect <- aspect.mean.sim[,2]

aspect.mean.long <- aspect.mean.full%>%
  gather(sim, mean, 1:1002)

# Plot the results aspect
predict.plot.aspect <- ggplot() +
  geom_line(data=aspect.mean.long, aes(x = cube.Aspect, y = mean, group=sim), colour='lightgreen',
            alpha=0.2) +
  geom_line(data=aspect.means, aes(x = aspect, y = aspect.means), colour='darkgreen') +
  labs(y="Predicted Abundance Legal Sized Fish", x="cube.Aspect")+
  Theme1
predict.plot.aspect

# status effect 
status.means <- data.frame(matrix(ncol=1, nrow=1))
x <- c("status.mean")
colnames(status.means) <- x

for(i in unique(gg.dat.long.6$status)){
  status.sim.1 <- gg.dat.long.6
  status.sim.2 <- dplyr::filter(status.sim.1, status==i)
  print(status.sim.3 <- dplyr::summarise_at(status.sim.2, vars(predicted.sim), list(mean)))
  status.means <- rbind(status.means, status.sim.3$predicted.sim)
}

status.means <- status.means[-1,]
status <- unique(gg.dat.6$status)
status.means <- as.data.frame(cbind(status.means,status))

# Get means for each value of aspect for each simulation 
status.mean.sim <- data.frame(matrix(ncol=1, nrow=20))
x <- c("status.mean.sim")
colnames(status.mean.sim) <- x

for(i in 1:1002){
  status.sim.1 <- gg.dat.long.6
  status.sim.2 <- filter(status.sim.1, sim==i)
  status.sim.3 <- group_by(status.sim.2, status)
  print(status.sim.4 <- summarise_at(status.sim.3, vars(predicted.sim), list(mean)))
  status.mean.sim <- cbind(status.mean.sim, status.sim.4)
}

status.mean.full <- status.mean.sim[ ,seq(3, ncol(status.mean.sim), 2)]
status.mean.full$status <- status.mean.sim[,2]

status.mean.long <- status.mean.full%>%
  gather(sim, mean, 1:1002)

# Plot the results status
predict.plot.status <- ggplot() +
  geom_line(data=status.mean.long, aes(x = status, y = mean, group=sim), colour='lightcoral',
            alpha=0.2) +
  geom_line(data=status.means, aes(x = status, y = status.means), colour='darkred') +
  labs(y="Predicted Abundance Legal Sized Fish", x="Distance to Boat Ramp")+
  Theme1
predict.plot.status