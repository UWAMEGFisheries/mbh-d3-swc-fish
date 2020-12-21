### Testing SpeciesMix script ####
# Fit Mixtures of Archetype Species # 


# I have not been successfull at installing this package: SpeciesMix
# It has been retired from CRAN 

#install.packages("//uniwa.uwa.edu.au/userhome/staff1/00093391/My Documents/R/SpeciesMix-master.zip", type="binary", repos=NULL)

# ecomix package

devtools::install_github('skiptoniam/ecomix')
install_github("twitter/AnomalyDetection")
library(ecomix)
library(AnomalyDetection)
