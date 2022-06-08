library(climate4R.UDG)
library(loadeR)
library(transformeR)
library(tidyverse)
library(furrr)


data_info <- read_csv("../data_info.csv")

lat <- c(10,21)
lon <- c(34,46)
bias.correction=FALSE

plan(multisession, workers = 3)

mod <- data_info %>% 
  filter(sensitivity=="high", RCP!="historical", simulation=="ICTP") %>% 
  { if (bias.correction) {
    
  muatate(., obs= loadGridData(dataset="W5E5", var = "tas", years = 1980:2010, season = 1:12, latLim = lat, lonLim = lon, aggr.m = "mean"))
  } else {.}
    
  } %>% 
  mutate(model= future_map(file, ~ loadGridData(dataset=.x, var = "tas", years = 2010:2099, season = 1:12, latLim = lat, lonLim = lon, aggr.m = "mean")))


mod$model[[1]]$Data

?interpGrid

for(i in 1:length(dset.hist)){
  dset <- dset.hist[i]
  message("Starting ", dset)
  
  # load either whole domain (REMO) or subdomain of interest (RegCM)
  if(dset=="CORDEX-AFR-22_MOHC-HadGEM2-ES_historical_r1i1p1_GERICS-REMO2015_v1" | dset=="CORDEX-AFR-22_MPI-M-MPI-ESM-LR_historical_r1i1p1_GERICS-REMO2015_v1" | dset=="CORDEX-AFR-22_NCC-NorESM1-M_historical_r1i1p1_GERICS-REMO2015_v1"){
    xlim <- NULL; ylim<- NULL; xlim.obs <-c(-30,60) ; ylim.obs <-c(-30,40) # African limits for observations, whole domain for REMO
  } else{xlim <- xlimit; ylim <- ylimit; xlim.obs <-xlimit ; ylim.obs <-ylimit} # subdomain limits for all
  hist <- loadGridData(dataset=dset, var = "tas", years = 1990, season = 6:8, latLim = ylim, lonLim = xlim) 
  obs <- loadGridData("W5E5", var = "tas", years = 1990, season = 6:8, latLim = ylim.obs, lonLim = xlim.obs)  
  
  # interpolate to observations grid
  hist_interp <- interpGrid(hist, new.coordinates = getGrid(obs))
  # plot to check
  print(spatialPlot(climatology(hist_interp), backdrop.theme = "coastline"))
  
  # subset subdomain if model is REMO
  if(dset=="CORDEX-AFR-22_MOHC-HadGEM2-ES_historical_r1i1p1_GERICS-REMO2015_v1" | dset=="CORDEX-AFR-22_MPI-M-MPI-ESM-LR_historical_r1i1p1_GERICS-REMO2015_v1" | dset=="CORDEX-AFR-22_NCC-NorESM1-M_historical_r1i1p1_GERICS-REMO2015_v1"){
    hist_interp <- subsetGrid(hist_interp, lonLim = xlimit, latLim = ylimit)
  } 
  # plot to check
  print(spatialPlot(climatology(hist_interp), backdrop.theme = "coastline"))
  message("Finished ", dset)
}

