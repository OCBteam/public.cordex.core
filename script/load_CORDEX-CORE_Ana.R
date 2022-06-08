# Load CORDEX-CORE from UDG
# A. Casanueva, 06.04.22
# Dependencies: climate4R



rm(list=ls())



library(climate4R.UDG)
library(loadeR)
library(transformeR)
library(tidyverse)

# historical simulations
dset.hist <- c("CORDEX-AFR-22_MOHC-HadGEM2-ES_historical_r1i1p1_GERICS-REMO2015_v1",       
               "CORDEX-AFR-22_MOHC-HadGEM2-ES_historical_r1i1p1_ICTP-RegCM4-7_v0",         
               "CORDEX-AFR-22_MPI-M-MPI-ESM-LR_historical_r1i1p1_GERICS-REMO2015_v1",      
               "CORDEX-AFR-22_MPI-M-MPI-ESM-MR_historical_r1i1p1_ICTP-RegCM4-7_v0",        
               "CORDEX-AFR-22_NCC-NorESM1-M_historical_r1i1p1_GERICS-REMO2015_v1",         
               "CORDEX-AFR-22_NCC-NorESM1-M_historical_r1i1p1_ICTP-RegCM4-7_v0")           


# rcp26 simulations
dset.rcp26 <- c("CORDEX-AFR-22_MOHC-HadGEM2-ES_rcp26_r1i1p1_GERICS-REMO2015_v1",            
                "CORDEX-AFR-22_MOHC-HadGEM2-ES_rcp26_r1i1p1_ICTP-RegCM4-7_v0",     
                "CORDEX-AFR-22_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_GERICS-REMO2015_v1",           
                "CORDEX-AFR-22_MPI-M-MPI-ESM-MR_rcp26_r1i1p1_ICTP-RegCM4-7_v0",             
                "CORDEX-AFR-22_NCC-NorESM1-M_rcp26_r1i1p1_GERICS-REMO2015_v1",              
                "CORDEX-AFR-22_NCC-NorESM1-M_rcp26_r1i1p1_ICTP-RegCM4-7_v0")                


# rcp85 simulations
dset.rcp85 <- c("CORDEX-AFR-22_MOHC-HadGEM2-ES_rcp85_r1i1p1_GERICS-REMO2015_v1",            
                "CORDEX-AFR-22_MOHC-HadGEM2-ES_rcp85_r1i1p1_ICTP-RegCM4-7_v0",              
                "CORDEX-AFR-22_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_GERICS-REMO2015_v1",           
                "CORDEX-AFR-22_MPI-M-MPI-ESM-MR_rcp85_r1i1p1_ICTP-RegCM4-7_v0",             
                "CORDEX-AFR-22_NCC-NorESM1-M_rcp85_r1i1p1_GERICS-REMO2015_v1",              
                "CORDEX-AFR-22_NCC-NorESM1-M_rcp85_r1i1p1_ICTP-RegCM4-7_v0")          


###################
# *** LOAD DATA ***
###################
# Example of African subdomain
xlimit <- c(34,46)
ylimit <- c(10,21)


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

ls <- c( "CORDEX-AFR-22_MPI-M-MPI-ESM-MR_rcp85_r1i1p1_ICTP-RegCM4-7_v0",  "CORDEX-AFR-22_MOHC-HadGEM2-ES_rcp26_r1i1p1_ICTP-RegCM4-7_v0")

try <- map(ls, ~ loadGridData(dataset=.x,
                    var = "tas", years = 2010:2098, season = 1:12, latLim = c(-10,0), lonLim = c(-20,-10)))



