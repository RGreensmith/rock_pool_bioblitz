################################################################################
# Playground for working out how to make Getis-Ord Hotspot analysis maps of
#     observations to show geographical coverage
################################################################################
# Notes:

# Maybe bin the data into 1km squares and create column for number of unique
#     observers per 1km square (or another spatial resolution i.e. 50 m^2)
#     - which will be useful for NBN comparisons anyway.

##### Links to guides ###########

#https://rpubs.com/heatherleeleary/hotspot_getisOrd_tut
#https://www.publichealth.columbia.edu/research/population-health-methods/hot-spot-spatial-analysis

library(sf)       #for simple features
library(sfdep)    #for spatial analyses
library(spdep)    #for spatial analyses
library(tidyr)    #for data manipulation
library(ggplot2)  #for data visualization

################################################################################
# inat data
################################################################################

library(rinat)     # using wrapper for downloading data
library(httr)      # for getting data using inat and WoRMS API's
library(lubridate) # for date conversion
library(dplyr)
library(utils)

# Define project ID and API parameters
project_slug <- "brpc-national-bioblitz-2025-practice"

# Download data using the rinat package
inat_data <- get_inat_obs_project(project_slug)

# Convert observed_on to date-time for comparison
inat_data$updated_at <- ymd_hms(inat_data$updated_at)
inat_data$time_observed_at <- ymd_hms(inat_data$time_observed_at)

last_update <- max(inat_data$updated_at)
cat("Last update:", as.character(last_update), "\n")

saved_data_path <- "../NatBioBlitz_iNat.RData"

if (file.exists(saved_data_path)) {
  load(saved_data_path)
} else {
  source("scripts/new get project obs function.R")
  NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
  save(NatBioBlitz_iNat, file = saved_data_path)
}

location_split <- strsplit(NatBioBlitz_iNat$location, ",")
location_df <- do.call(
  rbind,
  lapply(location_split, function(x) as.numeric(x))
)
colnames(location_df) <- c("latitude", "longitude")

NatBioBlitz_iNat <- cbind(NatBioBlitz_iNat, location_df) %>%
  filter(!is.na(latitude), !is.na(longitude))

obs_points <- st_as_sf(
  NatBioBlitz_iNat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

hist(obs_points)
head(obs_points)
################################################################################
#End