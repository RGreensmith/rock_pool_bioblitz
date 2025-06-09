################################################################################
# Playground for working out how to: bin the data into 1km squares
################################################################################
# Notes:

# https://cengel.github.io/R-spatial/spatialops.html#spatial-aggregation-points-in-polygons

# Bin the data into 1km squares and create column for number of unique
#     observers per 1km square (or another spatial resolution i.e. 50 m^2)
#     - which will be useful for NBN comparisons anyway.

# Check the NBN Atlas

##### Links to guides ###########


library(sf)       #for simple features
library(sfdep)    #for spatial analyses
library(spdep)    #for spatial analyses
library(tidyr)    #for data manipulation
library(ggplot2)  #for data visualization
install.packages("rnbn")

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

# create obs_points simple feature - Geodetic CRS:  WGS 84
obs_points <- st_as_sf(
  NatBioBlitz_iNat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

head(obs_points)

################################################################################
# NBN Atlas data
################################################################################

# Reason key:
# {"rkey":"logger.download.reason.professional.research","name":
# "professional researcher/publisher|
# Download may lead to professional publication (including universities,
# NGOs etc. where people are being paid to research and publish)",
# "id":17,"deprecated":false}

api = "https://records-ws.nbnatlas.org/index/fields"
taxonInfo = GET(api)
taxonInfoContent = httr::content(taxonInfo, as = 'text')
taxonInfoContentJSON = jsonlite::fromJSON(taxonInfoContent)

api2 = "https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&fq=genus:Vulpes&lat=51.5074&lon=0.1278&radius=10.0&qa=none"
taxonInfo2 = GET(api2)
taxonInfoContent2 = content(taxonInfo2,as="text",type = "application/zip")
taxonInfoContentJSON2 = readr::read_csv(unzip(taxonInfoContent2))
a=unzip(taxonInfoContent2)
a=unz(taxonInfoContent2,filename = "data")

data=read.csv(unz(taxonInfo2,filename = "data.csv",open = "r"))
unz("C:/Users/Rose/rock_pool_bioblitz/data.zip",filename = "data",open = "")

###########################################################################
# records of Sargassum muticum within 100km (radius=100.0) of a point in London
# api2 = "https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&fq=genus:Sargassum&lat=51.5074&lon=0.1278&radius=100.0&qa=none"
api2 = "https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&fq=genus:Sargassum&facets=rk_species:muticum&lat=51.5074&lon=0.1278&radius=100.0&qa=none"

download.file(url = api2, 
              destfile = "C:/Users/Rose/rock_pool_bioblitz/data.zip", 
              mode = "wb")
df = readr::read_csv(unz(description = "C:/Users/Rose/rock_pool_bioblitz/data.zip", 
                        filename = "data.csv"))
df=as.data.frame(df)
numRecords = length(df[,1])
################################################################################
#End
