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
api = "https://records-ws.nbnatlas.org/occurrences/search?q=lsid%3ANHMSYS0021060242&fq=occurrence_status%3Apresent&fq=-(identification_verification_status%3A%22Unconfirmed%22%20OR%20identification_verification_status%3A%22Unconfirmed%20-%20not%20reviewed%22%20OR%20identification_verification_status%3A%22Unconfirmed%20-%20plausible%22)"

api = "https://records-ws.nbnatlas.org/index/fields"
taxonInfo = GET(api)
taxonInfoContent = httr::content(taxonInfo, as = 'text')
taxonInfoContentJSON = jsonlite::fromJSON(taxonInfoContent)
df=taxonInfoContentJSON$occurrences
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
# Number of inat records at least 1km away from NBN Atlas records

# Combine the inat_data dataframe with the new columns
NBN_recs = rep(NA, times = length(natbioblitz_nns[,1]))
natbioblitz_nns = cbind(natbioblitz_nns,NBN_recs)

for (a in 22:length(natbioblitz_nns[,1])) {
  binomClassNm = natbioblitz_nns$taxon.name[a]
  binomClassNmSplit = strsplit(binomClassNm,"[ ]")
  genus = binomClassNmSplit[[1]][1]
  species = binomClassNmSplit[[1]][2]
  
  # Paste the genus and species names into the NBN Atlas API key and download relevant data
  lat = natbioblitz_nns$latitude[a]
  lon = natbioblitz_nns$longitude[a]
  
  api = paste("https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=17&q=*:*&fq=genus:",
              genus,
              "&facets=rk_species:",
              species,
              "&lat=",
              lat,
              "&lon=",
              lon,
              "&radius=2.0&qa=none",
              sep = "")
  print(paste("a =",a))
  print(Sys.time())
  # pause for 1 minute
  Sys.sleep(60)
  print(Sys.time())
  
  download.file(url = api, 
                destfile = "C:/Users/Rose/rock_pool_bioblitz/data.zip", 
                mode = "wb")
  dfNBN = readr::read_csv(unz(
    description = "C:/Users/Rose/rock_pool_bioblitz/data.zip",
    filename = "data.csv"))
  dfNBN=as.data.frame(dfNBN)
  
  numRecords = length(dfNBN[,1])
  natbioblitz_nns$NBN_recs.1[a]=numRecords
  print(paste(natbioblitz_nns$taxon.common_name.name[a],"record =",natbioblitz_nns$NBN_recs.1[a]))
  rm(dfNBN)
}



#########
#############
#End
