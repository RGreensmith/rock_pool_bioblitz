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

api = "https://records-ws.nbnatlas.org/index/fields"
taxonInfo = GET(api)
taxonInfoContent = httr::content(taxonInfo, as = 'text')
taxonInfoContentJSON = jsonlite::fromJSON(taxonInfoContent)

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
# Creating data frame for number of inat records at least 1km away from NBN Atlas records
nnSppCNm=unique(as.character(natbioblitz_nns$taxon.common_name.name))
nnSppSNm=unique(as.character(natbioblitz_nns$taxon.name))
recordsInBuffer = matrix(ncol = 4, nrow = length(nnSppCNm))
recordsInBuffer=as.data.frame(recordsInBuffer)
row.names(recordsInBuffer)=nnSppSNm
names(recordsInBuffer)=c("latin_name","NBN_recs_in_inat_buffer",
                         "total_inat_recs",
                         "percent_new_recs")
recordsInBuffer$latin_name = latinName

NBN_recs = rep(NA, times = length(natbioblitz_nns[,1]))

# Combine the inat_data dataframe with the new columns
natbioblitz_nns = cbind(natbioblitz_nns,NBN_recs)

for (a in 1:length(nnSppSNm)) {
  natbioblitz_nns_filtered = filter(natbioblitz_nns,
                                    taxon.name == nnSppSNm[a])
  binomClassNm = natbioblitz_nns_filtered$taxon.name[1]
  binomClassNmSplit = strsplit(binomClassNm,"[ ]")
  genus = binomClassNmSplit[[1]][1]
  species = binomClassNmSplit[[1]][2]
  
  for (b in 1:length(natbioblitz_nns_filtered[,1])) {
    ####################### Get NBN Atlas Records for each point #################
    # records of Sargassum muticum within 100km (radius=100.0) of a point in London
    
    # Paste the genus and species names into the NBN Atlas API key and download relevant data
    lat = natbioblitz_nns_filtered$latitude[b]
    lon = natbioblitz_nns_filtered$longitude[b]
    
    api = paste("https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=17&q=*:*&fq=genus:",
                genus,
                "&facets=rk_species:",
                species,
                "&lat=",
                lat,
                "&lon=",
                lon,
                "&radius=10.0&qa=none",
                "&flimit=10",
                sep = "")
    taxonInfo = GET(api)
    
    download.file(url = api, 
                  destfile = "C:/Users/Rose/rock_pool_bioblitz/data.zip", 
                  mode = "wb")
    dfNBN = readr::read_csv(unz(
      description = "C:/Users/Rose/rock_pool_bioblitz/data.zip",
      filename = "data.csv"))
    dfNBN=as.data.frame(dfNBN)
    numRecords = length(dfNBN[,1])
    
    recordsInBuffer$NBN_recs_in_inat_buffer[a]=recordsInBuffer$NBN_recs_in_inat_buffer[a]+numRecords
    natbioblitz_nns_filtered$NBN_recs[b]=numRecords
  }
}
#############
#End
