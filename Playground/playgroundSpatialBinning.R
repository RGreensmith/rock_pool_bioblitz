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
api = "https://records-ws.nbnatlas.org/occurrences/search?q=*:*&fq=genus:Corella&pageSize=1000"
api="https://records-ws.nbnatlas.org/occurrences/search?q=*:*&fq=genus:Corella&fq=-(identification_verification_status%3A%22Unconfirmed%22%20OR%20identification_verification_status%3A%22Unconfirmed%20-%20not%20reviewed%22%20OR%20identification_verification_status%3A%22Unconfirmed%20-%20plausible%22)&fq=-occurrence_status%3A%22absent%22&fq=taxon_name%3A%22Corella%20eumyota%22&pageSize=1000"
taxonInfo = GET(api)
taxonInfoContent = httr::content(taxonInfo, as = 'text')
taxonInfoContentJSON = jsonlite::fromJSON(taxonInfoContent)
df=taxonInfoContentJSON$occurrences
###########################################################################
# records of Sargassum muticum within 100km (radius=100.0) of a point in London
# api2 = "https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&fq=genus:Sargassum&lat=51.5074&lon=0.1278&radius=100.0&qa=none"
api2 = "https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&fq=genus:Sargassum&facets=rk_species:muticum&lat=51.5074&lon=0.1278&radius=100.0&qa=none"
# Corella eumyota
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

for (a in 6:length(natbioblitz_nns[,1])) {
  binomClassNm = natbioblitz_nns$taxon.name[a]
  binomClassNmSplit = strsplit(binomClassNm,"[ ]")
  genus = binomClassNmSplit[[1]][1]
  species = binomClassNmSplit[[1]][2]
  
  # Paste the genus and species names into the NBN Atlas API key and download relevant data
  lat = natbioblitz_nns$latitude[a]
  lon = natbioblitz_nns$longitude[a]
  
  api = paste("https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&fq=genus:",
              genus,
              "&lat=",
              lat,
              "&lon=",
              lon,
              "&radius=2.0&qa=none",
              "&fq=-occurrence_status%3A%22absent%22",
              "&fq=occurrence_status%3A%22present%22",
              "&fq=taxon_name%3A%22",
              genus,
              "%20",
              species,
              "%22",
              sep = "")
  
  print(paste("a =",a))
  print(Sys.time())
  # pause for 1.5 minutes
  Sys.sleep(120)
  print(Sys.time())
  
  download.file(url = api, 
                destfile = "C:/Users/Rose/rock_pool_bioblitz/data.zip", 
                mode = "wb")
  dfNBN = readr::read_csv(unz(
    description = "C:/Users/Rose/rock_pool_bioblitz/data.zip",
    filename = "data.csv"))
  dfNBN=as.data.frame(dfNBN)
  
  numRecords = length(dfNBN[,1])
  natbioblitz_nns$NBN_recs.2[a]=numRecords
  print(paste(natbioblitz_nns$taxon.common_name.name[a],"record =",natbioblitz_nns$NBN_recs.2[a]))
  rm(dfNBN)
}
## subsetted 0 records within 2 km
natbioblitz_nns1 = subset(natbioblitz_nns,
                          NBN_recs.2 == 0)
for (a in 1:length(natbioblitz_nns1[,1])) {
  binomClassNm = natbioblitz_nns1$taxon.name[a]
  binomClassNmSplit = strsplit(binomClassNm,"[ ]")
  genus = binomClassNmSplit[[1]][1]
  species = binomClassNmSplit[[1]][2]
  
  # Paste the genus and species names into the NBN Atlas API key and download relevant data
  lat = natbioblitz_nns1$latitude[a]
  lon = natbioblitz_nns1$longitude[a]
  
  api = paste("https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&fq=genus:",
              genus,
              "&lat=",
              lat,
              "&lon=",
              lon,
              "&radius=20.0&qa=none",
              "&fq=-occurrence_status%3A%22absent%22",
              "&fq=occurrence_status%3A%22present%22",
              "&fq=taxon_name%3A%22",
              genus,
              "%20",
              species,
              "%22",
              sep = "")
  
  print(paste("a =",a))
  print(Sys.time())
  # pause for 7 minutes
  Sys.sleep(420)
  print(Sys.time())
  
  download.file(url = api, 
                destfile = "C:/Users/Rose/rock_pool_bioblitz/data.zip", 
                mode = "wb")
  dfNBN = readr::read_csv(unz(
    description = "C:/Users/Rose/rock_pool_bioblitz/data.zip",
    filename = "data.csv"))
  dfNBN=as.data.frame(dfNBN)
  
  numRecords = length(dfNBN[,1])
  natbioblitz_nns1$NBN_recs.3[a]=numRecords
  print(paste(natbioblitz_nns1$taxon.common_name.name[a],"record =",natbioblitz_nns1$NBN_recs.3[a]))
  rm(dfNBN)
}
#

library(geosphere)
lon1 = as.numeric(lon)
lat1 = as.numeric(lat)
lon2 = as.numeric(BDiegensis20$`Longitude (WGS84)`[2])
lat2 = as.numeric(BDiegensis20$`Latitude (WGS84)`[2])
closest = distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
#
# maps
## UK Map ##

uk_map <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(admin %in% c("United Kingdom", "Ireland",
                      "Jersey","France","Netherlands","Germany",
                      "Denmark","Belgium","Norway","Finland"))

# Create colour ramp for kernel density estimation of observations
# using The Rock Pool Project brand colours
fun_colour_range <- colorRampPalette(c("#FFFFFF","#00A6FB", "#4D56F5","#191D2D"))   
my_colours <- fun_colour_range(1000)  

# Setting up the layers to map
natbioblitz_nns1 = subset(natbioblitz_nns,
                            NBN_recs.2 == 0)
df = data.frame(as.numeric(natbioblitz_nns1$longitude),
                as.numeric(natbioblitz_nns1$latitude))
s = SpatialPoints(df)
obs_points <- st_as_sf(
  natbioblitz_nns1,
  coords = c("longitude", "latitude"),
  crs = 4326
)
POdf = subset(natbioblitz_nns,
                          taxon.common_name.name == "Pacific Oyster")
dev.new(width=5, height=8, unit="in", noRStudioGD = TRUE)
op = par(family = "mont", font.lab = 2)
plot(st_geometry(uk_map),border="#FFFFFF",axes=TRUE,xlim=c(-11,3),ylim=c(49,61),
     family="mont",col="#E4E6F6")

group = natbioblitz_nns1$taxon.common_name.name
colors <- c("#FFC0BF",
            "#4D56F5",
            "#00A6FB",
            "#F79824"
            )

plot(st_geometry(obs_points), pch=19, cex=1.2,
     col = colors[factor(group)],add = TRUE)
points(as.numeric(pacific_oyster_NBN$Longitude..WGS84.),
       as.numeric(pacific_oyster_NBN$Latitude..WGS84.),
       pch = 19,
       col="#00A6FB")
points(as.numeric(POdf$longitude),
       as.numeric(POdf$latitude),
       pch = 19,
       col="#F79824")
points(lon2,lat2)


# pos_vector <- rep(3, length(levels(factor(group))))
# pos_vector[levels(factor(group)) %in% c("Pacific Oyster", "Red Ripple Bryozoan", "San Diego Sea Squirt")] <- 4
# text(st_geometry(obs_points), labels=levels(factor(group)),
#      cex= 0.7)

legend("topleft", inset = c(0.01),
       legend = levels(factor(group)),
       pch = 19,
       col = colors[factor(group)],border = FALSE,box.lwd=0,box.col = "white")
title(main = "Non-native species over 10 km from NBN Atlas record",
      family="chivo",cex.main = 1)
# ggplot map
st_crs(uk_map)
################################################################################
# Maps of non native species - NBN and Bioblitz
################################################################################
nonNatives = unique(natbioblitz_nns$taxon.name)
dev.new(width=8, height=5, unit="in", noRStudioGD = TRUE)
op = par(mfrow=c(1,3),family = "mont", font.lab = 2,
         mar=c(3,3,3.5,0.5)+0.1,xpd=FALSE)
for (a in 1:length(nonNatives)) {
  binomClassNm = nonNatives[a]
  binomClassNmSplit = strsplit(binomClassNm,"[ ]")
  genus = binomClassNmSplit[[1]][1]
  species = binomClassNmSplit[[1]][2]
  
  # NBN Atlas record filter:
  #     - No unconfirmed, unconfirmed (not reviewed) or unconfirmed (plausible)
  #     - No absences
  api=paste("https://records-ws.nbnatlas.org/occurrences/search?",
            "q=*:*&fq=genus:",
            genus,
            "&fq=-(identification_verification_status%3A%22Unconfirmed%22%20OR",
            "%20identification_verification_status%3A%22Unconfirmed%20-%20",
            "not%20reviewed%22%20OR%20identification_verification_status%3A%22",
            "Unconfirmed%20-%20plausible%22)&fq=-occurrence_status%3A%22absent",
            "%22&fq=taxon_name%3A%22",
            genus,
            "%20",
            species,
            "%22&pageSize=10000",sep = "")
  taxonInfo = GET(api)
  taxonInfoContent = httr::content(taxonInfo, as = 'text')
  taxonInfoContentJSON = jsonlite::fromJSON(taxonInfoContent)
  df=taxonInfoContentJSON$occurrences
  print(paste(nonNatives[a],"=",length(df[,1])))
  bioblitzFiltered = subset(natbioblitz_nns,
                            taxon.name == binomClassNm)
  
  ############### Map the sightings from bioblitz and NBN Atlas ################
  plot(st_geometry(uk_map),border="#FFFFFF",axes=TRUE,
       xlim=c(-11,3),ylim=c(49,61),
       family="mont",col="#E4E6F6")
  points(as.numeric(df$decimalLongitude),
         as.numeric(df$decimalLatitude),
         pch = 19,
         col="#00A6FB")
  points(as.numeric(bioblitzFiltered$longitude),
         as.numeric(bioblitzFiltered$latitude),
         pch = 19,
         col="#F79824")
  title(main = paste(natbioblitz_nns$taxon.common_name.name[a]),
        family="chivo",cex.main = 1,line = -1)
  legend("top", ,inset = c(0.05),
         col = c("#00a6fb","#F79824"),
         legend=c("NBN",
                  "Bioblitz"), horiz = FALSE,cex = 0.9,pch=19,
         box.lwd=0.01,box.col = "#191d2d")
  mtext("Non-native Invasive Species Records",
        side = 3, line = -2, outer = TRUE,col = c("#191d2d"),
        font = 2,cex = 1.1,
        family="chivo")
  rm(taxonInfo,taxonInfoContent,taxonInfoContentJSON,df)
}
################################################################################
#End
################################################################################