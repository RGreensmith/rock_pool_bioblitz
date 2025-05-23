# Script to produce maps from iNaturalist Bioblitz rock pool data

# ==============================================================================
#            SESSION ONE: DOWNLOADING AND FILTERING iNaturalist DATA
# ==============================================================================
# Load Required Packages
library(rinat)
library(httr)
library(lubridate)

# ------------------------------------------------------------------------------
#                      Download Data from the iNaturalist Project
# ------------------------------------------------------------------------------
# Define project ID and API parameters
project_slug <- "brpc-national-bioblitz-2025-practice"

# Download data using the rinat package
inat_data <- get_inat_obs_project(project_slug)

# Convert observed_on to date-time for comparison
inat_data$updated_at <- ymd_hms(inat_data$updated_at)
inat_data$time_observed_at <- ymd_hms(inat_data$time_observed_at)

last_update <- max(inat_data$updated_at)
cat("Last update:", as.character(last_update), "\n")

# ------------------------------------------------------------------------------
#                     Adding World Register of Marine Species Data
# ------------------------------------------------------------------------------
library(jsonlite)
library(dplyr)

# ------------------------------------------------------------------------------
#        Add and fill columns for Order and Family to iNaturalist dataframe
# ------------------------------------------------------------------------------
# Create new empty columns to merge with inat_data for taxonomy data
taxon.kingdom = rep(NA, times = length(inat_data[,1]))
taxon.phylum = rep(NA, times = length(inat_data[,1]))
taxon.class = rep(NA, times = length(inat_data[,1]))
taxon.order = rep(NA, times = length(inat_data[,1]))
taxon.family = rep(NA, times = length(inat_data[,1]))
marine = rep(NA, times = length(inat_data[,1]))
brackish = rep(NA, times = length(inat_data[,1]))
freshwater = rep(NA, times = length(inat_data[,1]))
terrestrial = rep(NA, times = length(inat_data[,1]))

# combine the inat_data dataframe with the new taxon columns
inat_data = cbind(inat_data,
                  taxon.kingdom,
                  taxon.phylum,
                  taxon.class,
                  taxon.order,
                  taxon.family,
                  marine,
                  brackish,
                  freshwater,
                  terrestrial
                  )

# clean up the global environment
rm(taxon.kingdom,
   taxon.phylum,
   taxon.class,
   taxon.order,
   taxon.family,
   marine,
   brackish,
   freshwater,
   terrestrial
   )

# filling in new inat_data columns with taxonomic information from WoRMS
l = length(inat_data[,1])
for (a in 1:l) {
    if (is.na(inat_data$taxon.rank[a])==FALSE && inat_data$taxon.rank[a]=="species"){
      
      binomClassNm = inat_data$taxon.name[a]
      binomClassNmSplit = strsplit(binomClassNm,"[ ]")

      genus = binomClassNmSplit[[1]][1]
      species = binomClassNmSplit[[1]][2]
      
      api = paste("https://www.marinespecies.org/rest/AphiaRecordsByName/",
                  genus,
                  "%20",
                  species,
                  "?like=true&marine_only=false&extant_only=true&offset=1",sep = "")
      taxonInfo = GET(api)
      taxonInfoContent = httr::content(taxonInfo, as = 'text')
      
      if(object.size(taxonInfoContent)>112) {
        taxonInfoContentJSON = jsonlite::fromJSON(taxonInfoContent)
        
        inat_data$taxon.kingdom[a] = taxonInfoContentJSON$kingdom[1]
        inat_data$taxon.phylum[a] = taxonInfoContentJSON$phylum[1]
        inat_data$taxon.class[a] = taxonInfoContentJSON$class[1]
        inat_data$taxon.order[a] = taxonInfoContentJSON$order[1]
        inat_data$taxon.family[a] = taxonInfoContentJSON$family[1]
        
        if(is.na(taxonInfoContentJSON$isMarine[1])==FALSE) {
          inat_data$marine[a] = taxonInfoContentJSON$isMarine[1]
        }
        if(is.na(taxonInfoContentJSON$isBrackish[1])==FALSE) {
          inat_data$brackish[a] = taxonInfoContentJSON$isBrackish[1]
        }
        if(is.na(taxonInfoContentJSON$isFreshwater[1])==FALSE) {
          inat_data$freshwater[a] = taxonInfoContentJSON$isFreshwater[1]
        }
        if(is.na(taxonInfoContentJSON$isTerrestrial[1])==FALSE) {
          inat_data$terrestrial[a] = taxonInfoContentJSON$isTerrestrial[1]
        }
      } else {
        inat_data$taxon.kingdom[a] = "taxon info not retrieved"
      }
    }
}

# ==============================================================================
# Bar plots of number of records identified to species level across taxonomic ranks
# ==============================================================================

inat_data_filtered = inat_data
inat_data_filtered = filter(inat_data_filtered,marine == 1 | brackish == 1)
inat_data_filtered = filter(inat_data_filtered,taxon.class == "Gastropoda")

par(mfrow=c(1,3),mar=c(2,5,2,1)+0.1)

df <- table(inat_data_filtered$taxon.kingdom)
barplot(sort(df, decreasing = F),
        horiz = TRUE, cex.names = 0.9,las = 1,border = FALSE,
        main = "Records per Kingdom",col = "lightblue")

df <- table(inat_data_filtered$taxon.phylum)
par(mar=c(2,5,2,1)+0.1)
barplot(sort(df, decreasing = F),
        horiz = TRUE, cex.names = 0.9,las = 1,border = FALSE,
        main = "Records per Phylum",col = "lightblue")

df <- table(inat_data_filtered$taxon.class)
barplot(sort(df, decreasing = F),
        horiz = TRUE, cex.names = 0.7,las = 1,border = FALSE,
        main = "Records per Class",col = "lightblue")



#### making class filtered plots ####
inat_data_filtered = inat_data
inat_data_filtered = filter(inat_data_filtered,
                            marine == 1 | brackish == 1)
taxonPhylumNames = unique(inat_data_filtered$taxon.phylum)


for (a in 1:length(taxonPhylumNames)){
  inat_data_filtered = inat_data
  inat_data_filtered = filter(inat_data_filtered,
                              marine == 1 | brackish == 1)
  inat_data_filtered = filter(inat_data_filtered,
                              taxon.phylum == taxonPhylumNames[a])
  print(paste("a =",taxonPhylumNames[a]))
  pdf(file = paste(getwd(),"/",taxonPhylumNames[a],".pdf",sep=""),   # The directory you want to save the file in
      width = 7.9, # The width of the plot in inches
      height = 7.5) # The height of the plot in inches
  
  taxonClassNames = unique(inat_data_filtered$taxon.class)
  if (length(taxonClassNames)/3>=1){
    par(mfrow=c(length(taxonClassNames)/3,3))
  } else if (length(taxonClassNames)==2) {
    par(mfrow=c(1,2))
  } else {
    par(mfrow=c(1,1))
  }
  
  for (b in 1:length(taxonClassNames)) {
    inat_data_filtered2 = filter(inat_data_filtered,
                                taxon.class == taxonClassNames[b])
    if (is.na(inat_data_filtered2$taxon.common_name.name[1])==FALSE){
      df <- table(inat_data_filtered2$taxon.common_name.name)
    } else {
      df <- table(inat_data_filtered2$taxon.name)
    }
    
    par(mar=c(2,10,2,1)+0.1)
    barplot(sort(df, decreasing = F),
            horiz = TRUE, cex.names = 0.7,las = 1,border = FALSE,
            main = paste(taxonClassNames[b],"spp.",sep = " "),
            col = "lightblue")
  }
  dev.off()
  rm(taxonClassNames)
}

###

####### map ##################
# Create a color palette based on common names
species_colors <- colorFactor(
  palette = hue_pal()(length(unique(inat_data_filtered$taxon.phylum))),
  domain = inat_data_filtered$taxon.phylum
)

# Filter anemone data
anemone_data <- filter(inat_data_filtered, taxon.order %in% "Actiniaria")
nrow(anemone_data)

# Create a color palette based on common names
anemone_colours <- colorFactor(
  palette = hue_pal()(length(unique(anemone_data$taxon.common_name.name))),
  domain = anemone_data$taxon.common_name.name
)


map <- leaflet() %>%
  # Base groups
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
  addProviderTiles(providers$OpenStreetMap,group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # Overlay groups
  addCircleMarkers(~ as.numeric(anemone_data$longitude), ~ as.numeric(anemone_data$latitude),
                   radius = 1,
                   color = ~ anemone_colours(taxon.common_name.name),
                   popup = ~ paste("Species:", taxon.common_name.name, " Date:",
                                   time_observed_at),group = "anemone"
  )%>%
  addCircleMarkers(~ as.numeric(longitude), ~ as.numeric(latitude),
             radius = 1,
             color = ~ species_colors(taxon.phylum),
             popup = ~ paste("Species:", taxon.phylum, " Date:",
                             time_observed_at),group = "b"
  ) %>%
  # Layers control
  addLayersControl(
    baseGroups = c(
      "World Imagery (satellite)",
      "Open Street Map",
      "Esri OceanBasemap"
    ),
    overlayGroups = c("anemone", "b"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addLegend("bottomright",
            colors = scales::hue_pal()(
              length(unique(anemone_data$taxon.common_name.name))),
            labels = unique(anemone_data$taxon.common_name.name),
            title = "Species"
  )
map
# addLegend("bottomright",
#           colors = scales::hue_pal()(
#             length(unique(inat_data_filtered$taxon.phylum))),
#           labels = unique(inat_data_filtered$taxon.phylum),
#           title = "Species"
# ------------------------------------------------------------------------------
#                             Step 1: Map of Anemones
# ------------------------------------------------------------------------------
library(leaflet)
library(scales)

inat_data_filtered = inat_data
inat_data_filtered = filter(inat_data_filtered,marine == 1 | brackish == 1 | freshwater == 1)
# Filter anemone data
anemone_data <- filter(inat_data_filtered, taxon.class %in% "Gastropoda")

# Create a color palette based on common names
species_colors <- colorFactor(
  palette = hue_pal()(length(unique(anemone_data$taxon.name))),
  domain = anemone_data$taxon.name
)

leaflet(data = anemone_data) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
  addProviderTiles(providers$OpenStreetMap,group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(~ as.numeric(longitude), ~ as.numeric(latitude),
                   radius = 5,
                   color = ~ species_colors(taxon.name),
                   popup = ~ paste("Species:", taxon.name, " Date:", time_observed_at)
  ) %>%
  # Layers control
  addLayersControl(
    baseGroups = c(
      "World Imagery (satellite)",
      "Open Street Map",
      "Esri OceanBasemap"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addLegend("bottomright",
            colors = scales::hue_pal()(
              length(unique(anemone_data$taxon.name))
            ),
            labels = unique(anemone_data$taxon.name),
            title = "Species"
  )

# ------------------------------------------------------------------------------
# End of script