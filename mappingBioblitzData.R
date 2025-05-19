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

# Merge the inat_data dataframe with the new taxon columns
inat_data = cbind(inat_data,
                  taxon.kingdom,
                  taxon.phylum,
                  taxon.class,
                  taxon.order,
                  taxon.family
                  )

# clean up the global environment
rm(taxon.kingdom,
   taxon.phylum,
   taxon.class,
   taxon.order,
   taxon.family
   )

# filling in new inat_data columns with taxonomic information from WoRMS
l = length(inat_data[,1])
for (a in 130:l) {
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
        
        kin = taxonInfoContentJSON$kingdom[1]
        phy = taxonInfoContentJSON$phylum[1]
        cls = taxonInfoContentJSON$class[1]
        ord = taxonInfoContentJSON$order[1]
        fam = taxonInfoContentJSON$family[1]
        
        inat_data$taxon.kingdom[a] = kin
        inat_data$taxon.phylum[a] = phy
        inat_data$taxon.class[a] = cls
        inat_data$taxon.order[a] = ord
        inat_data$taxon.family[a] = fam
      } else {
        inat_data$taxon.kingdom[a] = "taxon info not retrieved"
      }
    }
}

# ==============================================================================
#                                     ANEMONES
# ==============================================================================

# ------------------------------------------------------------------------------
#                       Step 1: Filtering Anemone Data
# ------------------------------------------------------------------------------
library(dplyr)

anemoneNms = c(
    "Atlantic Beadlet Anemone",
    "Strawberry Anemone",
    "Gem Anemone",
    "Daisy Anemone",
    "snakelocks anemone",
    "Dahlia Anemone",
    "Plumose Anemone",
    "Pimplet Anemone"
)
anemone_data <- filter(inat_data, taxon.common_name.name %in% anemoneNms)
nrow(anemone_data)

# ------------------------------------------------------------------------------
#                             Step 2: Map of Anemones
# ------------------------------------------------------------------------------
library(leaflet)
library(scales)

# Create a color palette based on common names
species_colors <- colorFactor(
    palette = hue_pal()(length(unique(anemone_data$taxon.common_name.name))),
    domain = anemone_data$taxon.common_name.name
)

leaflet(data = anemone_data) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addCircleMarkers(~ as.numeric(longitude), ~ as.numeric(latitude),
        radius = 5,
        color = ~ species_colors(taxon.common_name.name),
        popup = ~ paste("Species:", taxon.common_name.name, " Date:", time_observed_at)
    ) %>%
    addLegend("bottomright",
        colors = scales::hue_pal()(
          length(unique(anemone_data$taxon.common_name.name))
          ),
        labels = unique(anemone_data$taxon.common_name.name),
        title = "Species"
    )

# ------------------------------------------------------------------------------
#                 Step 3: Bar Plot - Records per Species (anemones)
# ------------------------------------------------------------------------------
anemone_data <- table(anemone_data$taxon.common_name.name)
barplot(sort(anemone_data, decreasing = T), horiz = TRUE, cex.names = 0.9)

# bar plot of records by Phyla
taxonPhylum <- table(inat_data$taxon.phylum)
barplot(sort(taxonPhylum, decreasing = T), horiz = TRUE, cex.names = 0.5,las = 2)

# ==============================================================================
#                             NON-NATIVE MARINE SPECIES
# ==============================================================================

# ------------------------------------------------------------------------------
#                   Step 1: Linking to Non-native Species List
# ------------------------------------------------------------------------------

# Load the non-native species list
non_native_species <- read.csv("data/UK marine NNS.csv")

# Match observations against non-native species list
natbioblitz_nns <- subset(inat_data, taxon.id %in% non_native_species$inat_id)
cat("Number of non-native species records found:", nrow(natbioblitz_nns), "\n")
View(natbioblitz_nns)

# ------------------------------------------------------------------------------
#                       Step 2: Map of Non-native Species
# ------------------------------------------------------------------------------

# Create a colour palette based on scientific names
species_colors <- colorFactor(
    palette = hue_pal()(length(unique(natbioblitz_nns$taxon.common_name.name))),
    domain = natbioblitz_nns$taxon.common_name.name
)

leaflet(data = natbioblitz_nns) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addCircleMarkers(~ as.numeric(longitude), ~ as.numeric(latitude),
        radius = 5,
        color = ~ species_colors(taxon.common_name.name),
        popup = ~ paste("Species:", taxon.common_name.name, " Date:", time_observed_at)
    ) %>%
    addLegend("bottomright",
        colors = scales::hue_pal()(length(unique(natbioblitz_nns$taxon.common_name.name))),
        labels = unique(natbioblitz_nns$taxon.common_name.name),
        title = "Species"
    )

# ------------------------------------------------------------------------------
#     Step 3: Bar Plot - Records per Species (Non-native Marine Species Only)
# ------------------------------------------------------------------------------

species_count <- table(natbioblitz_nns$taxon.common_name.name)
barplot(sort(species_count, decreasing = T),horiz = TRUE, cex.names = 0.9)

# ------------------------------------------------------------------------------
# End of script