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

# View the new data set
View(inat_data)

# Convert observed_on to date-time for comparison
inat_data$updated_at <- ymd_hms(inat_data$updated_at)
inat_data$time_observed_at <- ymd_hms(inat_data$time_observed_at)

last_update <- max(inat_data$updated_at)
cat("Last update:", as.character(last_update), "\n")

# ------------------------------------------------------------------------------
#                     Adding World Register of Marine Species Data
# ------------------------------------------------------------------------------
install.packages("jsonlite")
install.packages("dplyr")
library(jsonlite)
library(dplyr)

cockle = GET('https://www.marinespecies.org/rest/AphiaRecordsByName/Cerastoderma%20edule?like=true&marine_only=false&extant_only=true&offset=1')
cockle_content = httr::content(cockle, as = 'text')
cockle_content_from_json = jsonlite::fromJSON(cockle_content)
cockle_order = cockle_content_from_json$order[1]

# ------------------------------------------------------------------------------
#        Add and fill columns for Order and Family to iNaturalist dataframe
# ------------------------------------------------------------------------------
# Adding new columns to inat_data for taxa Order and Family
taxon.order = rep(NA, times = length(inat_data[,1]))
taxon.family = rep(NA, times = length(inat_data[,1]))

inat_data = merge(inat_data,cbind(taxon.order,taxon.family))

# clean global environment
rm(taxon.order,taxon.family)

# functions to try in loop below (in case species is not in the WoRMS)
tryOrder = function (sppNm) {
  taxOrder =
    wm_records_taxamatch(name = inat_data$taxon.name[sppNm])[[1]]$order
  return(taxOrder)
}
tryFamily = function (sppNm) {
  taxFamily =
    wm_records_taxamatch(name = inat_data$taxon.name[sppNm])[[1]]$family
  return(taxFamily)
}
l = length(inat_data[,4])
# filling in new inat_data columns for taxa Order and Family
for (a in 1:l) {
    
    taxOrder = "NA"
    taxFamily = "NA"
    
    inat_data$taxon.order[a] = try(tryOrder(a))
    inat_data$taxon.family[a] = try(tryFamily(a))
    
    # fail-safe for filling rows
    rm(taxOrder,taxFamily)
  
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