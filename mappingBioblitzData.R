# Script to produce maps and graphical outputs from
# iNaturalist Bioblitz rockpool data

# ==============================================

# SESSION ONE: DOWNLOADING AND FILTERING iNaturalist DATA

# ==============================================

# Load Required Packages

library(rinat)
library(httr)
library(lubridate)

# --------------------------------------------------

# Step 1: Download Data from the iNaturalist Project

# --------------------------------------------------

# Define project ID and API parameters

project_slug <- "brpc-national-bioblitz-2025-practice"

# Download data using the rinat package

inat_data <- get_inat_obs_project(project_slug)

# View the new dataset

View(inat_data)

# Convert observed_on to date-time for comparison

inat_data$updated_at <- ymd_hms(inat_data$updated_at)
inat_data$time_observed_at <- ymd_hms(inat_data$time_observed_at)

last_update <- max(inat_data$updated_at)
cat("Last update:", as.character(last_update), "\n")

# --------------------------------------------------

# Step 2: Filtering Data

# --------------------------------------------------

# Filter by species
beadlet_data <- subset(inat_data,
                        taxon.common_name.name == "Atlantic Beadlet Anemone")
nrow(beadlet_data)

# Filter by ID status (e.g., Needs ID)
research_grade_data <- subset(inat_data, quality_grade == "research")
nrow(research_grade_data)

# --------------------------------------------------

# Step 3: Linking to Non-native Species List

# --------------------------------------------------

# Load the non-native species list
non_native_species <- read.csv("data/UK marine NNS.csv")

# Match observations against non-native species list
natbioblitz_nns <- subset(inat_data, taxon.id %in% non_native_species$inat_id)
cat("Number of non-native species records found:", nrow(natbioblitz_nns), "\n")
View(natbioblitz_nns)
# --------------------------------------------------

# ==============================================

# FOLLOW-UP ANALYSIS SCRIPT - NON-NATIVE MARINE SPECIES

# ==============================================

library(leaflet)
library(scales)

# --------------------------------------------------

# 1. Bar Plot - Records per Species (Non-native Marine Species Only)

# --------------------------------------------------

species_count <- table(natbioblitz_nns$taxon.common_name.name)

barplot(sort(species_count, decreasing = T), cex.names = 0.8)

# --------------------------------------------------

# 3. Map of Non-native Species

# --------------------------------------------------

# Create a color palette based on scientific names
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

# --------------------------------------------------

# End