#Script to accompany Session on Using R to Explore iNaturalist Data - 13th May 2025


# ==============================================

# SESSION ONE: DOWNLOADING AND FILTERING iNaturalist DATA

# ==============================================

# Load Required Packages

library(rinat)
library(httr)
library(lubridate)

# Set Working Directory
# --------------------------------------------------
# Make sure your working directory is set to the 'natbioblitz2025_data_training' project root folder**, 
# not the 'Scripts' subfolder. Use getwd() to check:
getwd()

# If needed, set it manually:
# setwd("your_path/to/natbioblitz2025_data_training")
# Or use the RStudio top menu - go to 'Session' and 'Set Working Directory' - then 'Choose Directory'

# --------------------------------------------------

# Step 1: Load the Exported Dataset

# --------------------------------------------------

# Set the file path to the exported dataset
file_path <- "data/observations-569743.csv"

# Read the CSV file

exported_data <- read.csv(file_path)

# View the dataset

View(exported_data)

# Extract the last observation date from the exported dataset

last_update <- max(ymd_hms(exported_data$updated_at))
cat("Last update:", as.character(last_update), "\n")

# --------------------------------------------------

# Step 2: Download Data from the iNaturalist Project

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

# Step 3: Compare Exported Data and Downloaded Data

# --------------------------------------------------

# Identify updated and new records in the downloaded data

new_records <- subset(inat_data, !id %in% exported_data$id )
cat("Number of new records since export:", nrow(new_records), "\n")

browseURL(sample(new_records$uri, 1)) #check one out

updated_records <- subset(inat_data, updated_at > max(exported_data$updated_at) 
                          & id %in% exported_data$id)

cat("Number of updated records since export:", nrow(updated_records), "\n")

#view an updated record on iNaturalist
browseURL(sample(updated_records$uri, 1))



# --------------------------------------------------

# Step 4: Filtering Data

# --------------------------------------------------

# Filter by species

beadlet_data <- subset(inat_data, taxon.common_name.name == "Atlantic Beadlet Anemone")
nrow(beadlet_data)


# Filter by location (e.g., Falmouth)

falmouth_data <- subset(inat_data, place_guess == "Falmouth, UK")
nrow(falmouth_data)

# Filter by ID status (e.g., Needs ID)

research_grade_data <- subset(inat_data, quality_grade == "research")

nrow(research_grade_data)

# Filter by date range (e.g., records from 1st onwards)

may_records <- subset(inat_data, time_observed_at >= as_datetime("2025-05-01"))

nrow(may_records)


# --------------------------------------------------

# Step 5: Getting Images

# --------------------------------------------------

# Get images for records for first record

first_beadlet_record_images <- beadlet_data$photos[[1]] #multiple images for this record

browseURL(first_beadlet_record_images$large_url[1]) #first image for this record
browseURL(first_beadlet_record_images$large_url[2]) #second image for this record
browseURL(first_beadlet_record_images$square_url[1]) #thumbnail image for this record

# Get images for taxa
source("Scripts/Ben's R iNat functions.R")
default_beadlet_image <- get_taxon_image("Beadlet anemone")

browseURL(default_beadlet_image) #default image for this species


# --------------------------------------------------

# Step 6: Linking to Non-native Species List

# --------------------------------------------------

# Load the non-native species list

non_native_species <- read.csv("data/UK marine NNS.csv")

View(non_native_species)

# Match observations against non-native species list

natbioblitz_nns <- subset(inat_data, taxon.id  %in% non_native_species$inat_id)
cat("Number of non-native species records found:", nrow(natbioblitz_nns), "\n")

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

# 2. Bar Plot - Records per day (All Species)

# --------------------------------------------------

# Extract month from observed_on

recs_per_day <- table(inat_data$observed_on)

barplot(recs_per_day, names.arg = format(as.Date(names(recs_per_day)), "%d-%m"))

# --------------------------------------------------

# 3. Map of Non-native Species

# --------------------------------------------------

# Create a color palette based on scientific names
species_colors <- colorFactor(palette = hue_pal()(length(unique(natbioblitz_nns$taxon.name))),
                              domain = natbioblitz_nns$taxon.name)

leaflet(data = natbioblitz_nns) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(~as.numeric(longitude), ~as.numeric(latitude),
                   radius = 5,
                   color = ~species_colors(taxon.name),
                   popup = ~paste("Species:", taxon.name, " Date:", time_observed_at)) %>%
  addLegend("bottomright",
            colors = scales::hue_pal()(length(unique(natbioblitz_nns$taxon.name))),
            labels = unique(natbioblitz_nns$taxon.name),
            title = "Species")

# --------------------------------------------------

# Script Complete


