## Session 2, Script 4 – Real-time National BioBlitz Update: Map of Observations

# ------------------------------
# Load required libraries
# ------------------------------
library(ggplot2)             # For making plots
library(dplyr)               # For data manipulation
library(sf)                  # For handling spatial data (simple features)
library(showtext)            # For custom Google Fonts
library(rnaturalearth)       # For downloading country borders
library(rnaturalearthdata)   # Extra map data for Natural Earth

# ------------------------------
# Set up fonts
# ------------------------------
# Add Montserrat for labels and Chivo for the title
font_add_google("Montserrat", "mont")  # General font
font_add_google("Chivo", "chivo")      # Used for plot title
showtext_auto()  # Automatically use these fonts in plots

# ------------------------------
# Load or download the latest iNaturalist data
# ------------------------------
saved_data_path <- "NatBioBlitz_iNat.RData"  # File where previously downloaded data is saved

# Check if RStudio prompt functionality is available
if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
library(rstudioapi)

# If saved data exists, give the user a choice to use it or download fresh data
if (file.exists(saved_data_path)) {
  user_choice <- if (isAvailable()) {
    showQuestion(
      title = "Use saved data?",
      message = "Saved iNaturalist data found. Would you like to use this (recommended), or download the latest data from iNaturalist?",
      ok = "Use saved data",
      cancel = "Download latest"
    )
  } else {
    TRUE  # If not using RStudio, default to using saved data
  }
  
  if (isTRUE(user_choice)) {
    message("Loading saved data...")
    load(saved_data_path)  # This will load the object 'NatBioBlitz_iNat'
  } else {
    message("Downloading latest data from iNaturalist...")
    source("scripts/new get project obs function.R")  # Custom script to fetch iNat data
    NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
    save(NatBioBlitz_iNat, file = saved_data_path)
    message("Latest data downloaded and saved.")
  }
} else {
  # If no saved data exists, automatically download the latest
  message("No saved data found. Downloading now...")
  source("scripts/new get project obs function.R")
  NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
  save(NatBioBlitz_iNat, file = saved_data_path)
  message("Data saved for future use.")
}

# ------------------------------
# Convert 'location' column into latitude and longitude
# ------------------------------
# 'location' field is a string like "50.12,-5.06" – we split it into two numbers
location_split <- strsplit(NatBioBlitz_iNat$location, ",")
location_df <- do.call(rbind, lapply(location_split, function(x) as.numeric(x)))
colnames(location_df) <- c("latitude", "longitude")  # Label the columns

# Add these new lat/lon values to the main dataset
NatBioBlitz_iNat <- cbind(NatBioBlitz_iNat, location_df)

# Remove any rows that failed to convert properly
NatBioBlitz_iNat <- NatBioBlitz_iNat %>%
  filter(!is.na(latitude), !is.na(longitude))

# Convert the data frame to a spatial object
obs_points <- st_as_sf(NatBioBlitz_iNat, coords = c("longitude", "latitude"), crs = 4326)

# ------------------------------
# Load a background map of the UK and Ireland
# ------------------------------
uk_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% c("United Kingdom", "Ireland"))  # Keep only UK and Ireland

# ------------------------------
# Create the map plot
# ------------------------------
plot4 <- ggplot() +
  geom_sf(data = uk_map, fill = "whitesmoke", colour = "grey50") +       # Draw base map
  geom_sf(data = obs_points, colour = "#0B6EF5", size = 5, alpha = 0.7) + # Plot observation points
  coord_sf(xlim = c(-11, 3), ylim = c(49.5, 61), expand = FALSE) +       # Zoom to UK
  labs(title = "Locations of National BioBlitz Observations (2025)") +   # Plot title
  theme_void(base_family = "mont") +                                     # Minimal theme with font
  theme(
    plot.title = element_text(
      family = "chivo", size = 36, face = "bold", hjust = 0.5,
      margin = margin(t = 10, b = 10)
    ),
    plot.margin = margin(10, 10, 10, 10)
  )

# ------------------------------
# Save the map as an image file
# ------------------------------
png("bioblitz_map_2025.png", width = 1600, height = 1000, res = 150)  # Set image size
plot4
dev.off()  # Close the graphics device and save the file
