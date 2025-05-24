## Session 2, Script 1 - Real time National BioBlitz update - Infographic One

# ------------------------------
# Load latest or saved iNaturalist data
# ------------------------------

# Load base utility functions
library(utils)

saved_data_path <- "NatBioBlitz_iNat.RData"
# Load required for RStudio prompt
if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
library(rstudioapi)

# Check if data file exists
if (file.exists(saved_data_path)) {
  
  # Show a yes/no dialog box in RStudio
  user_choice <- if (isAvailable()) {
    showQuestion(
      title = "Use saved data?",
      message = "Saved iNaturalist data found. Would you like to use this (recommended), or download the latest data from iNaturalist?",
      ok = "Use saved data",
      cancel = "Download latest"
    )
  } else {
    TRUE  # fallback to using saved data if not in RStudio
  }
  
  if (isTRUE(user_choice)) {
    message("Loading saved data...")
    load(saved_data_path)
    
  } else {
    message("Downloading latest data from iNaturalist...")
    source("scripts/new get project obs function.R")
    NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
    save(NatBioBlitz_iNat, file = saved_data_path)
    message("Latest data downloaded and saved.")
  }
  
} else {
  message("No saved data found. Downloading now...")
  source("scripts/new get project obs function.R")
  NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
  save(NatBioBlitz_iNat, file = saved_data_path)
  message("Data saved for future use.")
}

# Infographic One - data summary

## first save the latest summary data
## How many records?
n_recs <- nrow(NatBioBlitz_iNat)

## How many observers?
observers <- unique(NatBioBlitz_iNat$user.login)

n_observers <- length(observers)

#how many species

sp_dat <- subset(NatBioBlitz_iNat, taxon.rank == "species")

sp_list <- unique(sp_dat$taxon.name)

n_sp <- length(sp_list)

# Load required libraries
library(ggplot2)    # Core plotting library
library(ggtext)     # Allows rich text (e.g. markdown) in plot titles and labels
library(ggimage)    # Enables image placement inside ggplots
library(showtext)   # Allows use of custom fonts (e.g. Google Fonts)

# Load the RPP Google Font "Montserrat" and name it "mont" in R
font_add_google("Montserrat", "mont")

# Activate showtext so custom fonts are rendered in plots
showtext_auto()

# Create the dataset with three summary metrics and corresponding icons
summary_data <- data.frame(
  label = c("observers", "records", "Species"),  # Labels for the metrics
  value = c(23, 695, 195),                       # Actual numbers to display
  icon = c("icons/people.png",                  # Path to PNG icons (must exist)
           "icons/clipboard.png",
           "icons/crab.png")
)

# Start building the plot
plot1 <- ggplot(summary_data, aes(x = label, y = 1)) +
  
  # Add icons as images slightly above center
  geom_image(aes(image = icon, y = 1.2), size = 0.6) +  # `size` controls icon scale
  
  # Add the text values under each icon
  geom_text(
    aes(y = 0.6, label = paste0(value, " ", label)),  # e.g. "695 records"
    family = "mont",         # Use Montserrat font
    fontface = "bold",       # Bold text
    size = 12                # Text size (larger = more readable)
  ) +
  
  # Add a main title to the plot
  labs(title = "Latest National BioBlitz results") +
  
  # Adjust horizontal spacing between items
  scale_x_discrete(expand = expansion(add = 1)) +
  
  # Limit the vertical space shown on the plot
  coord_cartesian(ylim = c(0.5, 1.6)) +
  
  # Remove axes, gridlines, and other chart elements
  theme_void(base_family = "mont") +
  
  # Custom styling for the plot title and margins
  theme(
    plot.title = element_text(
      size = 50,        # Font size for the title
      face = "bold",    # Bold title
      hjust = 0.5,      # Center horizontally
      margin = margin(t = 0)  # Remove space above title
    ),
    plot.margin = margin(10, 10, 10, 10)  # Narrow outer margins
  )

# Save the plot as a PNG file (high-resolution, wide format)
png("NatBioSummaryDat.png", width = 1400, height = 450)

# Draw the plot to the file
plot1

# Close the PNG device to finish saving
dev.off()






