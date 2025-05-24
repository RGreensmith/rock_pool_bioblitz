## Session 2, Script 3 — Real-time National BioBlitz update: Infographic 3

# ------------------------------
# Load required packages
# ------------------------------
library(ggplot2)    # For plotting
library(dplyr)      # For data wrangling
library(showtext)   # To use Google Fonts in plots
library(utils)

# Load fonts
font_add_google("Montserrat", "mont")
font_add_google("Chivo", "chivo")
showtext_auto()

# ------------------------------
# Load or download the latest iNaturalist data
# ------------------------------
saved_data_path <- "NatBioBlitz_iNat.RData"

if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
library(rstudioapi)

if (file.exists(saved_data_path)) {
  user_choice <- if (isAvailable()) {
    showQuestion(
      title = "Use saved data?",
      message = "Saved iNaturalist data found. Would you like to use this (recommended), or download the latest data from iNaturalist?",
      ok = "Use saved data",
      cancel = "Download latest"
    )
  } else {
    TRUE
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

# ------------------------------
# Infograpghic 3 - Categorise observations by iconic rock pool taxa
# ------------------------------

# Load the lookup table for iconic taxa
RPP_iconic_taxa <- read.csv("Data/BRPC_iconic_taxa.csv", stringsAsFactors = FALSE)
RPP_iconic_taxa$iNat_code <- as.character(RPP_iconic_taxa$iNat_code)

# Create a named vector to map ancestry codes to taxon names
taxon_lookup <- setNames(RPP_iconic_taxa$Common, RPP_iconic_taxa$iNat_code)

# Define a function to search each ancestry string and return the first matching label
get_iconic_label <- function(ancestry_str) {
  ids <- unlist(strsplit(ancestry_str, "/"))
  match <- taxon_lookup[ids]
  label <- match[!is.na(match)][1]
  if (is.na(label)) "Other" else label
}

# Apply the function to all records
NatBioBlitz_iNat$iconic_taxon <- vapply(NatBioBlitz_iNat$taxon.ancestry, get_iconic_label, character(1))

# ------------------------------
# Count records by iconic taxon
# ------------------------------
iconic_counts <- NatBioBlitz_iNat %>%
  count(iconic_taxon, name = "n") %>%
  arrange(desc(n))

# ------------------------------
# Create the bar plot
# ------------------------------
plot3 <- ggplot(iconic_counts, aes(x = reorder(iconic_taxon, n), y = n, fill = iconic_taxon)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "National BioBlitz Records by Rock Pool Iconic Taxa",
    x = NULL,
    y = "Number of Records"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_family = "mont") +
  theme(
    plot.title = element_text(family = "chivo", size = 38, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    plot.margin = margin(t = 20, r = 30, b = 30, l = 30)
  )

# ------------------------------
# Save the plot
# ------------------------------
png("iconic_taxa_summary.png", width = 1600, height = 900, res = 150)
plot3
dev.off()
