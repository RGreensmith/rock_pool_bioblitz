## Session 2, Script 2 — Real-time National BioBlitz update: Infographic 2

# ------------------------------------------------------------
# PART 1: Load saved data or download fresh from iNaturalist
# ------------------------------------------------------------

library(utils)

# This file will store the downloaded iNaturalist observations
saved_data_path <- "NatBioBlitz_iNat.RData"

# Load RStudio prompt library (used for pop-up question)
if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
library(rstudioapi)

# Check if data already exists locally
if (file.exists(saved_data_path)) {
  
  # If running in RStudio, ask user what to do
  user_choice <- if (isAvailable()) {
    showQuestion(
      title = "Use saved data?",
      message = "Saved iNaturalist data found. Would you like to use this (recommended), or download the latest data from iNaturalist?",
      ok = "Use saved data",
      cancel = "Download latest"
    )
  } else {
    TRUE  # Fallback: default to using saved data
  }
  
  if (isTRUE(user_choice)) {
    message("✔️ Loading saved data...")
    load(saved_data_path)  # Loads object: NatBioBlitz_iNat
  } else {
    message("⬇️ Downloading latest data from iNaturalist...")
    source("scripts/new get project obs function.R")
    NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
    save(NatBioBlitz_iNat, file = saved_data_path)
    message("✅ Latest data saved for future use.")
  }
  
} else {
  # If no data exists locally, download it fresh
  message("No saved data found. Downloading now...")
  source("scripts/new get project obs function.R")
  NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
  save(NatBioBlitz_iNat, file = saved_data_path)
  message("✅ Data downloaded and saved.")
}

# ------------------------------------------------------------
# PART 2: Create bar plot of observations per participant
# ------------------------------------------------------------

library(ggplot2)   # For plotting
library(dplyr)     # For data transformation
library(showtext)  # For using Google Fonts

# Load fonts once (no need to re-download if already added)
font_add_google("Montserrat", "mont")
font_add_google("Chivo", "chivo")
showtext_auto()


# Create a summary table of number of records per observer
user_counts <- NatBioBlitz_iNat %>%
  group_by(user.login, user.name) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    # Use full name if available, otherwise default to iNat username
    display_name = ifelse(is.na(user.name) | user.name == "", user.login, user.name)
  ) %>%
  arrange(desc(n)) %>%
  mutate(display_name = factor(display_name, levels = display_name))  # lock order for plotting

# Build the bar chart
plot2 <- ggplot(user_counts, aes(x = display_name, y = n)) +
  geom_col(fill = "#0B6EF5", width = 0.7) +  # Blue bars
  geom_text(
    aes(label = n), 
    vjust = -0.6, 
    family = "mont", 
    size = 6
  ) +
  labs(
    title = "Records per Participant – National BioBlitz 2025",
    x = NULL,
    y = "Number of Records"
  ) +
  theme_minimal(base_family = "mont") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20),   # Slanted x labels
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 30),
    plot.title = element_text(
      size = 36, family = "chivo", face = "bold", hjust = 0.5,
      margin = margin(b = 20)
    ),
    plot.margin = margin(t = 20, r = 40, b = 30, l = 40)
  ) +
  ylim(0, max(user_counts$n) + 25)  # Space for value labels above bars

# ------------------------------------------------------------
# PART 3: Save the image
# ------------------------------------------------------------

# Save the plot to a PNG image
png("records_per_participant.png", width = 1600, height = 900, res = 150)
plot2
dev.off()

message("📊 Plot saved as 'records_per_participant.png'")
