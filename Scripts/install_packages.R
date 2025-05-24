# ==============================================
# INSTALLING REQUIRED R PACKAGES FOR THE COURSE
# ==============================================

# This script will guide you through the installation of essential R packages 
# for the "Using R to Explore BioBlitz Data" course. 
# These packages include: rinat, httr, knitr, and rmarkdown.

# --------------------------------------------------
# Step 1: Set your CRAN repository
# --------------------------------------------------
# Setting the CRAN repository ensures that all packages are downloaded 
# from a reliable source. You can change the URL to a CRAN mirror closer to you.
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# --------------------------------------------------
# Step 2: List the required packages
# --------------------------------------------------
# We will create a vector of package names for easy management.
required_packages <- c("rinat", "httr", "knitr", "rmarkdown", "lubridate", "leaflet", "scales")

# --------------------------------------------------
# Step 3: Check and install missing packages
# --------------------------------------------------
# This loop checks if each package is installed. If not, it installs the package.
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    message(paste("Installing", package, "..."))
    install.packages(package)
  } else {
    message(paste(package, "is already installed."))
  }
}

# --------------------------------------------------
# Step 4: Verify package installations
# --------------------------------------------------
# Load each package to verify that the installation was successful.
for (package in required_packages) {
  library(package, character.only = TRUE)
  message(paste(package, "loaded successfully."))
}

# --------------------------------------------------
# Step 5: Confirm installation paths
# --------------------------------------------------
# This command shows where each package was installed.
installed.packages()[required_packages, "LibPath"]

# --------------------------------------------------
# Additional Information:
# - rinat: Access data from iNaturalist.
# - httr: Perform HTTP requests for API access.
# - knitr: Create dynamic reports.
# - rmarkdown: Convert R scripts to HTML, PDF, or Word documents.
# - lubridate: working with time data
# - leaflet: create interative maps
# - scales: colour scales and ramps


message("All packages are successfully installed and loaded. You are ready to proceed!")

