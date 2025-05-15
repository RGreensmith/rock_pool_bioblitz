# Load required packages
library(httr)
library(jsonlite)

# Function to get the default image for a taxon
get_taxon_image <- function(taxon_name) {
  
  # Construct the API URL
  base_url <- "https://api.inaturalist.org/v1/taxa"
  query <- list(q = taxon_name, per_page = 1)
  
  # Send the GET request
  response <- GET(base_url, query = query)
  
  # Check if the request was successful
  if (response$status_code == 200) {
    
    # Parse the JSON response
    data <- fromJSON(content(response, as = "text"))
    
    # Check if any taxa were returned
    if (length(data$results) > 0) {
      
      # Extract the taxon data
      taxon <- data$results
      
      # Extract taxon ID, name, and default image
      taxon_id <- taxon$id
      taxon_name <- taxon$preferred_common_name
      image_url <- taxon$default_photo$medium_url
      
      # Display the information
      cat("Taxon ID:", taxon_id, "\n")
      cat("Taxon Name:", taxon_name, "\n")
      cat("Image URL:", image_url, "\n")
      
      # Return the image URL
      return(image_url)
      
    } else {
      cat("No taxon found for:", taxon_name, "\n")
      return(NULL)
    }
    
  } else {
    cat("Error: Request failed with status code", response$status_code, "\n")
    return(NULL)
  }
}

# Example usage
taxon_image <- get_taxon_image("Beadlet anemone")
