get_inat_obs_project_v2 <- function(project_slug,
                                    per_page = 200,
                                    max_pages = 10,
                                    quality_grade = NULL,
                                    place_id = NULL,
                                    taxon_id = NULL,
                                    d1 = NULL, d2 = NULL,
                                    verbose = TRUE,
                                    ...) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  all_results <- list()
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  for (page in 1:max_pages) {
    if (verbose) cat("Fetching page", page, "...\n")
    
    query <- list(
      project_id = project_slug,
      per_page = per_page,
      page = page,
      quality_grade = quality_grade,
      place_id = place_id,
      taxon_id = taxon_id,
      d1 = d1,
      d2 = d2,
      ...
    )
    
    # Remove NULL values to keep the URL clean
    query <- query[!sapply(query, is.null)]
    
    res <- GET(url = base_url, query = query)
    
    if (status_code(res) != 200) {
      warning("API request failed on page ", page)
      break
    }
    
    content_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (length(content_json$results) == 0) break
    
    all_results[[page]] <- content_json$results
  }
  
  # Combine all pages into a single data frame
  df <- bind_rows(all_results)
  return(df)
}
