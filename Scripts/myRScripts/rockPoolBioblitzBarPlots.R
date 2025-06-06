# Script to produce bar plots from iNaturalist Bioblitz rock pool data of the
# number of native and non-native species (marine, brackish and freshwater)
# records across the taxa that have been identified to species level

# ==============================================================================
#                 DOWNLOADING AND FILTERING DATA
# ==============================================================================
# Load Required Packages
library(rinat)     # using wrapper for downloading data
library(httr)      # for getting data using inat and WoRMS API's
library(lubridate) # for date conversion
library(jsonlite)  # for getting WoRMS data
library(dplyr)     # for WoRMS data
# ------------------------------------------------------------------------------
#                      Download Data from the iNaturalist Project
# ------------------------------------------------------------------------------
# Define project ID and API parameters
project_slug <- "brpc-national-bioblitz-2025-practice"

# Download data using the rinat package
inat_data <- get_inat_obs_project(project_slug)

# Convert observed_on to date-time for comparison
inat_data$updated_at <- ymd_hms(inat_data$updated_at)
inat_data$time_observed_at <- ymd_hms(inat_data$time_observed_at)

last_update <- max(inat_data$updated_at)
cat("Last update:", as.character(last_update), "\n")

# ------------------------------------------------------------------------------
#                     Adding World Register of Marine Species Data
# ------------------------------------------------------------------------------
# Create new empty columns to merge with inat_data for taxonomy data
taxon.kingdom = rep(NA, times = length(inat_data[,1]))
taxon.phylum = rep(NA, times = length(inat_data[,1]))
taxon.class = rep(NA, times = length(inat_data[,1]))
taxon.order = rep(NA, times = length(inat_data[,1]))
taxon.family = rep(NA, times = length(inat_data[,1]))
marine = rep(NA, times = length(inat_data[,1]))
brackish = rep(NA, times = length(inat_data[,1]))
freshwater = rep(NA, times = length(inat_data[,1]))
terrestrial = rep(NA, times = length(inat_data[,1]))

# combine the inat_data dataframe with the new taxon columns
inat_data = cbind(
  inat_data,taxon.kingdom,taxon.phylum,taxon.class,taxon.order,taxon.family,
  marine,brackish,freshwater,terrestrial)

# clean up the global environment
rm(taxon.kingdom,taxon.phylum,taxon.class,taxon.order,taxon.family,
   marine,brackish,freshwater,terrestrial)

# filling in new inat_data columns with taxonomic information from WoRMS
l = length(inat_data[,1])
for (a in 1:l) {
  if (
    is.na(inat_data$taxon.rank[a])==FALSE && inat_data$taxon.rank[a]=="species"
    ){
    
    binomClassNm = inat_data$taxon.name[a]
    binomClassNmSplit = strsplit(binomClassNm,"[ ]")
    
    genus = binomClassNmSplit[[1]][1]
    species = binomClassNmSplit[[1]][2]
    
    api = paste("https://www.marinespecies.org/rest/AphiaRecordsByName/",
                genus,
                "%20",
                species,
                "?like=true&marine_only=false&extant_only=true&offset=1",
                sep = "")
    taxonInfo = GET(api)
    taxonInfoContent = httr::content(taxonInfo, as = 'text')
    
    if(object.size(taxonInfoContent)>112) {
      taxonInfoContentJSON = jsonlite::fromJSON(taxonInfoContent)
      
      inat_data$taxon.kingdom[a] = taxonInfoContentJSON$kingdom[1]
      inat_data$taxon.phylum[a] = taxonInfoContentJSON$phylum[1]
      inat_data$taxon.class[a] = taxonInfoContentJSON$class[1]
      inat_data$taxon.order[a] = taxonInfoContentJSON$order[1]
      inat_data$taxon.family[a] = taxonInfoContentJSON$family[1]
      
      if(is.na(taxonInfoContentJSON$isMarine[1])==FALSE) {
        inat_data$marine[a] = taxonInfoContentJSON$isMarine[1]
      }
      if(is.na(taxonInfoContentJSON$isBrackish[1])==FALSE) {
        inat_data$brackish[a] = taxonInfoContentJSON$isBrackish[1]
      }
      if(is.na(taxonInfoContentJSON$isFreshwater[1])==FALSE) {
        inat_data$freshwater[a] = taxonInfoContentJSON$isFreshwater[1]
      }
      if(is.na(taxonInfoContentJSON$isTerrestrial[1])==FALSE) {
        inat_data$terrestrial[a] = taxonInfoContentJSON$isTerrestrial[1]
      }
    } else {
      inat_data$taxon.kingdom[a] = "taxon info not retrieved"
    }
  }
}

# ------------------------------------------------------------------------------
#                   Linking to Non-native Species List
# ------------------------------------------------------------------------------

# Load the non-native species list
non_native_species <- read.csv("data/UK marine NNS.csv")

# Match observations against non-native species list

natbioblitz_nns <- subset(inat_data, taxon.id  %in% non_native_species$inat_id)
cat("Number of non-native species records found:", nrow(natbioblitz_nns), "\n")

################################################################################

### stacked bar plots #######
plotTitles=c("Class","Phylum","Kingdom")
png(file = paste(getwd(),"/stacked_bars.png",sep=""),
    width = 1050,height = 700,res=130)
par(mfrow=c(1,3))
for (a in 3:1){
  inat_data_filtered = inat_data
  inat_data_filtered = filter(inat_data_filtered,
                              marine == 1 | brackish == 1 | freshwater ==1)
  colRefINat_data_filtered=length(inat_data_filtered)-(5+a)
  taxonNames = sort(unique(
    inat_data_filtered[,colRefINat_data_filtered]))
  df = matrix(0,nrow=1,ncol=length(taxonNames))
  colnames(df)=taxonNames
  rownames(df)=c("nonNative")
  colRefNatbioblitz_nns = length(natbioblitz_nns)-(5+a)
  nativeTable = table(inat_data_filtered[,colRefINat_data_filtered])
  nonNativeTable = table(natbioblitz_nns[,colRefNatbioblitz_nns])
  for (b in 1:ncol(df)){
    for (c in 1:length(dimnames(nonNativeTable)[[1]])){
      if (colnames(df)[b] == dimnames(nonNativeTable)[[1]][c]){
        df[b] = nonNativeTable[c]
      }
    }
  }
  df = rbind(nativeTable,df)
  df2 = df
  for (d in 1:ncol(df)) {
    df[1,d] = df[1,d]-df[2,d]
  }
  if (a==3) {
    par(mar=c(8,5,7,5)+0.1,xpd=TRUE)
  } else if (a==2){
    par(mar=c(8,1,7,4)+0.1,xpd=TRUE)
  } else if (a==1){
    par(mar=c(8,3,7,2)+0.1,xpd=TRUE)
  }
  
  barplot(df, 
          col=c("#00a6fb","#F79824"), 
          horiz = TRUE, cex.names = 0.8,las = 1,border = FALSE, 
          space=0.04, 
          font.axis=1, 
          xlab="Number of records",
          col.lab =c("#191d2d"))
  axis(1,col="#191d2d")
  mtext(paste(plotTitles[a],sep=""),
        side = 3, adj = 0, line = -0.5,cex = 0.7,col=c("#191d2d"),font = 2)
  if(a==2){
    legend("topright", inset = c(0.15, 1.2),
           fill = c("#00a6fb","#F79824"),
           legend=c("Native species",
                    "Non-native species"))
  }
}
points(40, 40, pch = 16,col = "#ffc0be",cex=17)
points(160, 35, pch = 16,col = "#ffc0be",cex=30)
mtext("Number of non-terrestrial records identified to species level by rank",
      side = 3, line = -3, outer = TRUE,col = c("#0e6bff"),font = 2)
last_update <- max(inat_data$updated_at)
mtext(paste("Last update:",last_update,sep = ""),side = 3, line = -4.5, outer = TRUE,col = c("#0e6bff"),
      cex = 0.6,font = 3)
box("outer", col="#0e6bff",lwd=7)
dev.off()
################################################################################
# End
################################################################################