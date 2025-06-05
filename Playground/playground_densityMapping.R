install.packages("eks")
install.packages("colorspace")
library(eks)
library(colorspace)
library(ggplot2)
library(dplyr)
install.packages("sf")
library(sf)
install.packages("ggthemes")
library(ggthemes)

## Grevillea data
data(grevilleasf, package="eks")
grevilleasf <- mutate(grevilleasf, species=factor(species))
paradoxa <- filter(grevilleasf, name %in% "Grevillea paradoxa")
eryngioides <- filter(grevilleasf, name %in% "Grevillea eryngioides")
grevillea_ep <- filter(grevilleasf, name %in% c("Grevillea eryngioides", 
                                                "Grevillea paradoxa"))
################################################################################
# inat data
################################################################################

library(rinat)     # using wrapper for downloading data
library(httr)      # for getting data using inat and WoRMS API's
library(lubridate) # for date conversion

# Define project ID and API parameters
project_slug <- "brpc-national-bioblitz-2025-practice"

# Download data using the rinat package
inat_data <- get_inat_obs_project(project_slug)

# Convert observed_on to date-time for comparison
inat_data$updated_at <- ymd_hms(inat_data$updated_at)
inat_data$time_observed_at <- ymd_hms(inat_data$time_observed_at)

last_update <- max(inat_data$updated_at)
cat("Last update:", as.character(last_update), "\n")

saved_data_path <- "../NatBioBlitz_iNat.RData"

if (file.exists(saved_data_path)) {
  load(saved_data_path)
} else {
  source("scripts/new get project obs function.R")
  NatBioBlitz_iNat <- get_inat_obs_project_v2("brpc-national-bioblitz-2025")
  save(NatBioBlitz_iNat, file = saved_data_path)
}

################################################################################
library(ggplot2)
library(dplyr)
library(ggtext)
library(ggimage)
library(showtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(utils)

location_split <- strsplit(NatBioBlitz_iNat$location, ",")
location_df <- do.call(
  rbind,
  lapply(location_split, function(x) as.numeric(x))
)
colnames(location_df) <- c("latitude", "longitude")

NatBioBlitz_iNat <- cbind(NatBioBlitz_iNat, location_df) %>%
  filter(!is.na(latitude), !is.na(longitude))

obs_points <- st_as_sf(
  NatBioBlitz_iNat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

uk_map <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(admin %in% c("United Kingdom", "Ireland","Channel Islands"))

plot4 <- ggplot() +
  geom_sf(data = uk_map, fill = "whitesmoke", colour = "grey50") +
  geom_sf(
    data = obs_points,
    colour = "#0B6EF5",
    size = 2,
    alpha = 0.6
  ) +
  coord_sf(
    xlim = c(-11, 3),
    ylim = c(49.5, 61),
    expand = FALSE
  ) +
  labs(title = "Locations of National BioBlitz Observations (2025)") +
  theme_void(base_family = "mont") +
  theme(
    plot.title = element_text(
      size = 18,
      family = "chivo",
      face = "bold",
      hjust = 0.5
    )
  )

plot4
################################################################################
grevillea_ep <- group_by(grevillea_ep, name)
xlim <- c(1.2e5, 1.1e6); ylim <- c(6.1e6, 7.2e6)

## WA polygon
data(wa, package="eks")
gwa <- geom_sf(data=wa, fill=NA, colour=1)

skde1 <- st_kde(paradoxa)

## base R contour plot
plot(st_geometry(wa), xlim=xlim, ylim=ylim)
plot(st_geometry(paradoxa), add=TRUE, pch=16, col=8, cex=0.5)
plot(skde1, add=TRUE, col=NA, border=1, legend=FALSE)

## R base filled contour plot
plot(st_geometry(wa), xlim=xlim, ylim=ylim)
plot(skde1, add=TRUE)

plot(st_geometry(uk_map))
skde1 <- st_kde(obs_points)
skde1 = density(obs_points$location,)

plot(skde1, add=TRUE)

plot(st_geometry(uk_map))
plot(st_geometry(obs_points), add=TRUE, pch=16, col=8, cex=0.5)
plot(skde1, add=TRUE, col=NA, border=1, legend=FALSE)

install.packages("MASS")
library(MASS)

library(raster)
install.packages("adehabitatHR")
library(adehabitatHR)

install.packages("viridis")  # Install
library("viridis")           # Load

df = data.frame(NatBioBlitz_iNat$longitude,NatBioBlitz_iNat$latitude)
s = SpatialPoints(df)
kde.output <- kernelUD(s,h="href", grid = 1000)
plot(kde.output,col=viridis(500, begin = 0, end = 1, direction = 1))
plot(st_geometry(obs_points), add=TRUE, pch=16, col=8, cex=0.5)
plot(st_geometry(uk_map),add = TRUE,border="black")

bounding_box = st_geometry(uk_map)
plot(bounding_box)

coords = matrix(c(78.46801, 19.53407,
                  78.46801, 19.74557,
                  78.83157, 19.74557,
                  78.83157, 19.53407,
                  78.46801, 19.53407), 
                ncol = 2, byrow = TRUE)


P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(Ps1, axes = TRUE)


e <- as(raster::extent(
  -9,
  4,
  45,
  65
  ), "SpatialPolygons")
proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e,axes = TRUE,add = TRUE)

# R program to create a color range
# Apply colorRampPalette Function
fun_color_range <- colorRampPalette(c("white","#0e6bff","#F79824", "#F79824"))   
my_colors <- fun_color_range(1000)  

# Plotting a graph
# plot(1:100, pch = 20, col = my_colors)
dev.off()

# plot(masked_kde2,col=viridis(500, begin = 0, end = 1, direction = 1),
#      xlim=c(-11,3),ylim=c(48.5,61.5),axes=TRUE)
# 
# plot(kde.output,col=terrain.colors(500,rev = TRUE),axes=TRUE,
#      xlim=c(-11,3),ylim=c(48.5,61.5))

plot(masked_kde2,col=my_colors,axes=TRUE,
     xlim=c(-11,3),ylim=c(48.5,61.5))

plot(st_geometry(uk_map),add = TRUE,border="#191d2d")
# contour(masked_kde2,add = TRUE,col="#191d2d")
plot(st_geometry(obs_points), add=TRUE, pch=24, col="#191d2d", bg="#00a6fb", cex=0.7)
title(main = "Observations")


masked_kde2 <- mask(kde, uk_map)

dev.off()
plot(masked_kde2,col=topo.colors(500,rev = TRUE),
     xlim=c(-11,3),ylim=c(50,61),axes=TRUE)
plot(st_geometry(obs_points), add=TRUE, pch=21, col="blue", cex=0.5)
plot(st_geometry(uk_map),add = TRUE,border="black")
contour(masked_kde2,add = TRUE,col="blue")

# converts to raster
kde <- raster(kde.output)
# sets projection to British National Grid
projection(kde) <- CRS("+init=EPSG:27700")
#######################


# End