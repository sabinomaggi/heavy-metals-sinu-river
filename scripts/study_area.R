#---------------------------------------
# Clear the RStudio environment
#---------------------------------------
# clear workspace, console and plots if script is run in Rstudio
if (Sys.getenv("RSTUDIO") == "1") {
    rm(list = ls())     # clear workspace
    cat("\014")         # clear console
    graphics.off()      # clear plots
}
#---------------------------------------

#---------------------------------------
# Load the libraries
#---------------------------------------
# library(magrittr)       # pipe operator for R
library(tidyverse)      # load all tidyverse stuff (most packages are used here, so why not?)
library(stringr)        # string manipulation

library(ggplot2)        # what else?
library(viridis)        # other color scales

library(ggmap)          # retrieve map info and map tiles from popular online mapping services
                        # such as Google Maps and Stamen Maps

library(sp)             # classes and methods for spatial data
library(sf)             # simple spatial features for R
library(tmap)           # thematic maps
library(mapview)        # interactive viewing of spatial data
# webshot::install_phantomjs()
# library(rworldmap)      # map of the world

library(leaflet)            # dynamic maps
library(leaflet.extras2)    # extra leaflet functionalities
library(leafem)         # leaflet extensions from mapview
#---------------------------------------


#---------------------------------------
# Main constants
#---------------------------------------
# OS-specific constants
SEPARATOR <- .Platform$file.sep

# define the directory that contains the source files
SRC <- dirname(rstudioapi::getActiveDocumentContext()$path)

# set timezone
Sys.setenv(TZ = "CET")
#---------------------------------------


#---------------------------------------
# Set user-defined parameters
#---------------------------------------

shapes <- c(21, 22, 23, 24, 25)
lines  <- c("solid", "dashed", "dashed")
colors <- c("#00BFC4", "#F8766D", "#F8766D")
colors <- c("#00BA38", "#F8766D", "#F8766D")
col_annotations <- c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7")


# for security reasons, the user's API key is stored in the GOOGLE_API_KEY environment variable;
# to set it, open a terminal and run the command: export GOOGLE_API_KEY="your_api_key_here"
# or add the following line to your .Renviron file (usually located in your home directory):
# GOOGLE_API_KEY="your_api_key_here"
api_key <- Sys.getenv("GOOGLE_API_KEY")

register_google(key = api_key)
#---------------------------------------

#---------------------------------------
# Utility functions
#---------------------------------------

#---------------------------------------
# Convert column names to UPPERCASE
colnames.toupper <- function(df) {

    # convert column names to uppercase
    column_names <- names(df) %>% toupper()
    names(df) <- column_names

    return(df)
}

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

#---------------------------------------


#===========================================================
# Stage #0: Setup work environment
#===========================================================

# the main project directory is the parent of the source directory
PROJECT <- file.path(SRC, "..") %>% normalizePath()
setwd(PROJECT)

# define directories containing the data files 
# (all these directories must already exist!)
DATA <- file.path(PROJECT, "data") %>% normalizePath()
INPUT <- file.path(DATA)

# define directories for output files 
OUTPUT <- file.path(PROJECT, "figures") %>% normalizePath()

#---------------------------------------


#===========================================================
# Stage #1: Read and preprocess data files
#===========================================================

#---------------------------------------
# Read and preprocess the data file(s) containing the 
# coordinates of the sampling sites
#---------------------------------------

data_file <- "sampling-locations.csv"
data_filename <- file.path(DATA, data_file)
sl <- read_csv(data_filename, skip = 0, trim_ws = TRUE, ) %>% 
    colnames.toupper()

sl <- sl %>% dplyr::rename(LAT = LATITUDE, LON = LONGITUDE)
sl


#===========================================================
# Stage #2: Plot map of Colombia with locations of sampling sites
#===========================================================

# download map of region under study
region <- geocode("Colombia", output = "more", source = "google")

# calculate min/max coordinates and center of viewport
min_coords <- tibble(min(sl$LAT), min(sl$LON))
names(min_coords) <- c("LAT", "LON")

max_coords <- tibble(max(sl$LAT), max(sl$LON))
names(max_coords) <- c("LAT", "LON")

center_coords <- tibble(
    min_coords$LAT + (max_coords$LAT - min_coords$LAT) / 2, 
    min_coords$LON + (max_coords$LON - min_coords$LON) / 2)
names(center_coords) <- c("LAT", "LON")


# plot map with location markers
pal <- gg_color_hue(25)
map_sl <- leaflet(options = leafletOptions(attributionControl = FALSE, zoomControl = FALSE)) %>%
    addTiles() %>%
    addProviderTiles("OpenStreetMap") %>%
    setView(lng = center_coords$LON, lat = center_coords$LAT, zoom = 8) %>%
    fitBounds(lng1 = min_coords$LON + 0.0, lat1 = min_coords$LAT + 0.0,
              lng2 = max_coords$LON - 0.0, lat2 = max_coords$LAT - 0.0) %>%
    addSimpleGraticule(interval = 0.25) %>% 
    # addGraticule(interval = 0.1) %>%
    addCircleMarkers(
        lng = sl$LON, lat = sl$LAT,
        radius = 20,
        stroke = TRUE, color = "black", weight = 1,
        fillColor = pal, fillOpacity = 0.5,
    ) %>% 
    addLabelOnlyMarkers(lng = sl$LON, lat = sl$LAT,
                    label = sl$LOCATION,
                    labelOptions = labelOptions(noHide = TRUE, direction = "center",
                                                textOnly = TRUE, textsize = "16px",
                                                style = list("border" = "solid 0px",
                                                          "padding" = "0px",
                                                          "left" = "-40px",
                                                          "top" = "0px"
                                                          ),
                                                opacity = 1.0,
                                                )
                    )

map_sl


map_file  <- "sampling-sites.pdf"
map_filename  <- file.path(OUTPUT, map_file)

# for some reasons mapshot does not respect positions of labels
#
# therefore it is BETTER to save the map manually
# Export -> save as image... -> 1200 x 1200 -> Update preview
# then zoom once and move map to right position
#
# mapshot(map_sl, file = map_filename, selfcontained = FALSE,
#         # vheight = 1080, vwidth = 1080, 
#         vheight = 800, vwidth = 800,
#         remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar", 
#                             "drawToolbar", "easyButton"))


#---------------------------------------
# intermediate map of caribbean region
# fill_col   <- c("#830014", "orange", "cyan", "green")
pal <- gg_color_hue(25)
border_col <- gg_color_hue(25)

# mapviewOptions(basemaps = "OpenStreetMap", 
mapviewOptions(basemaps = "Esri.WorldStreetMap", 
               homebutton = FALSE, legend = FALSE, fgb = FALSE)

sf_sl <- st_as_sf(sl, coords = c("LON", "LAT"), crs = 4326)

map_caribe <- mapview(sf_sl[1,], cex = 12, lwd = 1,
                      color = border_col, col.regions = pal, 
                      alpha.regions = 0.5, layer.name = "This work") 

map_caribe

map_caribe@map <- map_caribe@map %>% setView(lat = 11.0, lng = -69.0, zoom = 6)
map_caribe

map_file  <- "map-caribe.pdf"
map_file_with_path  <- file.path(OUTPUT, map_file)


# for some reasons mapshot does not respect positions of labels
#
# therefore it is BETTER to save the map manually
# Export -> save as image... -> 1200 x 1200 -> Update preview
# then zoom once and move map to right position
#
# mapshot(map_caribe, file = map_file_with_path, selfcontained = FALSE,
#         # vheight = 1080, vwidth = 820, 
#         vheight = 1080, vwidth = 1920, 
#         remove_controls = c("zoomControl", "layersControl", "homeButton", # "scaleBar", 
#                             "drawToolbar", "easyButton"))
#---------------------------------------


#===========================================================
# Cleanup
#===========================================================

# unload packages that may conflict with other scripts
detach("package:ggmap", unload = TRUE)

