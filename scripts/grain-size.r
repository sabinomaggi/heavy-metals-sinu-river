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
library(tidyverse)              # load all the tidyverse environment
library(yaml)                   # read yaml files
#---------------------------------------

#---------------------------------------
# Main constants
#---------------------------------------
# OS-specific constants
SEPARATOR <- .Platform$file.sep

# ggplot2 setup
scales::comma_format(big.mark = ';', decimal.mark = '.')
#---------------------------------------


#===========================================================
# Setup work environment
#===========================================================

# get the directory containing the current R script
SCRIPTS <- dirname(rstudioapi::getActiveDocumentContext()$path)

# set the main project directory
PROJECT <- file.path(SCRIPTS, "..") %>% normalizePath()
setwd(PROJECT)

# define the directory containing the data files
DATA <- file.path(PROJECT, "data") %>% normalizePath()

# define the directory for the output files
OUTPUT <-  file.path(PROJECT, "figures") %>% normalizePath()
#---------------------------------------

#---------------------------------------
# define a common theme to be used in most plots below
# 'hm' stands for 'heavy metals'
theme_hm <- function(){
    font <- "Georgia"                       # assign font family up front
    theme_bw(base_size = 12) %+replace%     # replace elements we want to change
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "right",
          # legend.title = element_text(size = 12),
          # legend.text = element_text(size = 12),
          strip.text = element_text(size = 12, face = "plain"),
          strip.background = element_rect(fill = "#FAFAFA", linewidth = 0.1, linetype = "solid"),
    )
}
#---------------------------------------


#===========================================================
# Read the data files
#===========================================================

#---------------------------------------
# Read the yaml file containing the metadata
#---------------------------------------
filename <- file.path(DATA, "metadata.yml")
meta <- read_yaml(filename)
head(meta)
#---------------------------------------


#---------------------------------------
# Read the data file containing the grain-size measurements
#---------------------------------------
filename <- file.path(DATA, "grain-size.csv")
grainsize <- read.csv(filename, header = TRUE, strip.white = TRUE) %>% as_tibble()
grainsize
#---------------------------------------


#===========================================================
# Fix/rearrange the dataset(s)
#===========================================================

#---------------------------------------
# 3. grain-size measurements
#---------------------------------------

seas <- tibble(Season = c("dry", "rainy"))

# extract each measurement type and create a separate column containing the position

clay <- grainsize %>% select(c("Location") | starts_with("Clay")) %>% pivot_longer(c(Clay_surface, Clay_deep), names_to = "Position", values_to = "Clay", names_prefix = "Clay_")

silt <- grainsize %>% select(c("Location") | starts_with("Silt")) %>% pivot_longer(c(Silt_surface, Silt_deep), names_to = "Position", values_to = "Silt", names_prefix = "Silt_")

sand <- grainsize %>% select(c("Location") | starts_with("Sand")) %>% pivot_longer(c(Sand_surface, Sand_deep), names_to = "Position", values_to = "Sand", names_prefix = "Sand_")

# join all measurements in a single tibble
grainsize <- clay %>% left_join(silt) %>% left_join(sand)

# combine dataset with (two) possible seasons
grainsize <- grainsize %>% cross_join(seas)

# reorder rows by Position column
grainsize <- grainsize %>% arrange(Position) 
grainsize$Position <- factor(grainsize$Position, levels = c("surface", "deep"))
grainsize



#===========================================================
# Build the Shepards Diagram
#===========================================================

# Construction of Shepards Diagram
library(ggtern)
library(plyr)

# Build a library of points, left to right, top to bottom...
points <- data.frame(
    rbind(c( 1,1.000,0.000,0.000),
          c( 2,0.750,0.250,0.000),
          c( 3,0.750,0.125,0.125),
          c( 4,0.750,0.000,0.250),
          c( 5,0.600,0.200,0.200),
          c( 6,0.500,0.500,0.000),
          c( 7,0.500,0.000,0.500),
          c( 8,0.400,0.400,0.200),
          c( 9,0.400,0.200,0.400),
          c(10,0.250,0.750,0.000),
          c(11,0.250,0.000,0.750),
          c(12,0.200,0.600,0.200),
          c(13,0.200,0.400,0.400),
          c(14,0.200,0.200,0.600),
          c(15,0.125,0.750,0.125),
          c(16,0.125,0.125,0.750),
          c(17,0.000,1.000,0.000),
          c(18,0.000,0.750,0.250),
          c(19,0.000,0.500,0.500),
          c(20,0.000,0.250,0.750),
          c(21,0.000,0.000,1.000)            )
)
colnames(points) = c("IDPoint","T","L","R")

# Give each Polygon a number
polygon.labels <- data.frame(Label=c("Clay","Sandy Clay","Silty Clay",
                                     "Sand + Silt + Clay","Clayey Sand","Clayey Silt",
                                     "Sand","Silty Sand","Sandy Silt","Silt"))
polygon.labels$IDLabel=1:nrow(polygon.labels)

# Create a map of polygons to points
polygons <- data.frame(
    rbind(c(1,1),c(1,2),c(1,4),
          c(2,6),c(2,2),c(2,3),c(2,5),c(2,8),
          c(3,3),c(3,4),c(3,7),c(3,9),c(3,5),
          c(4,5),c(4,14),c(4,12),
          c(5,6),c(5,8),c(5,12),c(5,15),c(5,10),
          c(6,7),c(6,11),c(6,16),c(6,14),c(6,9),
          c(7,17),c(7,10),c(7,18),
          c(8,15),c(8,12),c(8,13),c(8,19),c(8,18),
          c(9,13),c(9,14),c(9,16),c(9,20),c(9,19),
          c(10,11),c(10,21),c(10,20)
    )
)
polygons$PointOrder <- 1:nrow(polygons)
colnames(polygons) = c("IDLabel","IDPoint","PointOrder")

# Merge the three sets together to create a master set.
df <- merge(polygons,points)
df <- merge(df,polygon.labels)
df <- df[order(df$PointOrder),]

# Determine the Labels Data
Labs = ddply(df,"Label",function(x){c(c(mean(x$T),mean(x$L),mean(x$R)))})
colnames(Labs) = c("Label","T","L","R")
#---------------------------------------


#===========================================================
# Data plots
#===========================================================

palette <- c("#FCAB10", "#44AF69")

# Build the final plot
p <- ggtern(data = df, mapping = aes(x = L, y = T, z = R)) +
    # geom_polygon(aes(fill = Label, group = Label), color = "black", alpha = 0.25) +
    geom_polygon(aes(group = Label), color = "black", fill = "white", alpha = 0.25) +
    geom_text(data = Labs, aes(label = Label), size = 4, color = "black") +
    geom_point(data = grainsize, aes(x = Sand, y = Clay, z = Silt, fill = Position), shape = 21, size = 4, color = "black", alpha = 0.75, show.legend = TRUE) +
    scale_fill_discrete(type = rev(palette)) +
    guides(fill = guide_legend(override.aes = list(size = 6))) +
    theme_bw(base_size = 16) +
    theme_showarrows() +
    custom_percent("Percent") +
    labs(# title="Shepard Sediment Classification Diagram",
        fill = "Position",
        T = "Clay",
        L = "Sand",
        R = "Silt") +
    #guides(fill = "none", group = "none") +
    theme(legend.position = "right",
          plot.margin = grid::unit(c(-5,0,0,-00), "mm"),
          legend.margin = margin(t = -100, r = 0, b = 0, l = -45, unit = "mm"))

p

ggsave(plot = p, file.path(OUTPUT, "shepard-diagram.png"), width = 21, height = 21, units = "cm")


#===========================================================
# Cleanup
#===========================================================

# unload packages that may conflict with other scripts
detach("package:ggtern", unload = TRUE)
detach("package:plyr", unload = TRUE)

