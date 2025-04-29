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
library(ggthemes)               # additional ggplot2 themes
library(RColorBrewer)
library(paletteer)              # huge collection of color palettes

library(yaml)                   # read yaml files
library(xtable)                 # export tables in LaTeX format
library(latex2exp)              # use LeTeX expressions in plots

library(corrplot)               # visualize correlations
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
          panel.spacing = unit(5, "mm"),
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
# Read the csv data file containing the concentration limits 
# (after Wyse, doi: 10.1039/B308584H)
#---------------------------------------
filename <- file.path(DATA, "concentration-limits.csv")
limits <- read.csv(filename, header = TRUE, strip.white = TRUE) %>% as_tibble()
limits

# convert the tibble to a wide 1-line format (to be used later)
conc_limits <- limits %>% pivot_wider(names_from = "Element", values_from = "Reference_Concentration")

# sort columns in alphabetical order
conc_limits <- conc_limits %>%
    select(order(colnames(.)))

conc_limits
#---------------------------------------

#---------------------------------------
# Read the data file containing the whole dataset
#---------------------------------------
filename <- file.path(DATA, "all-measurements.csv")
meas <- read.csv(filename, header = TRUE, strip.white = TRUE) %>% as_tibble()

meas
#---------------------------------------


#===========================================================
# Normalize metal concentrations to safety limits
#===========================================================

metals <- names(conc_limits)

# convert concentration data to long format
conc_long <- meas %>% 
    select(c("Location", "Position", "Season"), all_of(metals)) %>% 
    pivot_longer(!c(Location, Position, Season), names_to = "Element", values_to = "Concentration")

conc_long$Element <- factor(conc_long$Element, levels = c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb"))

conc_long
#---------------------------------------

# calculate the ratio between measured concentrations and safety limits
meas
conc_limits

mat <- meas %>% select(all_of(metals)) %>% as.matrix()
vec <- conc_limits %>% as_vector()
head(mat)
vec

# divide each row of the concentration matrix by the vector of safety limits
# mat_norm <- mat %o% 1/vec
mat_norm <- t(t(mat) / vec ) %>% as_tibble()
mat_norm

# create a new tibble containing metal concentrations normalized to safety limits
conc_norm <- meas %>%
    select(c("Location", "Position", "Season")) %>%
    cbind(mat_norm)
conc_norm

# convert the normalized concentration tibble to long format
conc_norm_long <- conc_norm %>% pivot_longer(!c(Location, Position, Season), names_to = "Element", values_to = "Norm_Concentration")

conc_norm_long$Element <- factor(conc_norm_long$Element, levels = c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb"))
conc_norm_long


#===========================================================
# Tables
#===========================================================

#---------------------------------------
# generate Table 1 of paper
#---------------------------------------
meas %>% filter(Location == "SL01" | Location == "SL12" | Location == "SL25") %>%
    arrange(rev(Position)) %>%
    select("Location", "Position", "Season", metals) %>%
    relocate(c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb"), .after = "Season") %>% 
    xtable::xtable(caption = "Caption GOES HERE", digits = c(NA, NA, NA, NA, 2, 2, 1, 1, 1, 2), label = "tab:samplerecord")

conc_limits %>%
    relocate(c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb")) %>%
    xtable::xtable()#caption = "Caption GOES HERE", digits = c(NA, 2, 2, 1, 0, 0, 1), label = "tab:samplerecord")
#---------------------------------------


#---------------------------------------
# generate Table S1 of paper
#---------------------------------------
meas %>% 
    arrange(Location, rev(Position)) %>%
    select("Location", "Position", "Season", metals) %>%
    relocate(c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb"), .after = "Season") %>% 
    xtable::xtable(caption = "Caption GOES HERE", digits = c(NA, NA, NA, NA, 2, 2, 1, 1, 1, 2), label = "tab:samplerecord") %>%
    print.xtable(type = "latex", include.rownames = FALSE,
                 floating = TRUE,
                 latex.environments = "center",
                 tabular.environment = "tabularx",
                 booktabs = TRUE,
                 caption.placement = "top")



conc_limits %>%
    relocate(c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb")) %>%
    xtable::xtable()#caption = "Caption GOES HERE", digits = c(NA, 2, 2, 1, 0, 0, 1), label = "tab:samplerecord")
#---------------------------------------


#---------------------------------------
# generate a table with summary data
#---------------------------------------
meas %>% summary()

fun_names <- c("min", "1st quartile", "mean", "median", "3rd quartile", "max")
summary <- meas %>% 
    reframe(across(Cd:Zn, 
                   ~ c(min(.x), quantile(.x, 0.25), mean(.x), median(.x), quantile(.x, 0.75), max(.x))),
            .by = c(Position, Season))

summary <- cbind(summary, fun_names) %>% 
    relocate(fun_names, .after = Season) %>%
    relocate(c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb"), .after = fun_names)

summary <- summary %>% dplyr::rename(Stat = fun_names)
summary

summary %>% 
    xtable::xtable(caption = "Caption HERE", digits = c(NA, NA, NA, NA, 3, 3, 3, 3, 3, 3))
#---------------------------------------


#===========================================================
# Data plots
#===========================================================

#---------------------------------------
# boxplot of the normalized concentration, 
# dividing it into rainy/dry and surface/deep panels
#---------------------------------------
data <- conc_norm_long


levels(conc_long$Element)

# Hg (Mercury):   #C0C0C0 (Silver)
# Cd (Cadmium):   #FFD700 (Yellow-Gold)
# Ni (Nickel):    #009688 (Teal)
# Cr (Chromium):  #A7C7E7 (Light Blue)
# Zn (Zinc):      #FF6347 (Tomato Red)
# Pb (Lead):      #8B4513 (Saddle Brown)

metal_palette = c("#C0C0C0", "#FFD700", "#009688", "#A7C7E7", "#FF6347", "#8B4513")

ggplot(data, aes(x = Element, y = Norm_Concentration, fill = Element)) +
    geom_hline(yintercept = 1.0, linetype = "solid", color = "black") +
    geom_violin(aes(fill = Element), stat = "ydensity", scale = "width",
                trim = FALSE, draw_quantiles = c(0.05, 0.5, 0.95), 
                adjust = 1.0, linewidth = 0.5, alpha = 0.75,
                show.legend = TRUE) +
    geom_jitter(fill = "white", color = "black", shape = 21, 
                position = position_jitter(width = 0.1, height = 0.1)) +
    facet_grid(rows = vars(factor(Position, levels = c("surface", "deep"))),
               cols = vars(Season)) +
    labs( x = "Element", y = "Normalized concentration") +
    scale_fill_manual(values = metal_palette) +
    theme_hm()

ggsave(file.path(OUTPUT, "concentration-distribution.png"), width = 21, height = 14, units = "cm")
#---------------------------------------


#---------------------------------------
# heatplot by sampling location, divided into rainy/dry and surface/deep panels
#---------------------------------------
data <- conc_norm_long

ncols <- data$Element %>% unique() %>% length()
conc_palette <- colorRampPalette(brewer.pal(ncols, "RdYlGn"), bias = 0.4)(ncols)

# for clarity, only every second location name is shown on the y-axis
x_breaks <- data$Location %>% unique() %>% .[seq(1, 25, by = 2)]

# define the limits and labels of the legend
breaks  <- seq(0.5, 4.5, 0.5)
labels <- c(  "0.0 - 0.5", "0.5 - 1.0", "1.0 - 1.5", "1.5 - 2.0",
              "2.0 - 2.5", "2.5 - 3.0", "3.0 - 3.5", "3.5 - 4.0",
              "4.0 - 4.5")

ggplot(data, aes(x = Location, y = Element)) +
    geom_tile(aes(fill = Norm_Concentration), na.rm = FALSE, 
              color = "white", lwd = 0.75, linetype = 1, width = 1) +
    facet_grid(rows = vars(factor(Position, levels = c("surface", "deep"))),
               cols = vars(Season)) +
    scale_x_discrete(expand = c(0.05, 0.05), breaks = x_breaks, guide = guide_axis(n.dodge = 2)) +
    labs( x = "Site") +
    scale_fill_gradientn(colours = conc_palette,
                         breaks = breaks,
                         labels = labels, 
                         trans = "reverse") +
    guides(fill = guide_legend(title = "Normalized\nconcentration",
                               even.steps = FALSE,
                               frame.colour = "black",
                               show.limits = TRUE, reverse = TRUE)) +
    theme_hm()

ggsave(file.path(OUTPUT, "concentration-heatmap.png"), width = 21, height = 14, units = "cm")
#---------------------------------------


#---------------------------------------
# scatter/area plots by sampling location, divided into rainy/dry and surface/deep panels
#---------------------------------------
data <- conc_long

levels(conc_long$Element)

# Hg (Mercury):   #C0C0C0 (Silver)
# Cd (Cadmium):   #FFD700 (Yellow-Gold)
# Ni (Nickel):    #009688 (Teal)
# Cr (Chromium):  #A7C7E7 (Light Blue)
# Zn (Zinc):      #FF6347 (Tomato Red)
# Pb (Lead):      #8B4513 (Saddle Brown)

metal_palette = c("#C0C0C0", "#FFD700", "#009688", "#A7C7E7", "#FF6347", "#8B4513")


# for clarity, only every second location name is shown on the x-axis
x_breaks <- data$Location %>% unique() %>% .[seq(1, 25, by = 2)]

# define the limits and labels of the legend
breaks  <- seq(0.5, 4.5, 0.5)
labels <- c(  "0.0 - 0.5", "0.5 - 1.0", "1.0 - 1.5", "1.5 - 2.0",
              "2.0 - 2.5", "2.5 - 3.0", "3.0 - 3.5", "3.5 - 4.0",
              "4.0 - 4.5")

ggplot(data, aes(x = Location, y = Concentration, group = Element, color = Element, fill = Element)) +
    geom_smooth(stat = "smooth", method = "loess", se = TRUE, level = 0.95,
                linewidth = 0.01, alpha = 0.25, show.legend = TRUE) +
    geom_point(size = 2.0) +
    facet_grid(rows = vars(factor(Position, levels = c("surface", "deep"))),
               cols = vars(Season)) +
    scale_y_log10(minor_breaks = waiver(), n.breaks = 12) +
    xlab("Site") +
    ylab("Concentration [mg/kg]") +
    scale_x_discrete(expand = c(0.05, 0.05), breaks = x_breaks, guide = guide_axis(n.dodge = 2)) +
    scale_color_discrete(type = metal_palette) +
    scale_fill_discrete(type = metal_palette) +
    theme_hm() +
    theme(strip.text = element_text(size = 12),
          panel.grid.major.y = element_blank(), 
          panel.grid.minor = element_blank(),
    )


ggsave(file.path(OUTPUT, "concentration-by-location.png"), width = 21, height = 21, units = "cm")
#---------------------------------------


#---------------------------------------
# correlation plot
#---------------------------------------

# all measurements
pdf(file.path(OUTPUT, "correlation-all.pdf"), width = 24, height = 24)

cr <- meas %>% 
    select(Cd:Zn) %>% 
    cor(method = "pearson")

conf <- meas %>% 
    select(Cd:Zn) %>% 
    cor.mtest(conf.level = 0.95)

conf

corrplot(cr, method = "ellipse", type = "upper", order = "alphabet", 
         p.mat = conf$p, 
         insig = "n",
         col = brewer.pal(n = 10, name = "RdYlBu"), 
         outline = TRUE,
         diag = FALSE, 
         addCoef.col = "grey50", 
         tl.cex = 4, tl.col = "black", tl.srt = 0, tl.offset = 1, 
         cl.cex = 4, cl.pos = "r",
         number.cex = 4,
    )

dev.off()



# surface measurements
pdf(file.path(OUTPUT, "correlation-surface.pdf"), width = 24, height = 24)

cr <- meas %>% 
    filter(Position == "surface") %>%
    select(Cd:Zn) %>% 
    cor(method = "pearson")

conf <- meas %>% 
    select(Cd:Zn) %>% 
    cor.mtest(conf.level = 0.95)

conf

corrplot(cr, method = "ellipse", type = "upper", order = "alphabet", 
         p.mat = conf$p, 
         insig = "n",
         col = brewer.pal(n = 10, name = "RdYlBu"), 
         outline = TRUE,
         diag = FALSE, 
         addCoef.col = "grey50", 
         tl.cex = 4, tl.col = "black", tl.srt = 0, tl.offset = 1, 
         cl.cex = 4, cl.pos = "r",
         number.cex = 4,
)

dev.off()

# deep measurements
pdf(file.path(OUTPUT, "correlation-deep.pdf"), width = 24, height = 24)

cr <- meas %>% 
    filter(Position == "deep") %>%
    select(Cd:Zn) %>% 
    cor(method = "pearson")

conf <- meas %>% 
    select(Cd:Zn) %>% 
    cor.mtest(conf.level = 0.95)

conf

corrplot(cr, method = "ellipse", type = "upper", order = "alphabet", 
         p.mat = conf$p, 
         insig = "n",
         col = brewer.pal(n = 10, name = "RdYlBu"), 
         outline = TRUE,
         diag = FALSE, 
         addCoef.col = "grey50", 
         tl.cex = 4, tl.col = "black", tl.srt = 0, tl.offset = 1, 
         cl.cex = 4, cl.pos = "r",
         number.cex = 4,
)


dev.off()
#---------------------------------------


#===========================================================
# Scatterplot matrix
#===========================================================

library(GGally)

# define a general function that generates a scatterplot matrix
plot_matrix <- function(data, selection, kind, palette) {
    p <- ggpairs(data = data %>% select(all_of(c(selection, kind))),
        lower=list(combo = wrap("facethist",  bins = 20)),
        upper = list(continuous = "cor"),
        mapping = aes(color = get(kind), alpha = 0.7),
        axisLabels = "show",
        ) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) + 
    theme_hm() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "mm"),
    )
    return(p)
}


data <- meas

# concentration data depends on position AND season
kind <- c("Position", "Season")
kind %>% purrr::map(\(x) {
    if (x == "Position") {palette <- c("#FCAB10", "#44AF69")}
    else if (x == "Season") {palette <- c("#CC3F0C", "#1C448E")}
    p <- plot_matrix(data = data, selection = c("Hg", "Cd", "Ni", "Cr", "Zn", "Pb"), kind = x, palette = palette)
    ggsave(plot = p, file.path(OUTPUT, paste0("scatter-metals-by-", str_to_lower(x), ".png")), width = 26, height = 24, units = "cm")
    }
)


# physico-chemical data does not depend on position: show only changes by season
# palette <- scales::hue_pal(direction = 1)(5)[5:4]
palette <- c("#CC3F0C", "#1C448E")
p <- plot_matrix(data = data, selection = c("pH", "OM", "EC", "CEC"), kind = "Season", palette = palette)

ggsave(plot = p, file.path(OUTPUT, "scatter-physchem-by-season.png"), width = 26, height = 24, units = "cm")


# sediment data does not depend on seasons: show only changes by position
# palette <- scales::hue_pal(direction = 1)(5)[1:2]
palette <- c("#FCAB10", "#44AF69")
p <- plot_matrix(data = data, selection = c("Clay", "Silt", "Sand"), kind = "Position", palette = palette)

ggsave(plot = p, file.path(OUTPUT, "scatter-sediment-by-position.png"), width = 26, height = 24, units = "cm")


#===========================================================
# Cleanup
#===========================================================

# unload packages that may conflict with other scripts
detach("package:GGally", unload = TRUE)

