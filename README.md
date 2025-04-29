# Heavy metals in the floodplains of the Sinu River, Colombia

This dataset contains raw measurements of heavy metal concentrations, physico-chemical parameters, and soil grain size from soil samples collected around the Sinú River in Colombia. The accompanying R scripts reproduce all figures and tables presented in the associated publication.

A description of each data file and of the measurement units used can be found in the `metadata.yml` file.

To use the R scripts, open the `scripts.Rproj` project file in [RStudio Desktop](https://posit.co/download/rstudio-desktop/) by Posit. Then, load and run any of the provided scripts.

- `study_area.R`: Generates a map of the study area and its location within the Caribbean region of Colombia.
- `grain_size.R`: Produces the Shepard diagram showing the classification of the soils.
- `data_analysis.R`: Creates all the remaining plots used in the analysis of the measured data, as presented in the associated publication.

All generated figures are saved in the `figures` directory, which must exist before running the scripts.
