# Supplementary code for science reports manuscript 
## Forest cover loss footprint of the global mining sector

This repository contains the code to quantify the global forest cover loss due to mining

### Scripts

- `00-prepare-mining-areas-dataset.R` - Rscript to merges polygons of mining and quarrying from OpenStreetMap with mining polygons from Maus et al. (2022).
`01-gee-calculate-tree-cover-loss.js` - Javascript for GEE to calculates the area of forest cover loss for each polygon using the Global Forest Change data Hansen et al. (2013). The script also adds further attributes to the polygons by intersecting each features with Ecoregions 2017 © Resolve and The World Database on Protected Areas from .
- `02-02-tidy-forest-cover-loss-time-series.R` - Rscript to tidy the data processed in GEE.
- `03-cluster-mining-commodities.R` - Rscript to cluster mining polygons and mining coordinates from the S&P database to obtain the commodities for each polygon.
- `04-create-manuscript-figures-and-tables.R` - Rscript to produce the figures and tables for the manuscript.
