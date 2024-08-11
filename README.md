# Code to calculate the deforestation footprint of the global mining sector

This repository contains the supplementary code to calculate global deforestation due to mining.

### Scripts

- `00-prepare-mining-areas-dataset.R` - Rscript to merge polygons of mining from OpenStreetMap with mining polygons from Maus et al. (2022).
- `01-gee-calculate-tree-cover-loss.js` - Javascript for GEE to calculate the area of forest cover loss for each polygon using the Global Forest Change data Hansen et al. (2013). The script adds further attributes to the polygons by intersecting each feature with Ecoregions 2017 © Resolve and The World Database on Protected Areas from the World Database on Protected Areas (WDPA).
- `02-cluster-mining-commodities.R` - Rscript to cluster mining polygons and mining coordinates from the S&P database to obtain the commodities for each polygon.
- `03-tidy-forest-cover-loss-time-series.R` - Rscript to tidy the data processed in GEE and add commodities.
- `04-create-manuscript-figures-and-tables.R` - Rscript to produce the figures and tables for the manuscript.
