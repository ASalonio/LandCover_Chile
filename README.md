# LandCover-Chile

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

**Historical Land-Cover Change Analysis (1999–2018) in Chile**  
Using ESA CCI Land Cover maps (200 m resolution).

---

**Project Overview**
This repository contains an R-based analysis of land cover composition and changes in the Centro-Sur region of Chile (from north to south: Ñuble, Bio-Bio, and La Araucanía) using ESA CCI land-cover maps at 200m resolution. The analysis covers the period from 1999 to 2018, focusing on original 16-class compositions, reclassification into 8 functional classes, and detailed transition dynamics.

The project is split into two Jupyter notebooks (in R kernel format):

- part_I.ipynb: Focuses on original land cover percentages, top-3 class visualizations, and reclassification of rasters into 8 functional classes (e.g., Water_Bare, Native, Plant, Shrub, Crop, Grass, Peat, Urban).

- part_II.ipynb: Performs full transition analysis between the reclassified classes, including Sankey diagrams (all transitions and top-15 dynamic), area-by-class bar plots, spatial maps of major transitions, and summary tables.

**Key insights from the analysis:**

Temperate Forest lost ~5,400 km² (7.6%), with significant declines between 2009-2018.
Shrubland expanded by ~5,300 km², often from harvested plantations or degraded native forests.
Major transitions include Plantation → Shrubland (2,696 km²), Native Forest → Shrubland (2,473 km²), and Native Forest → Plantation (1,335 km²), likely driven by harvesting, fires, and drought.
Changes accelerated post-2009, coinciding with wildfires and mega-droughts, increasing future fire risks due to higher shrubland fuel loads.

The analysis highlights environmental concerns like native forest loss through direct conversion (lowlands) and degradation (Andean slopes), with spatial patterns suggesting targeted management needs.

---

## Usage

Clone the repo:
```bash
git clone https://github.com/ASalonio/LandCover-Chile.git
```

---

## Repository Structure

- part_I.ipynb: Notebook for land cover composition and reclassification.
- part_II_ipynb: Notebook for change analysis and visualizations.
- sankeys/: subdirectory for interactive HTML files (generated in Part II).

---

## Key Results

**From Part I:**
   - original_landcover_percentages.xlsx: Percentages of 16 original classes      by year.
   - Bar plot: Top-3 classes per year (displayed in notebook).
   - Reclassified rasters: reclass_cover_YYYY.tif (GeoTIFFs for 1999, 2009,       2018).

**From Part II:**
   - full_transition_matrix_km2.csv: Full transition matrix in km² (including stable and dynamic areas).
   - top3_transition_summary.xlsx: Summary of top-3 transitions (km² and percentages).
   - Sankey diagrams: Interactive HTML files for all transitions and top-15 dynamic.
   - Bar plot: Area per class over time (1999/2009/2018).
   - Map: top3_transitions_map.png (static PNG of major transitions; interactive version in notebook via tmap). 

---

## Data Sources

ESA CCI Land Cover (v2.1.1, 200 m): https://www.esa-landcover-cci.org/
Study area: Southern Chile (custom polygon)

## Acknowledgments

Universidad Católica de la Santísima Concepción, UCSC
Consejo Superior de Investigaciones Científicas, CSIC

## Author

Augusto Salonio
Date: 2025-11-11

For questions or contributions, open an issue or pull request.
