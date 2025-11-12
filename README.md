# LandCover-Chile

**Historical Land-Cover Change Analysis (1999–2018) in Chile**  
Using ESA CCI Land Cover maps (200 m resolution).

---

## Project Structure
LandCover_Chile/
├── scripts/
│   ├── Land_Cover_Reclass.R        → Reclassifies 16 → 8 functional classes
│   └── Land_Cover_Analysis.Rmd     → Full analysis + interactive visualizations
├── data/                           → Raw and processed geospatial data
├── output/
│   └── Plots/LULC/                 → Maps, tables, Sankey diagrams
└── README.md


---

## Key Results
- **Top 15 land-cover transitions** (1999 → 2009 → 2018)  
- **Interactive Sankey diagrams** (all + top 15 dynamic)  
- **High-resolution spatial maps** with class-labeled transitions  
- **Area summaries** in hectares per class and year  
- **Fully reproducible** workflow

---

## View the Full Report
[Open interactive HTML report](./scripts/Land_Cover_Analysis.html)

> *Generated automatically from `Land_Cover_Analysis.Rmd`*

---

## Reproducibility
1. Clone the repo:
   ```bash
   git clone https://github.com/ASalonio/LandCover-Chile.git

---

## Data Sources

ESA CCI Land Cover (v2.1.1, 200 m): https://www.esa-landcover-cci.org/
Study area: Southern Chile (custom polygon)

## Contact

Augusto Salonio

augusto93921@gmail.com
