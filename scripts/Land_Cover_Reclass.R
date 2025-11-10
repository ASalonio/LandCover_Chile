## --------------------------------------------------------------
## Land_Cover_Reclass.R
## Purpose: Reclassify the original 16-class ESA CCI land-cover
##          maps (200 m) into 8 functional classes and export the
##          reclassified rasters.
## --------------------------------------------------------------

# ── Packages ---------------------------------------------------------
library(terra)      # raster handling
library(dplyr)      # data-frame manipulation
library(openxlsx)   # Excel export
library(knitr)      # nice console table (optional)

# ── Paths ------------------------------------------------------------
raw_dir   <- "~/AMID/data/Land_cover"
out_dir   <- "~/AMID/output/Paisaje"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ── Load raw rasters -------------------------------------------------
cover_1999 <- rast(file.path(raw_dir, "Land_cover_1999_200m.tif"))
cover_2009 <- rast(file.path(raw_dir, "Land_cover_2009_200m.tif"))
cover_2018 <- rast(file.path(raw_dir, "Land_cover_2018_200m.tif"))

# Convert to factor (required for classify())
cover_1999_f <- as.factor(cover_1999)
cover_2009_f <- as.factor(cover_2009)
cover_2018_f <- as.factor(cover_2018)

# ── ORIGINAL 16-CLASS COMPOSITION ------------------------------------
# (QC + documentation – saved as Excel)

ORIG_CLASS_NAMES <- c(
  "Water", "Sand", "Medit. Sclerophyll", "Temperate Forest",
  "Eucalyptus", "Fruit Trees", "Glacier", "Riparian",
  "Shrub", "Pinus radiata", "Crops", "Grassland",
  "Bare soil", "Peatland", "Human structure", "Harvested plantation"
)

compute_coverage <- function(rast, year) {
  freq(rast) %>%
    filter(!is.na(value)) %>%
    mutate(
      percentage = round(count / sum(count) * 100, 1),
      year       = year
    ) %>%
    arrange(desc(percentage))
}

freq_1999 <- compute_coverage(cover_1999, "1999")
freq_2009 <- compute_coverage(cover_2009, "2009")
freq_2018 <- compute_coverage(cover_2018, "2018")

coverage_summary <- bind_rows(freq_1999, freq_2009, freq_2018) %>%
  left_join(
    data.frame(value = 1:16, original_class = ORIG_CLASS_NAMES),
    by = "value"
  ) %>%
  select(year, original_class, value, count, percentage) %>%
  arrange(year, desc(percentage))

# Console preview (nice table)
cat("\n=== Original land-cover composition (top 3 per year) ===\n")
coverage_summary %>%
  group_by(year) %>%
  slice_max(percentage, n = 3) %>%
  ungroup() %>%
  print()

# Export full table
xlsx_file <- file.path(out_dir, "original_landcover_percentages.xlsx")
write.xlsx(coverage_summary, xlsx_file, rowNames = FALSE)
message("\nFull 16-class composition saved to:\n  ", xlsx_file)

# ── RECLASSIFICATION MATRIX -----------------------------------------
reclass_matrix <- matrix(c(
  # from → to
  1,1, 2,1, 7,1, 13,1,   # 1 Water_Bare
  3,2, 4,2, 8,2,         # 2 Native
  5,3, 6,3,10,3,16,3,    # 3 Plant
  9,4,                   # 4 Shrub
  11,5,                   # 5 Crop
  12,6,                   # 6 Grass
  14,7,                   # 7 Peat
  15,8                    # 8 Urban
), ncol = 2, byrow = TRUE)

# ── RECLASSIFY -------------------------------------------------------
reclass_cover_1999 <- classify(cover_1999_f, reclass_matrix)
reclass_cover_2009 <- classify(cover_2009_f, reclass_matrix)
reclass_cover_2018 <- classify(cover_2018_f, reclass_matrix)

# ── SET CATEGORICAL LEVELS -------------------------------------------
new_levels <- data.frame(
  ID       = 1:8,
  Category = c("Water_Bare", "Native", "Plant", "Shrub",
               "Crop", "Grass", "Peat", "Urban")
)

levels(reclass_cover_1999) <- new_levels
levels(reclass_cover_2009) <- new_levels
levels(reclass_cover_2018) <- new_levels

# ── EXPORT RECLASSIFIED RASTERS --------------------------------------
writeRaster(
  reclass_cover_1999,
  file.path(out_dir, "reclass_cover_1999.tif"),
  overwrite = TRUE
)
writeRaster(
  reclass_cover_2009,
  file.path(out_dir, "reclass_cover_2009.tif"),
  overwrite = TRUE
)
writeRaster(
  reclass_cover_2018,
  file.path(out_dir, "reclass_cover_2018.tif"),
  overwrite = TRUE
)

message("\nReclassified 8-class rasters written to:\n  ", out_dir)