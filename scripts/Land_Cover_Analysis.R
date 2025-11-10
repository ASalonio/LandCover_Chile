## --------------------------------------------------------------
## Land_Cover_Analysis.R
## Historical Land-Use Change Analysis (1999-2009-2018)
## --------------------------------------------------------------

# ── Packages ---------------------------------------------------------
library(terra)        # raster handling
library(tidyterra)    # tidy raster ops
library(dplyr)        # data-frame manipulation
library(tidyr)        # pivot_longer / pivot_wider
library(sf)           # vector handling (borders)
library(openxlsx)     # Excel export
library(networkD3)    # Sankey diagrams
library(htmlwidgets)  # saveWidget
library(webshot)      # HTML → PNG
library(ggplot2)      # bar-plots
library(tmap)         # static maps
library(RColorBrewer) # colour palettes
library(knitr)        # nice console tables (optional)

# ── Paths ------------------------------------------------------------
in_dir   <- "~/AMID/output/Paisaje"
out_dir  <- "~/AMID/output/Plots/LULC"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ── Helper functions -------------------------------------------------
pixel_area_m2 <- function(rast) prod(res(rast))               # m² per pixel
pixel_to_ha   <- function(count, rast) count * pixel_area_m2(rast) / 10000

transition_label <- function(code, class_names) {
  v1 <- floor(code / 100)
  v2 <- floor((code %% 100) / 10)
  v3 <- code %% 10
  if (all(c(v1, v2, v3) %in% seq_along(class_names))) {
    paste(class_names[v1], "->", class_names[v2], "->", class_names[v3])
  } else NA_character_
}

# ── Load reclassified rasters ----------------------------------------
reclass_cover_1999 <- rast(file.path(in_dir, "reclass_cover_1999.tif"))
reclass_cover_2009 <- rast(file.path(in_dir, "reclass_cover_2009.tif"))
reclass_cover_2018 <- rast(file.path(in_dir, "reclass_cover_2018.tif"))

# Administrative limits (for maps)
lim_32719 <- vect(file.path("~/AMID/data/Límites", "lim_32719.shp"))

# ── 1. Area per class (1999) -----------------------------------------
area_1999 <- freq(reclass_cover_1999) %>%
  mutate(area_ha = pixel_to_ha(count, reclass_cover_1999))

write.xlsx(area_1999,
           file.path(out_dir, "reclass_cover_1999_areas.xlsx"),
           rowNames = FALSE)
message("1999 class areas saved.")

# ── 2. Align rasters --------------------------------------------------
reclass_cover_2009_aligned <- resample(reclass_cover_2009,
                                       reclass_cover_1999,
                                       method = "near")
reclass_cover_2018_aligned <- resample(reclass_cover_2018,
                                       reclass_cover_1999,
                                       method = "near")

# Stack for crosstab
lc_stack <- c(reclass_cover_1999,
              reclass_cover_2009_aligned,
              reclass_cover_2018_aligned)

# ── 3. Full 3-way crosstab --------------------------------------------
ct <- crosstab(lc_stack, useNA = FALSE)      # Category, Category.1, Category.2, Freq
ct_df <- as.data.frame(ct) %>%
  rename(y1999 = Category, y2009 = Category.1, y2018 = Category.2) %>%
  filter(Freq > 0) %>%                       # drop zero-frequency rows
  mutate(
    area_m2 = Freq * pixel_area_m2(reclass_cover_1999),
    area_ha = area_m2 / 10000,
    Tipo    = if_else(y1999 == y2009 & y2009 == y2018,
                      "Estable", "Dinámica")
  ) %>%
  arrange(desc(area_ha))

write.xlsx(ct_df, file.path(out_dir, "ct_df.xlsx"), rowNames = FALSE)
write_csv2(ct_df, file.path(out_dir, "ct_df.csv"))
message("Full transition table (ct_df) saved.")

# ── 4. Stable vs. changing transitions -------------------------------
ct_stable <- filter(ct_df, Tipo == "Estable")
ct_change <- filter(ct_df, Tipo == "Dinámica")
ct_major  <- slice_head(ct_change, n = 15)   # top-15 dynamic transitions

# ── 5. Sankey diagram (all transitions) -------------------------------
sankey_data <- function(df, value_col = "Freq") {
  classes <- unique(c(df$y1999, df$y2009, df$y2018))
  nodes   <- data.frame(name = paste0(rep(c("1999_", "2009_", "2018_"),
                                          each = length(classes)), classes))
  links   <- rbind(
    data.frame(source = match(df$y1999, classes) - 1,
               target = match(df$y2009, classes) + length(classes) - 1,
               value  = df[[value_col]]),
    data.frame(source = match(df$y2009, classes) + length(classes) - 1,
               target = match(df$y2018, classes) + 2*length(classes) - 1,
               value  = df[[value_col]])
  ) %>% group_by(source, target) %>% summarise(value = sum(value), .groups = "drop")
  list(nodes = nodes, links = links)
}

sankey_all <- sankey_data(ct_df)
sankey_all_plot <- sankeyNetwork(Links = sankey_all$links,
                                 Nodes = sankey_all$nodes,
                                 Source = "source", Target = "target",
                                 Value = "value", NodeID = "name",
                                 fontSize = 12, nodeWidth = 30)

saveWidget(sankey_all_plot,
           file.path(out_dir, "sankey_all_transitions.html"),
           selfcontained = TRUE)
webshot(file.path(out_dir, "sankey_all_transitions.html"),
        file.path(out_dir, "sankey_all_transitions.png"),
        delay = 2)
message("Full Sankey saved (HTML + PNG).")

# ── 6. Sankey for the 15 major dynamic transitions -------------------
sankey_major <- sankey_data(ct_major, value_col = "area_ha")
sankey_major_plot <- sankeyNetwork(Links = sankey_major$links,
                                   Nodes = sankey_major$nodes,
                                   Source = "source", Target = "target",
                                   Value = "value", NodeID = "name",
                                   fontSize = 12, nodeWidth = 30)

saveWidget(sankey_major_plot,
           file.path(out_dir, "sankey_major_transitions.html"),
           selfcontained = TRUE)
webshot(file.path(out_dir, "sankey_major_transitions.html"),
        file.path(out_dir, "sankey_major_transitions.png"),
        delay = 2)
message("Major-transition Sankey saved.")

# ── 7. Area per class over time (bar-plots) -------------------------
area_per_class <- ct_df %>%
  pivot_longer(cols = c(y1999, y2009, y2018),
               names_to = "Year", values_to = "Class") %>%
  mutate(Year = recode(Year, y1999 = "1999", y2009 = "2009", y2018 = "2018"),
         Year = factor(Year, levels = c("1999","2009","2018"))) %>%
  group_by(Year, Class) %>%
  summarise(area_ha = sum(area_ha), .groups = "drop")

# Faceted
p_facet <- ggplot(area_per_class,
                  aes(x = Class, y = area_ha, fill = Class)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ Year, ncol = 3) +
  labs(title = "Land-Cover Area by Class (1999-2018)",
       y = "Area (ha)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(file.path(out_dir, "area_faceted.png"), p_facet,
       width = 12, height = 4, dpi = 300)

# Dodged
p_dodge <- ggplot(area_per_class,
                  aes(x = Class, y = area_ha, fill = Year)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Land-Cover Area by Class (1999-2018)",
       x = "Class", y = "Area (ha)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "area_dodged.png"), p_dodge,
       width = 10, height = 6, dpi = 300)

message("Bar-plots saved.")

# ── 8. Transition raster (all transitions) ---------------------------
class_names <- c("Water_Bare","Native","Plant","Shrub",
                 "Crop","Grass","Peat","Urban")
transition_raster <- (reclass_cover_1999 * 100) +
  (reclass_cover_2009 * 10) +
  reclass_cover_2018

trans_vals  <- unique(values(transition_raster), na.rm = TRUE)
trans_labels <- sapply(trans_vals,
                       transition_label,
                       class_names = class_names)
trans_lookup <- data.frame(value = trans_vals,
                           label = trans_labels) %>%
  filter(!is.na(label))

# Frequency + area
trans_freq <- freq(transition_raster) %>%
  mutate(area_ha = pixel_to_ha(count, reclass_cover_1999)) %>%
  left_join(trans_lookup, by = c("value" = "value")) %>%
  filter(!is.na(label))

levels(transition_raster) <- trans_lookup
writeRaster(transition_raster,
            file.path(out_dir, "transitions.tif"),
            overwrite = TRUE)
message("Full transition raster saved.")

# ── 9. Top-15 transition map (static) --------------------------------
top15_vals <- slice_head(arrange(trans_freq, desc(area_ha)), n = 15)$value
top15_rast <- transition_raster
top15_rast[!top15_rast %in% top15_vals] <- NA
levels(top15_rast) <- filter(trans_lookup, value %in% top15_vals)

tmap_mode("plot")
pal15 <- brewer.pal(min(12, nrow(top15_vals)), "Set3")
if (length(top15_vals) > 12) pal15 <- c(pal15,
                                        brewer.pal(length(top15_vals)-12, "Set1"))

tm15 <- tm_shape(top15_rast) +
  tm_raster(style = "cat", palette = pal15,
            title = "Top-15 Transitions",
            labels = levels(top15_rast)[[1]]$label) +
  tm_shape(lim_32719) + tm_borders(col = "black", lwd = 1.5) +
  tm_layout(main.title = "Spatial Distribution of the 15 Largest Transitions",
            legend.outside = TRUE, legend.outside.position = "right")

tmap_save(tm15, file.path(out_dir, "top15_transitions_map.png"),
          width = 12, height = 8, units = "in")
writeRaster(top15_rast,
            file.path(out_dir, "top15_transitions.tif"),
            overwrite = TRUE)
message("Top-15 map + raster saved.")

# ── 10. Dynamic-only transition map (coloured by end-class) ----------
ct_major <- ct_major %>%
  mutate(v1999 = match(y1999, class_names),
         v2009 = match(y2009, class_names),
         v2018 = match(y2018, class_names),
         trans_val = v1999*100 + v2009*10 + v2018,
         trans_lab = paste(y1999, "->", y2009, "->", y2018))

dyn_rast <- transition_raster
dyn_rast[!dyn_rast %in% ct_major$trans_val] <- NA
levels(dyn_rast) <- data.frame(value = ct_major$trans_val,
                               label = ct_major$trans_lab)

# ---- colour palette by final class (Plant=red, Shrub=blue, …) ----
end_counts <- table(ct_major$y2018)
pal_plant <- if (end_counts["Plant"]>0) brewer.pal(9,"Reds")[4:(3+end_counts["Plant"])] else character()
pal_shrub <- if (end_counts["Shrub"]>0) brewer.pal(9,"Blues")[9:(10-end_counts["Shrub"])] else character()
pal_native<- if (end_counts["Native"]>0) "#006400" else character()
pal_crop  <- if (end_counts["Crop"]>0) c("#FFD700","#FFFFE0")[1:end_counts["Crop"]] else character()
pal_grass <- if (end_counts["Grass"]>0) "black" else character()

col_vec <- character(nrow(ct_major))
idx <- list(Plant=1, Shrub=1, Native=1, Crop=1, Grass=1)
for (i in seq_along(ct_major$y2018)) {
  ec <- ct_major$y2018[i]
  if (ec == "Plant" && idx$Plant <= length(pal_plant)) { col_vec[i] <- pal_plant[idx$Plant]; idx$Plant <- idx$Plant+1 }
  else if (ec == "Shrub" && idx$Shrub <= length(pal_shrub)) { col_vec[i] <- pal_shrub[idx$Shrub]; idx$Shrub <- idx$Shrub+1 }
  else if (ec == "Native" && idx$Native <= length(pal_native)) { col_vec[i] <- pal_native[idx$Native]; idx$Native <- idx$Native+1 }
  else if (ec == "Crop" && idx$Crop <= length(pal_crop)) { col_vec[i] <- pal_crop[idx$Crop]; idx$Crop <- idx$Crop+1 }
  else if (ec == "Grass" && idx$Grass <= length(pal_grass)) { col_vec[i] <- pal_grass[idx$Grass]; idx$Grass <- idx$Grass+1 }
}

tm_dyn <- tm_shape(dyn_rast) +
  tm_raster(style = "cat", palette = col_vec,
            title = "Dynamic Transitions (coloured by 2018 class)",
            labels = levels(dyn_rast)[[1]]$label) +
  tm_shape(lim_32719) + tm_borders(col = "black", lwd = 1.5) +
  tm_layout(main.title = "Major Dynamic Land-Cover Transitions",
            legend.outside = TRUE, legend.outside.position = "right")

tmap_save(tm_dyn,
          file.path(out_dir, "dynamic_transitions_map.png"),
          width = 12, height = 8, units = "in")
writeRaster(dyn_rast,
            file.path(out_dir, "dynamic_transitions.tif"),
            overwrite = TRUE)
message("Dynamic-transition map + raster saved.")

# ── End of script ----------------------------------------------------
message("\n=== Land_Cover_Analysis.R finished ===")
