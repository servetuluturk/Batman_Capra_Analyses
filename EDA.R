# Habitat Suitability Modeling for Capra aegagrus
# Study Area: Batman Province, T??rkiye
# Method: KDE-based weighted habitat suitability analysis
# Software: R (terra package)
############################################################

library(terra)
setwd("C:/KDE_Analiz")

kde         <- rast("std_kde.tif")
dist_water  <- rast("dist_water.tif")
dist_sett   <- rast("dist_settlements.tif")
dist_forest <- rast("dist_forest.tif")
dist_agri   <- rast("dist_agriculture.tif")

kde_min <- min(values(kde), na.rm = TRUE)
kde_max <- max(values(kde), na.rm = TRUE)
kde_std <- (kde - kde_min) / (kde_max - kde_min)

w_min <- min(values(dist_water), na.rm = TRUE)
w_max <- max(values(dist_water), na.rm = TRUE)
water_std <- (w_max - dist_water) / (w_max - w_min)

s_min <- min(values(dist_sett), na.rm = TRUE)
s_max <- max(values(dist_sett), na.rm = TRUE)
sett_std <- (dist_sett - s_min) / (s_max - s_min)

f_min <- min(values(dist_forest), na.rm = TRUE)
f_max <- max(values(dist_forest), na.rm = TRUE)
forest_std <- (f_max - dist_forest) / (f_max - f_min)

a_min <- min(values(dist_agri), na.rm = TRUE)
a_max <- max(values(dist_agri), na.rm = TRUE)
agri_std <- (dist_agri - a_min) / (a_max - a_min)

habitat_suitability <-
  (kde_std    * 0.40) +
  (water_std  * 0.20) +
  (forest_std * 0.25) +
  (agri_std   * 0.075) +
  (sett_std   * 0.075)

writeRaster(habitat_suitability,"Capra_habitat_suitability.tif",
  overwrite = TRUE)

rcl <- matrix(c(0.00, 0.20, 1,0.20, 0.40, 2,0.40, 0.60, 3,0.60, 0.80, 4,
    0.80, 1.00, 5),ncol = 3,byrow = TRUE)

habitat_class <- classify(habitat_suitability, rcl)

writeRaster(habitat_class,"Capra_habitat_classes.tif",overwrite = TRUE)

pixel_area_km2 <- prod(res(habitat_class)) / 1e6

freq_table <- freq(habitat_class)
freq_table <- freq_table[!is.na(freq_table$value), ]

freq_table$area_km2 <- freq_table$count * pixel_area_km2
freq_table$class <- c("Very Low", "Low", "Medium", "High", "Very High")

total_area <- sum(freq_table$area_km2)
freq_table$percent <- (freq_table$area_km2 / total_area) * 100

high_area <- sum(freq_table$area_km2[freq_table$class %in% c("High", "Very High")])
high_percent <- (high_area / total_area) * 100

print(freq_table)
cat("\nTotal Area (km2):", round(total_area, 2), "\n")
cat("High + Very High Area (km2):", round(high_area, 2), "\n")
cat("High + Very High Area (%):", round(high_percent, 2), "\n")

plot(habitat_class,main = "Habitat Suitability Classes for Capra aegagrus",
  col = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850"))

