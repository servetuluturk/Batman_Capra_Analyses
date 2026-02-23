# Kernel Density Estimation (KDE) Analysis
# Study area: Batman Province, Türkiye
# Author: Servet Ulutürk

install.packages("terra")
install.packages("sf")
install.packages("spdep")
install.packages("spatialreg")
install.packages("geodata")
install.packages("car")

library(terra)
library(sf)
library(spdep)
library(spatialreg)
library(geodata)
library(ggplot2)
library(car)

data <- read.csv("D:/Capra_DEM/Capra_Batman.csv")
capra_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
capra_utm <- st_transform(capra_sf, crs = 32637)
coords <- st_coordinates(capra_utm)

North_kde <- rast("D:/Capra_DEM/North_kde.tif")
South_kde <- rast("D:/Capra_DEM/South_kde.tif")

data$kde_value <- NA
north_pts <- capra_utm[capra_utm$stratum == "North", ]
terra::extract(North_kde, vect(north_pts))
south_pts <- capra_utm[capra_utm$stratum == "South", ]
terra::extract(South_kde, vect(south_pts))

capra_utm$kde <- NA
north_pts <- capra_utm[capra_utm$stratum == "North", ]
north_vals <- terra::extract(North_kde, terra::vect(north_pts))
capra_utm$kde[capra_utm$stratum == "North"] <- north_vals[,2]
south_pts <- capra_utm[capra_utm$stratum == "South", ]
south_vals <- terra::extract(South_kde, terra::vect(south_pts))
capra_utm$kde[capra_utm$stratum == "South"] <- south_vals[,2]
summary(capra_utm$kde)

turkiye_dem <- elevation_30s(country = "TUR", path = "D:/Capra_DEM/")
tr_gadm <- gadm(country = "TUR", level = 1, path = "D:/Capra_DEM/")
batman_sinir <- tr_gadm[tr_gadm$NAME_1 == "Batman", ]
batman_dem <- crop(turkiye_dem, batman_sinir)
batman_dem <- mask(batman_dem, batman_sinir)
plot(batman_dem, main = "Batman Say??sal Y??kseklik Modeli (DEM)")

batman_slope <- terrain(batman_dem, v = "slope", unit = "degrees")
capra_pts_proj <- st_transform(capra_sf, crs(batman_dem))
env_vals <- terra::extract(c(batman_dem, batman_slope), terra::vect(capra_pts_proj))
data$elevation_m <- env_vals[, 2]
data$slope_deg   <- env_vals[, 3]
summary(data[, c("elevation_m", "slope_deg")])

capra_utm$elevation_m <- data$elevation_m
capra_utm$slope_deg   <- data$slope_deg
capra_utm$log_kde <- log(capra_utm$kde)
model_lm <- lm(log_kde ~ elevation_m + slope_deg + stratum,data = capra_utm)
summary(model_lm)

coords <- st_coordinates(capra_utm)
knn <- knearneigh(coords, k = 4)
nb  <- knn2nb(knn)
lw  <- nb2listw(nb, style = "W")
moran.test(residuals(model_lm), lw)

Sys.setlocale("LC_ALL", "English")
Sys.setlocale("LC_ALL", "C")

water_frac <- landcover(var = "water", path = "D:/Capra_DEM/")
built_frac <- landcover(var = "built", path = "D:/Capra_DEM/")
trees_frac <- landcover(var = "trees", path = "D:/Capra_DEM/")
crop_frac  <- landcover(var = "cropland", path = "D:/Capra_DEM/")
bare_frac  <- landcover(var = "bare", path = "D:/Capra_DEM/")

water_batman <- crop(water_frac, batman_sinir)
built_batman <- crop(built_frac, batman_sinir)
trees_batman <- crop(trees_frac, batman_sinir)
crop_batman  <- crop(crop_frac, batman_sinir)
bare_batman  <- crop(bare_frac, batman_sinir)

water_batman <- project(water_batman, crs(capra_utm))
built_batman <- project(built_batman, crs(capra_utm))
trees_batman <- project(trees_batman, crs(capra_utm))
crop_batman  <- project(crop_batman, crs(capra_utm))
bare_batman  <- project(bare_batman, crs(capra_utm))

capra_vect <- vect(capra_utm)
capra_utm$water_perc <- terra::extract(water_batman, capra_vect)[,2]
capra_utm$built_perc <- terra::extract(built_batman, capra_vect)[,2]
capra_utm$trees_perc <- terra::extract(trees_batman, capra_vect)[,2]
capra_utm$crop_perc  <- terra::extract(crop_batman, capra_vect)[,2]
capra_utm$bare_perc  <- terra::extract(bare_batman, capra_vect)[,2]

model_ols <- lm(log_kde ~ elevation_m + slope_deg + water_perc + built_perc + trees_perc + crop_perc + bare_perc + stratum,
                data = capra_utm)
moran.test(residuals(model_ols), lw)

model_sem  <- errorsarlm(log_kde ~ elevation_m + slope_deg + water_perc + built_perc + trees_perc + crop_perc + bare_perc + stratum,
                         data = capra_utm, listw = lw)
moran.test(residuals(model_sem), lw)
model_sar  <- lagsarlm(log_kde ~ elevation_m + slope_deg + water_perc + built_perc + trees_perc + crop_perc + bare_perc + stratum,
                       data = capra_utm, listw = lw)
moran.test(residuals(model_sar), lw)
model_sdm  <- lagsarlm(log_kde ~ elevation_m + slope_deg + water_perc + built_perc + trees_perc + crop_perc + bare_perc + stratum,
                       data = capra_utm, type="mixed", listw = lw)
moran.test(residuals(model_sdm), lw)
AIC(model_ols, model_sem, model_sar, model_sdm)

reduced_sem <- errorsarlm(log_kde ~ elevation_m + slope_deg + built_perc + bare_perc + stratum, data = capra_utm, listw = lw)
moran.test(residuals(reduced_sem), lw)
summary(reduced_sem)

AIC(model_ols, model_sem, reduced_sem, model_sar, model_sdm)

coef_table <- data.frame(variable = c("Elevation (100 m)", "Slope (1??)", "Built (10%)", "Bare (10%)", "South vs North"),
                         estimate = c(-0.026*100, 0.67, -31, 6, 48))
ggplot(coef_table, aes(x = variable, y = estimate)) + geom_point(size = 3) + geom_hline(yintercept = 0, linetype = "dashed") + coord_flip() +
  ylab("Percent Change in KDE (%)") + xlab("") + theme_minimal(base_size = 14)

sum_sem <- summary(reduced_sem)
coef_df <- as.data.frame(sum_sem$Coef)
coef_df$variable <- rownames(coef_df)
coef_df <- coef_df[coef_df$variable != "(Intercept)", ]
coef_df$percent_change <- (exp(coef_df$Estimate) - 1) * 100
coef_df$lower_CI <- (exp(coef_df$Estimate - 1.96 * coef_df$`Std. Error`) - 1) * 100
coef_df$upper_CI <- (exp(coef_df$Estimate + 1.96 * coef_df$`Std. Error`) - 1) * 100
coef_df


ggplot(coef_df, aes(x = reorder(variable, percent_change), y = percent_change)) + geom_point(size = 3) + geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), 
width = 0.2) +  geom_hline(yintercept = 0, linetype = "dashed") + coord_flip() + ylab("Percent Change in KDE (%)") + xlab("") +theme_minimal(base_size = 14)

beta <- coef(reduced_sem)["elevation_m"]
(exp(beta * 100) - 1) * 100

beta <- coef(reduced_sem)["bare_perc"]
(exp(beta * 0.10) - 1) * 100

coefs <- coef(reduced_sem)
effects <- data.frame(variable = c("Elevation (100 m)","Slope (1??)","Built (10%)","Bare (10%)"), percent_change = c((exp(coefs["elevation_m"] * 100) - 1) * 100,
  (exp(coefs["slope_deg"] * 1) - 1) * 100,(exp(coefs["built_perc"] * 0.10) - 1) * 100,(exp(coefs["bare_perc"] * 0.10) - 1) * 100))
effects

ggplot(effects, aes(x = reorder(variable, percent_change),y = percent_change)) + geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") + coord_flip() + labs(x = "",y = "Percentage change in KDE intensity (%)",
    title = "Effect sizes from Spatial Error Model") + theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"))
