install.packages(c("terra", "sf"))
library(terra)
library(sf)
setwd("C:/KDE_Analiz")
list.files()
batman_boundary <- rast("Batman.tif")
corine          <- rast("Batman_corine.tif")
water           <- rast("water.tif")
settlements     <- rast("settlements.tif")
forest          <- rast("forest.tif")
agriculture     <- rast("agriculture.tif")
capra_points    <- rast("Capra_points.tif")
kernel_arcmap   <- rast("Kernel_Capra.tif")

template <- batman_boundary

water       <- resample(water, template)
settlements <- resample(settlements, template)
forest      <- resample(forest, template)
agriculture <- resample(agriculture, template)
capra_points<- resample(capra_points, template)

capra_bin <- classify(capra_points, rcl = matrix(c(-Inf, 0, NA,0, Inf, 1),
                                                 ncol=3, byrow=TRUE))
kernel <- focalMat(capra_bin, d = 5000, type = "Gauss") 
kde_capra_r <- focal(capra_bin, w = kernel, fun = sum, na.policy = "omit")
writeRaster(kde_capra_r, "KDE_Capra_R.tif", overwrite=TRUE)
plot(kernel_arcmap, main="ArcMap KDE")


kernel_arcmap_fix <- resample(kernel_arcmap, kde_capra_r, method="bilinear")
kde_diff <- kde_capra_r - kernel_arcmap_fix
plot(kde_diff, main="KDE Fark (R - ArcMap)")

batman_ext <- ext(batman_boundary)
batman_ext_buffered <- ext(
  batman_ext$xmin - 5000,
  batman_ext$xmax + 5000,
  batman_ext$ymin - 5000,
  batman_ext$ymax + 5000)

freq(capra_points)

NAflag(capra_points) <- 2147483648
capra_points[capra_points == 2147483648] <- NA
freq(capra_points)
plot(capra_points)
capra_bin <- capra_points
capra_bin[!is.na(capra_bin)] <- 1
freq(capra_bin)
res(capra_points)

res(capra_points)     # 300 x 300 m
freq(capra_points)    # encounter de??erleri
sum(values(capra_points), na.rm=TRUE)

