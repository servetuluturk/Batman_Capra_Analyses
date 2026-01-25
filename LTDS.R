library(Distance)
obs <- read.csv("C:/Users/aleyn/OneDrive/Belgeler/Capra_R/Data/csv/obs.csv")
sample <- read.csv("C:/Users/aleyn/OneDrive/Belgeler/Capra_R/Data/csv/sample.csv")
region <- read.csv("C:/Users/aleyn/OneDrive/Belgeler/Capra_R/Data/csv/region.csv")
conversion <- convert_units("meter", "kilometer", "square kilometer")
combined_data <- merge(obs, sample, by = "Sample.Label")
combined_data <- merge(combined_data, region, by = "Region.Label")
print(paste("Birle??tirme sonras?? toplam sat??r say??s??:", nrow(combined_data)))
names(combined_data)
head(combined_data)
conversion <- convert_units("meter", "kilometer", "square kilometer")
model_final <- ds(data = combined_data,key = "hn",adjustment = "herm",convert_units = conversion,
                  truncation = list(left = 50, right = "10%"),formula = ~Region.Label)
summary(model_final)
gof_sonuclar <- gof_ds(model_final)
gof_sonuclar
par(mfrow = c(1, 3))
plot(model_final, subset = (Region.Label == "North"), 
     main = "Kuzey (North)", 
     xlab = "Distance (m)", ylab = "Detection Prob.")
plot(model_final, subset = (Region.Label == "South"), 
     main = "Guney (South)", 
     xlab = "Distance (m)", ylab = "Detection Prob.")
plot(model_final, 
     main = "Toplam (Total)", 
     xlab = "Distance (m)", ylab = "Detection Prob.")
par(mfrow = c(1, 1))

yogunluklar <- c(5.847, 6.953)
bolgeler <- c("North", "South")
bp <- barplot(yogunluklar, 
              names.arg = bolgeler, 
              col = c("steelblue", "darkorange"), 
              main = "Yaban Kecisi Yogunluklari (Density)",
              ylab = "Birey / km2", 
              ylim = c(0, 10),
              border = "white")
text(x = bp, 
     y = yogunluklar + 0.4, 
     labels = round(yogunluklar, 2), 
     font = 2, 
     cex = 1.2)

install.packages("ggplot2")
library(ggplot2)
bolgeler <- c("North", "South")
yogunluklar <- c(5.847, 6.953)
alt_sinir <- c(2.735, 4.035)
ust_sinir <- c(12.501, 11.982)

bp <- barplot(yogunluklar, names.arg = bolgeler, ylim = c(0, 15), 
              col = c("gray80", "gray40"), 
              main = "Yaban Kecisi Yogunluk Tahminleri (%95 GA)",
              ylab = "Birey / km2", border = "black")

arrows(x0 = bp, y0 = alt_sinir, x1 = bp, y1 = ust_sinir, 
       angle = 90, code = 3, length = 0.1, lwd = 2)

text(x = bp, y = ust_sinir + 0.8, labels = round(yogunluklar, 2), font = 2)

avg_p <- summary(model_final)$ds$average.p

genislik <- model_final$ddf$meta.data$width - 50

esw_kesin <- avg_p * genislik

print(paste("Ortalama Alg??lama Olas??l?????? (p):", round(avg_p, 4)))
print(paste("Etkin Serit Genisligi (ESW):", round(esw_kesin, 2), "metre"))


