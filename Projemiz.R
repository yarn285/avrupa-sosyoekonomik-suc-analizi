install.packages("readxl")
library(readxl)
veri <- read_excel(file.choose())
View(veri)

#Veri Tipini D??zeltme ve Numeric Yapma
veri$gdp <- as.numeric(as.character(veri$gdp))
veri$unemployment <- as.numeric(as.character(veri$unemployment))
veri$military <- as.numeric(as.character(veri$military))
veri$intentional_homicides <- as.numeric(as.character(veri$intentional_homicides)) # Bunu unutma!

#NA Doldurma (Mean ile)
veri$gdp[is.na(veri$gdp)] <- mean(veri$gdp, na.rm = TRUE)
veri$unemployment[is.na(veri$unemployment)] <- mean(veri$unemployment, na.rm = TRUE)
veri$military[is.na(veri$military)] <- mean(veri$military, na.rm = TRUE)
veri$intentional_homicides[is.na(veri$intentional_homicides)] <- mean(veri$intentional_homicides, na.rm = TRUE)

print("Eksik veriler ortalama ile dolduruldu")
set.seed(432) 
sum(is.na(veri)) 
View(veri)

#NORMAL DA??ILIM ANAL??Z??
par(mfrow=c(2,2))

hist(veri$gdp, main="GSYH (GDP) Histogram", col="skyblue", xlab="De??er", ylab="Frekans")
hist(veri$military, main="Askeri (Military) Histogram", col="plum", xlab="De??er", ylab="Frekans")
hist(veri$intentional_homicides, main="Cinayet (Homicides) Histogram", col="salmon", xlab="De??er", ylab="Frekans")
hist(veri$unemployment, main="????sizlik (Unemployment) Histogram", col="palegreen", xlab="De??er", ylab="Frekans")

qqnorm(veri$gdp, main="GSYH QQ Plot"); qqline(veri$gdp, col="darkorchid")
qqnorm(veri$military, main="Military QQ Plot"); qqline(veri$military, col="darkorchid")
qqnorm(veri$intentional_homicides, main="Homicides QQ Plot"); qqline(veri$intentional_homicides, col="darkorchid")
qqnorm(veri$unemployment, main="Unemployment QQ Plot"); qqline(veri$unemployment, col="darkorchid")

print("--- Shapiro-Wilk Test Sonu??lar?? ---")
shapiro.test(veri$gdp)
shapiro.test(veri$military)
shapiro.test(veri$intentional_homicides)
shapiro.test(veri$unemployment)

boxplot(veri$gdp, main = "GSYH Boxplot", col = "mistyrose", outline=F)
boxplot(veri$military, main = "Military Boxplot", col = "mistyrose", outline=F)
boxplot(veri$intentional_homicides, main = "Homicides Boxplot", col = "mistyrose", outline=F)
boxplot(veri$unemployment, main = "Unemployment Boxplot", col = "mistyrose", outline=F)




#AYKIRI DE??ER ANAL??Z??

#GDP
Q1 <- quantile(veri$gdp, 0.25)
Q3 <- quantile(veri$gdp, 0.75)
IQR_val <- Q3 - Q1

alt_sinir <- Q1 - 1.5 * IQR_val
ust_sinir <- Q3 + 1.5 * IQR_val
print(paste("GSYH Ayk??r?? De??er Say??s??:", sum(veri$gdp < alt_sinir | veri$gdp > ust_sinir)))


#M??L??TARY
Q1 <- quantile(veri$military, 0.25)
Q3 <- quantile(veri$military, 0.75)
IQR_val <- Q3 - Q1

alt_sinir <- Q1 - 1.5 * IQR_val
ust_sinir <- Q3 + 1.5 * IQR_val

print(paste("Military Ayk??r?? De??er Say??s??:", sum(veri$military < alt_sinir | veri$military > ust_sinir)))
median_clean <- median(veri$military[veri$military >= alt_sinir & veri$military <= ust_sinir], na.rm = TRUE)

veri$military <- ifelse(veri$military < alt_sinir | veri$military > ust_sinir,
                        median_clean, veri$military)


#INTENTIONAL HOMICIDES
Q1 <- quantile(veri$intentional_homicides, 0.25)
Q3 <- quantile(veri$intentional_homicides, 0.75)
IQR_val <- Q3 - Q1

alt_sinir <- Q1 - 1.5 * IQR_val
ust_sinir <- Q3 + 1.5 * IQR_val

print(paste("Homicides Ayk??r?? De??er Say??s??:", sum(veri$intentional_homicides < alt_sinir | veri$intentional_homicides > ust_sinir)))
median_clean <- median(veri$intentional_homicides[veri$intentional_homicides >= alt_sinir & veri$intentional_homicides <= ust_sinir], na.rm = TRUE)

veri$intentional_homicides <- ifelse(veri$intentional_homicides < alt_sinir | veri$intentional_homicides > ust_sinir,
                                     median_clean, veri$intentional_homicides)


#UNEMPLOYMENT
Q1 <- quantile(veri$unemployment, 0.25)
Q3 <- quantile(veri$unemployment, 0.75)
IQR_val <- Q3 - Q1

alt_sinir <- Q1 - 1.5 * IQR_val
ust_sinir <- Q3 + 1.5 * IQR_val

print(paste("Unemployment Ayk??r?? De??er Say??s??:", sum(veri$unemployment < alt_sinir | veri$unemployment > ust_sinir)))
median_clean <- median(veri$unemployment[veri$unemployment >= alt_sinir & veri$unemployment <= ust_sinir], na.rm = TRUE)

veri$unemployment <- ifelse(veri$unemployment < alt_sinir | veri$unemployment > ust_sinir,
                            median_clean, veri$unemployment)

View(veri)


#NORMAL??ZAYSON
min_max_normalizasyon <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

veri_normalize<-veri
veri_normalize$gdp <- min_max_normalizasyon(veri$gdp)
veri_normalize$military <- min_max_normalizasyon(veri$military)
veri_normalize$intentional_homicides <- min_max_normalizasyon(veri$intentional_homicides)
veri_normalize$unemployment <- min_max_normalizasyon(veri$unemployment)

View(veri_normalize)

#KORELASYON
analiz_verisi <- veri_normalize[, c("military", "intentional_homicides", "gdp", "unemployment")]

kor_matris <- cor(analiz_verisi)

print("Korelasyon Matrisi:")
print(kor_matris)

install.packages("ggcorrplot")
library(ggcorrplot)

#Korelasyon Matrisi G??rseli
ggcorrplot(kor_matris, 
           method = "circle", 
           lab = TRUE, 
           title = "De??i??kenler Aras?? Korelasyon Matrisi",)

#VIF
install.packages("car")
library(car)

vif_modeli <- lm(intentional_homicides ~ gdp + military + unemployment, data = veri_normalize)
vif_sonuclar <- vif(vif_modeli)

print("VIF De??erleri:")
print(vif_sonuclar)

high_vif <- vif_sonuclar[vif_sonuclar > 5]
print("Y??ksek VIF De??erleri:")
print(high_vif)

#K-MEANS ALGOR??TMASI
install.packages("cluster")
install.packages("tidyverse")
install.packages("caret")
install.packages("factoextra")
library(factoextra)
library(tidyverse)  
library(caret)      
library(cluster)    

set.seed(123)
kmeans_result <- kmeans(veri_normalize[, 3:6], centers = 3, nstart = 25)
kmeans_result


wss <- numeric(10)
for (k in 1:10) {
  kmeans_temp <- kmeans(veri_normalize[, 3:6], centers = k, nstart = 25)
  wss[k] <- kmeans_temp$tot.withinss 
}
wss


#WSS Grafi??i
plot(1:10, wss, type = "b", pch = 19, col = "gold", 
     xlab = "Kume Sayisi (K degeri)", ylab = "WSS (Within Sum of Squares)", 
     main = "Elbow Yontemi")

#K??meleme Sonu??lar?? G??rseli
fviz_cluster(kmeans_result, data = veri_normalize[, 3:6],
             geom = "point", ellipse.type = "euclid", 
             main = "K-means Kumeleme Sonuclari")


install.packages("cluster")
library(cluster)

#Silhouette Score G??rseli
sil_score <- silhouette(kmeans_result$cluster, dist(veri_normalize[, 3:6])) 
plot(sil_score, col = 1:3, main = "Silhouette Analizi") 


veri_normalize$cluster <- kmeans_result$cluster
veri_normalize

View(veri_normalize)
table(veri_normalize$cluster)