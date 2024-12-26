install.packages("ggplot2") #install packagesnya dulu
install.packages("visreg")
library(ggplot2)
library(visreg)

#membuat data simulasi
set.seed(123)  #reproduksibilitas
jam_bekerja <- runif(100, 20, 60)  #100 data jam bekerja antara 20 dan 60 jam
pendapatan <- 2000 + 50 * jam_bekerja + rnorm(100, mean = 0, sd = 200)  #pendapatan dengan noise
data <- data.frame(jam_bekerja, pendapatan)

#uji asumsi normalitas shapiro-wilk
model <- lm(pendapatan ~ jam_bekerja, data = data)
residuals <- model$residuals
normality_test <- shapiro.test(residuals)
print(normality_test)
#uji asumsi heteroskedastisitas plot residual
plot(model$fitted.values, residuals,
     main = "plot residual",
     xlab = "nilai fitted",
     ylab = "residuals")
abline(h = 0, col = "red")
#uji asumsi linearitas plot ccatter
plot(data$jam_bekerja, data$pendapatan,
     main = "Hubungan Jam Bekerja dan Pendapatan",
     xlab = "Jam Bekerja",
     ylab = "Pendapatan")
abline(model, col = "blue")
#analisis regresi linear
summary(model)
#visualisasi hasil regresi
ggplot(data, aes(x = jam_bekerja, y = pendapatan)) +
  geom_point() +  #menambahkan titik data
  geom_smooth(method = "lm", col = "blue") +  #menambahkan garis regresi
  labs(title = "Regresi Linear Sederhana",
       x = "Jam Bekerja",
       y = "Pendapatan")
#visualisasi menggunakan visreg
visreg(model, "jam_bekerja", main="visualisasi model regresi")

