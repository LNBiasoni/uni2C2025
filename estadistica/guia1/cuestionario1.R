install.packages("Lock5Data")
library(Lock5Data)
data("FloridaLakes")
help("FloridaLakes")

aa <- hist(FloridaLakes$Alkalinity, prob= TRUE)
names(aa)

max_densidad <- max(aa$density)
sprintf("%.4f", max_densidad)
print(max_densidad)
indice_max <- which.max(aa$density)
print(indice_max)

media <- mean(FloridaLakes$Alkalinity)
sprintf("%.4f", media)
mediana <- median(FloridaLakes$Alkalinity)
sprintf("%.4f", mediana)

mediaPodada20 <- mean(FloridaLakes$Alkalinity, trim = 0.2)
sprintf("%.4f", mediaPodada20)

total_obs <- sum(aa$counts)

frecuencia_menor_o_igual_a_40 <- aa$counts[1] + aa$counts[2]

probabilidad_estimada <- frecuencia_menor_o_igual_a_40 / total_obs

print(probabilidad_estimada)

boxplot(FloridaLakes$Alkalinity)

densidad_alcalinidad <- density(FloridaLakes$Alkalinity)

plot(densidad_alcalinidad,
     main = "Gráfico de Densidad de la Alcalinidad",
     xlab = "Alcalinidad (mg/L)",
     ylab = "Densidad")