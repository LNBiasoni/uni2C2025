##ejercicio1
datos <- read.csv(
  "Debernardi.csv"
)
diagnosticos <- datos$diagnosis
repeticiones <- table(diagnosticos)
barplot(repeticiones,
        ylab = "frecuencia",
        xlab = "diagnostico",
        main = "frecuencia del diagnostico",
        col = c("darkseagreen", "lightgoldenrod", "salmon" ),
        names.arg = c("Control", "Benigno", "ACPD"),
        border = "black") 
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)

#ejercicio2 
library(dplyr)
datos2 <- read.csv("datos_titanic.csv")

sobrevivientesM <- sum(datos2$Sex == "female" & datos2$Survived == 1)
print(paste("Número de mujeres que sobrevivieron:", sobrevivientesM))


sobrevivientestotales <- sum(datos2$Survived == 1)
print(paste("Número total de sobrevivientes:", sobrevivientestotales))


ProbaM_sisobrevevivio <- sobrevivientesM / sobrevivientestotales
print(paste("Probabilidad de ser mujer dado que sobrevivió:", ProbaM_sisobrevevivio))

cantidadM <- sum(datos2$Sex == "female")


total_pasajeros <- nrow(datos2)

probaM_correcta <- cantidadM / total_pasajeros
print(probaM_correcta)


tabla_contingencia <- table(datos2$Pclass, datos2$Survived)
colnames(tabla_contingencia) <- c("No Sobrevivió (0)", "Sobrevivió (1)")
rownames(tabla_contingencia) <- c("Primera (1)", "Segunda (2)", "Tercera (3)")
print(tabla_contingencia)

total_clase1 <- sum(datos2$Pclass == 1)
total_clase2 <- sum(datos2$Pclass == 2)
total_clase3 <- sum(datos2$Pclass == 3)
sobrevivientes_por_clase <- table(datos2[datos2$Survived == 1, "Pclass"])

barplot(sobrevivientes_por_clase,
        main = " cantidad sobrevivientes por clase ",
        xlab = "clase", ylab = "Frecuencia",
        col = c("lightgoldenrod", "ivory3", "indianred"),
        names.arg = c("1", "2", "3"),
        border = "black") 
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
       

sobrevivientes_clase1 <- tabla_contingencia[1, 2]
sobrevivientes_clase2 <- tabla_contingencia[2, 2]
sobrevivientes_clase3 <- tabla_contingencia[3, 2]

proba_clase1 <- sobrevivientes_clase1 / total_clase1
proba_clase2 <- sobrevivientes_clase2 / total_clase2
proba_clase3 <- sobrevivientes_clase3 / total_clase3

print(paste("Probabilidad de sobrevivir en 1era clase:", proba_clase1))
print(paste("Probabilidad de sobrevivir en 2da clase:", proba_clase2))
print(paste("Probabilidad de sobrevivir en 3era clase:", proba_clase3))

##Ejercicio3
iridio <- read.table("iridio.txt", header = TRUE)
rodio <- read.table("rodio.txt", header= TRUE)
temp_i <-  as.numeric(iridio[["iridio"]])
temp_r <- as.numeric(rodio[["rodio"]])

hist(temp_i, prob = TRUE, ylab = "densidad", xlab= "temperatura", main = "temperatura sublimacion (°C) del iridio")
grid(nx = NA, ny = NULL, lty = 2, col = "black", lwd = 1)
hist(temp_i, prob = TRUE, add = TRUE, col = "gray21") 

hist(temp_r, prob = TRUE, ylab = "densidad", xlab= "temperatura", main = "temperatura sublimacion (°C) del rodio")
grid(nx = NA, ny = NULL, lty = 2, col = "black", lwd = 1)
hist(temp_r, prob = TRUE, add = TRUE, col = "honeydew4") 

mR <- data.frame(
  metal = rep("rodio", 21),
  temp = temp_r
)

mI <- data.frame(
  metal = rep("iridio",19),
  temp = temp_i
)

metales <- bind_rows(mR, mI)



boxplot(metales$temp ~ metales$metal)
stripchart(metales$temp ~ metales$metal, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = c("honeydew4", "gray21"))
par(mfrow = c(1, 2)) 
boxplot(temp_r, vertical = TRUE,main = "temperatura sublimacion rodio",
        ylab = "temperatura" )
stripchart(temp_r,
           vertical = TRUE,
           method = "jitter",
           add = TRUE,
           pch = 20,
           col = "honeydew4")

boxplot(temp_i, vertical = TRUE,main = "temperatura sublimacion iridio",
        ylab = "temperatura" )
stripchart(temp_i,
           vertical = TRUE,
           method = "jitter",
           add = TRUE,
           pch = 20,
           col = "gray21")

#calculo medidas de aproximacion de temp rodio
mediaR <- mean(temp_r)
medianaR <- median(temp_r)
mediaPodadaR20 <- mean(temp_r, trim = 0.2)
mediaPodadaR10 <- mean(temp_r, trim = 0.1)
medidasComparacionR <- c (mediaR, medianaR, mediaPodadaR10, mediaPodadaR20)
print(medidasComparacionR)

##Grafico comparacion de medidas de aproximacion para el rodio
par(mfrow = c(1, 2))
box_grafico <- boxplot(temp_r,
                       main = "Comparación de Medidas de Tendencia Central",
                       ylab = "temperatura",
                       names = c("Distribución"))
points(x = 1, y = mediaR, pch = 17, col = "lightpink", cex = 1.5)
points(x = 1, y = mediaPodadaR10, pch = 15, col = "maroon4", cex = 1.5)
points(x = 1, y = mediaPodadaR20, pch = 18, col = "mediumpurple1", cex = 1.5)
points(x = 1, y = mediana, pch = 16, col = "black", cex = 1.5)
legend("topright",
       legend = c("Mediana", "Media", "Media Truncada 10%", "Media Truncada 20%"),
       col = c("black", "lightpink", "maroon4", "mediumpurple1"),
       pch = c(16, 17, 15, 18),
       cex = 0.8)

medidasR <- c(mediaR, medianaR, mediaPodadaR10, mediaPodadaR20) #vector de aproximaciones
par(mfrow = c(1, 2))#para que cada grafico este separado
barplot(medidasR-130,
        ylab = "Temperatura",
        xlab = "Cálculo de aproximación",
        yaxt = "n",
        main = "Aproximación de la temp. de sublimación de rodio",
        col = c("lightpink", "hotpink", "maroon4", "mediumpurple1"),
        names.arg = c("Media", "Mediana", "Podada 10%", "Podada 20%"),
        cex.names = 0.8,
        ylim= c(0,2.5),
        yaxs = "i"
)
axis(2, at = seq(0, 2, 0.5), labels = seq(130, 132, 0.5))
options(repr.plot.width = 8, repr.plot.height = 6)

##aproximaciones para la temp iridio
mediaI <- mean (temp_i)
medianaI <- median(temp_i)
mediaPodadaI20 <- mean(temp_i, trim = 0.2)
mediaPodadaI10 <-mean(temp_i, trim = 0.1)
medidasI <- c(mediaI, medianaI, mediaPodadaI10, mediaPodadaI20) #vector de iridio
print(medidasI)
par(mfrow = c(1, 2))
box_grafico <- boxplot(temp_i,
                       main = "Comparación de Medidas de Tendencia Central del iridio",
                       ylab = "temperatura",
                       names = c("Distribución"))
points(x = 1, y = mediaI, pch = 17, col = "lightpink", cex = 1.5)
points(x = 1, y = mediaPodadaI10, pch = 15, col = "maroon4", cex = 1.5)
points(x = 1, y = mediaPodadaI20, pch = 18, col = "mediumpurple1", cex = 1.5)
points(x = 1, y = medianaI, pch = 16, col = "black", cex = 1.5)
legend("topright",
       legend = c("Mediana", "Media", "Media Truncada 10%", "Media Truncada 20%"),
       col = c("black", "lightpink", "maroon4", "mediumpurple1"),
       pch = c(16, 17, 15, 18),
       cex = 0.5)


par(mfrow = c(1, 2))
barplot(medidasI-158,
        ylab = "Temperatura",
        xlab = "Cálculo de aproximación",
        yaxt = "n",
        main = "Aproximación de la temp. de sublimación de iridio",
        col = c("lightpink", "hotpink", "maroon4", "mediumpurple1"),
        names.arg = c("Media", "Mediana", "Podada 10%", "Podada 20%"),
        cex.names = 0.8,
        ylim= c(0,2.5),
        yaxs = "i"
)
axis(2, at = seq(0, 2, 0.5), labels = seq(158, 160, 0.5))
options(repr.plot.width = 8, repr.plot.height = 6)


###Hallar los desvıos estandares, las distancias intercuartiles y las MAD muestrales como
#medidas de dispersion.

desvioRodio <- sd(temp_r)
desvioIridio <- sd(temp_i)

intercuartilR <- IQR(temp_r)
intercuartilI <- IQR(temp_i)

madR <- mad(temp_r)
madI <- mad(temp_i)

errores <- data.frame(
  medidas_e = c("devio estandar", "intercuartil", "mad"),
  rodio = c(desvioRodio, intercuartilR, madR),
  iridio = c(desvioIridio, intercuartilI, madI)
)

## cuantiles
quantilRodio<- quantile(temp_r,             # Vector numérico
         probs = c(0.90, 0.75, 0.50, 0.25, 0.10), # Cuantiles (Por defecto los cuartiles: 0, 0.25, 0.5, 0.75, 1)
         na.rm = FALSE, # Si TRUE, elimina los datos faltantes
         names = TRUE,  # Si TRUE, el resultado tendrá nombres
         type = 7,      # Entero entre 1 y 9 para seleccionar un algoritmo para calcular los cuantiles
         digits = 7,    # Si names = TRUE, es el número de fígitos de los porcentages
         ) 

print(quantilRodio)

quantiliridio<- quantile(temp_i,             # Vector numérico
                        probs = c(0.90, 0.75, 0.50, 0.25, 0.10), # Cuantiles (Por defecto los cuartiles: 0, 0.25, 0.5, 0.75, 1)
                        na.rm = FALSE, # Si TRUE, elimina los datos faltantes
                        names = TRUE,  # Si TRUE, el resultado tendrá nombres
                        type = 7,      # Entero entre 1 y 9 para seleccionar un algoritmo para calcular los cuantiles
                        digits = 7,    # Si names = TRUE, es el número de fígitos de los porcentages
) 

print(quantiliridio)

##EJERCICIO 4
##n un estudio nutricional se consideran las calorıas y el contenido de sodio de tres tipos de
##salchichas y se obtuvieron los datos que se encuentran en los archivos salchichas A.txt,
##salchichas B.txt y salchichas C.txt

salchichasA <- read.table("salchichas_A.txt", header = TRUE)
salchichasB <- read.table("salchichas_B.txt", header = TRUE)
salchichasC <- read.table("salchichas_C.txt", header = TRUE)
salchichasA <- cbind(salchichasA,rep("A", 20))
salchichasB <- cbind(salchichasB,rep("B", 17))
salchichasC <- cbind(salchichasC,rep("C", 17))

names(salchichasA) <- c("Calorias", "Sodio","tipo")
names(salchichasB) <- c("Calorias", "Sodio","tipo")
names(salchichasC) <- c("Calorias", "Sodio","tipo")
salchichas <- merge(salchichasA, merge(salchichasB,salchichasC, all = TRUE),all = TRUE)

write.table(salchichas, "salchichas.txt", sep = "   ", row.names = FALSE)

##histogramas de las calorias
#A
par(mfrow = c(1, 2))
hist(salchichasA$Calorias, prob = TRUE, ylab = "Densidad", xlab="calorias", main = "distribucion de las calorias de las salchichas del tipo A")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(salchichasA$Calorias , prob = TRUE, add = TRUE, col = "lightpink", xlab="calorias") 
#sodio
par(mfrow = c(1, 2))
hist(salchichasA$Sodio, prob = TRUE, ylab = "Densidad", xlab="Sodio", main = "distribucion del sodio de las salchichas del tipo A")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(salchichasA$Sodio , prob = TRUE, add = TRUE, col = "lightpink", xlab="Sodio") 


#B
#calorias
par(mfrow = c(1, 2))
hist(salchichasB$Calorias, prob = TRUE, ylab = "Densidad", xlab="calorias", main = "distribucion de las calorias de las salchichas del tipo B")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(salchichasB$Calorias , prob = TRUE, add = TRUE, col = "mediumpurple4", xlab="calorias") 
#sodio
par(mfrow = c(1, 2))
hist(salchichasB$Sodio, prob = TRUE, ylab = "Densidad", xlab="Sodio", main = "distribucion del sodio de las salchichas del tipo B")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(salchichasB$Sodio , prob = TRUE, add = TRUE, col = "mediumpurple4", xlab="Sodio") 


#C
#calorias
par(mfrow = c(1, 2))
hist(salchichasC$Calorias, prob = TRUE, ylab = "Densidad", xlab="calorias", main = "distribucion de las calorias de las salchichas del tipo C")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(salchichasC$Calorias , prob = TRUE, add = TRUE, col = "maroon4", xlab="calorias") 
#sodio
par(mfrow = c(1, 2))
hist(salchichasC$Sodio, prob = TRUE, ylab = "Densidad", xlab="sodio", main = "distribucion del sodio de las salchichas del tipo C")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(salchichasC$Sodio , prob = TRUE, add = TRUE, col = "maroon4", xlab="Sodio") 

##boxplot

#calorias
par(mfrow = c(1, 2))
boxplot(salchichas$Calorias ~ salchichas$tipo, ylab = "calorias", xlab= "tipos")
stripchart(salchichas$Calorias ~ salchichas$tipo, vertical = TRUE,method = "jitter", ylab= "calorias", xlab= "tipo",
           pch = 19, add = TRUE, col= c("lightpink", "mediumpurple4", "maroon4"))

#sodio
par(mfrow = c(1, 2))
boxplot(salchichas$Sodio ~ salchichas$tipo, ylab = "Sodio", xlab= "tipos")
stripchart(salchichas$Sodio ~ salchichas$tipo, vertical = TRUE,method = "jitter", ylab= "calorias", xlab= "tipo",
           pch = 19, add = TRUE, col= c("lightpink", "mediumpurple4", "maroon4"))

##EJERCICIO 5
#l conjunto de datos que figura en el archivo estudiantes.txt corresponde a 100 determi-
#naciones repetidas de la concentracion de ion nitrato (en μg/l), 50 de ellas corresponden a
#un grupo de estudiantes (Grupo 1) y las restantes 50 a otro grupo (Grupo 2).
#a) Estudiar si la distribuci´on de los conjuntos de datos para ambos grupos es normal,
#realizando los correspondientes histogramas y superponiendo la curva normal. Adem´as
#dibujar los qqplots para cada conjunto de datos superponiendo, en otro color, la recta
#mediante el comando qqline.
par(mfrow = c(1, 2))
estudiantes <- read.table("estudiantes.txt", header = TRUE)
hist(estudiantes$GRUPO1, prob = TRUE, ylab = "Densidad", xlab="concentracion ion nitrato", main = "distribucion de la concentracion del ion nitrato en los estudiantes")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(estudiantes$GRUPO1 , prob = TRUE, add = TRUE, col = "lightpink", xlab="concentracion ion nitrato") 
curve(dnorm(x, mean = mean(estudiantes$GRUPO1), sd = sd(estudiantes$GRUPO1)), add = TRUE, col = "hotpink", lwd = 2)

par(mfrow = c(1, 2))
hist(estudiantes$GRUPO2, prob = TRUE, ylab = "Densidad", xlab="concentracion ion nitrato", main = "distribucion de la concentracion del ion nitrato en el grupo2")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(estudiantes$GRUPO2 , prob = TRUE, add = TRUE, col = "mediumpurple", xlab="concentracion ion nitrato") 
curve(dnorm(x, mean = mean(estudiantes$GRUPO2), sd = sd(estudiantes$GRUPO2)), add = TRUE, col = "purple", lwd = 2)

par(mfrow = c(1, 2))
qqnorm(estudiantes$GRUPO1, main="QQplot de los estudiantes",ylab = "Cuantiles de la muestra",
       xlab = "Cuantiles teóricos")
qqline(estudiantes$GRUPO1, col = "hotpink", lwd = 2)

par(mfrow = c(1, 2))
qqnorm(estudiantes$GRUPO2, main="QQplot del grupo 2")
qqline(estudiantes$GRUPO2, col = "hotpink", lwd = 2)


##Le parece a partir de estos datos que ambos grupos est´an midiendo lo mismo? Respon-
#der comparando medidas de centralidad y de dispersi´on de los datos. Hacer boxplots
#paralelos.Le parece a partir de estos datos que ambos grupos est´an midiendo lo mismo? Respon-
#der comparando medidas de centralidad y de dispersi´on de los datos. Hacer boxplots
#paralelos.
par(mfrow = c(1, 2))
boxplot(estudiantes$GRUPO1, estudiantes$GRUPO2, 
        names = c("Grupo 1", "Grupo 2"), 
        main = "Comparación de los Grupos 1 y 2",
        ylab = "concentracion ion nitrato")
stripchart(estudiantes$GRUPO1, estudiantes$GRUPO2, vertical = TRUE,method = "jitter", ylab= "concentracion ion nitrato", xlab= "GRUPO",
           pch = 19, add = TRUE, col= c("lightpink", "mediumpurple4"))