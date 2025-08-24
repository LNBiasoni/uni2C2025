install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(aplpack)

datos<- read.table("ENNyS_menorA2.txt", header = TRUE)
freqSexo <-prop.table(table(datos$Sexo))*100
freqTipo_E <-prop.table(table(datos$Tipo_embarazo))*100


barplot(freqSexo , main = "Frequencia relativa (%) sexo del feto",
        xlab = "Sexo",          
        ylab = "Frecuencia",                   
        border = "black",                       
        col = c("lightpink", "lightsteelblue"))

pie(freqSexo, labels = c("femenino", "masculino"),
    col = c("lightpink", "lightsteelblue"),
    density = 50, angle = 90, 
    main = "Frequencia relativa (%) sexo del feto")

barplot(freqTipo_E, main = "Frequencia relativa (%) del tipo de embarazo",
        xlab = "tipo",           
        ylab = "Frecuencia",                    
        border = "black",                       
        col = c("palevioletred1", "peachpuff"))

pie(freqTipo_E,
    labels = c("multiple 3.347 %", "simple 96.652%"), 
    col = c("palevioletred1", "peachpuff"), 
    density = 50, angle = 90, 
    main = "Frequencia relativa (%) del tipo de embarazo")



datos <- read.table("ENNyS_menorA2.txt", header = TRUE) %>%
  mutate(across(c(Sexo, Tipo_embarazo), as.factor))
freqSexo <- prop.table(table(datos$Sexo)) * 100
freqTipo_E <- prop.table(table(datos$Tipo_embarazo)) * 100


barplot(freqSexo, main = "Frecuencia relativa (%) sexo del feto",
        xlab = "Sexo",
        ylab = "Frecuencia",
        border = "black",
        col = c("lightpink", "lightsteelblue"))


pie(freqSexo, labels = c("femenino", "masculino"),
    col = c("lightpink", "lightsteelblue"),
    density = 50, angle = 90,
    main = "Frecuencia relativa (%) sexo del feto")


barplot(freqTipo_E, main = "Frecuencia relativa (%) del tipo de embarazo",
        xlab = "Tipo",
        ylab = "Frecuencia",
        border = "black",
        col = c("palevioletred1", "peachpuff"))

pie(freqTipo_E,
    labels = c("multiple 3.347 %", "simple 96.652%"),
    col = c("palevioletred1", "peachpuff"),
    density = 50, angle = 90,
    main = "Frecuencia relativa (%) del tipo de embarazo")

tabla_conti <- prop.table(table(datos$Sexo, datos$Tipo_embarazo))
barplot(tabla_conti,
        main = "Distribución del tipo de embarazo por sexo",
        xlab = "Tipo de embarazo",
        ylab = "Conteo",
        col = c("lightpink", "lightsteelblue"),
        legend = rownames(tabla_conti),
        args.legend = list(x = "topright", title = "Sexo"))
variables = c("Edad", "Peso", "Perim_encef", "Talla")
for (variable in variables) {
  tabla<- datos[[variable]]
  titulo <- paste("Frecuencia de", variable)
  hist(tabla, main =titulo ,ylab = "frecuencia", xlab= variable, col="palevioletred")
  grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
  hist(tabla, add = TRUE, col = "palevioletred") 
}



