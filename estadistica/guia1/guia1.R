##ejercicio1
datos <- read.csv(
  "Debernardi.csv"
)
diagnosticos <- datos$diagnosis
repeticiones <- as.data.frame(table(diagnosticos))
repes_matriz <- as.matrix(repeticiones)

barplot(repes_matriz, 
        main = 'frecuencia de diagnostico',
        xlab = 'diagnostico',
        ylab= 'frecuencia',
)
