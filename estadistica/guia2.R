#Generar n = 100 datos como si provinieran de una muestra aleatoria X1, ...Xn donde
#Xi es una variable aleatoria con distribucion ε(λ) con λ = 1 y graficar su funcion de
#distribucion empırica junto con la verdadera funcion de distribucion acumulada. En
#adelante, para simplificar diremos “Generar una muestra de tamaño n de una dis-
#tribucion ε(λ)”. Nos referiremos a los valores como observaciones o datos.

set.seed(42)
datos <- rexp(100, rate = 1)

Omom <- 1/mean(datos)

Omv <- 1/mean(datos)

f_verdadera <- function(x){
  1-exp(-1 * x)
}

f_empirica <- ecdf(datos)

x<- seq(from = 0, to = max(datos), length.out = 50)


plot(f_empirica,
     main = "Función de Distribución Empírica vs Verdadera FDA",
     xlab = "Valores de la variable",
     ylab = "Probabilidad acumulada",
     col = "palevioletred",
     pch = 16)


lines(x,
      f_verdadera(x),
      col = "violet",
      lwd = 2)


legend("bottomright",
       legend = c("FDE (Empírica)", "FDA (Verdadera)"),
       col = c("palevioletred", "violet"),
       lty = c(NA, 1),
       pch = c(16, NA),
       lwd = c(NA, 2))

f_estimador <- function(x){
  1 - exp(-Omom * x)
}

plot(f_empirica,
     main = "Función de Distribución Empírica vs Verdadera FDA",
     xlab = "Valores de la variable",
     ylab = "Probabilidad acumulada",
     col = "palevioletred",
     pch = 16)


lines(x,
      f_verdadera(x),
      col = "violet",
      lwd = 2)
lines(x,
      f_estimador(x),
      col = "purple",
      lwd = 3)


legend("bottomright",
       legend = c("FDE (Empírica)", "FDA (Verdadera)", "FDA (estimador)"),
       col = c("palevioletred", "violet", "purple"),
       lty = c(NA, 1,2),
       pch = c(19, NA, NA),
       lwd = c(NA, 2,2))

##EJERCICIO 15
datos <- runif(6, rate = 3)

Emv <- function(x){
  return(max(x))
}

Emom <- function(x){
  return(mean(x)*2)
}

Emodificado <- function(x){
  n= length(x)
  return(max(x)*((n+1)/n))
} 

verosimilitud = c()
momentos = c()
modificado= c()

estimadores <- function(n, O=3){
  for ( i in 1:1000){
    datos <- runif(n, rate = 3)
    verosimilitud <- c(verosimilitud, Emv(datos))
    momentos <- c(momentos, Emom(datos))
    modificado <- c(modificado, Emodificado(datos))
  }
  
}



