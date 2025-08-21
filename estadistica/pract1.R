### ejercicios clase

##a
ejercicio1a <- function (){
  acum <- 0
  for (i in 1:1000){
    acum <- acum + i
  }
  return (acum)
}

ejercicio1b <-function(){
  acum <- 0
  i <-0
  while (acum <=10000){
    acum <- acum + i
    i<- i+1
  }
  return (i-1)
}

ejercicio1c <- function(vec){
  acum <- 0
  n<- length(vec)
  for (i in 1:n){
    if (vec[i]>0){
      acum <- acum + vec[i]
    }
    
  }
  return (acum)
}

print(ejercicio1a())
print(ejercicio1b())

vector1 <- c(2, -3, 1, 5)

print(ejercicio1c(vector1))

#ejercicio2
help("function")
f_p <- function(a){
  res <- a*(1-a)
  return(res)
}
  


graficar_fp<- function(esp){
  xs <- seq(0,1, by= esp )
  
  ys <- f_p(xs)
 
    
    plot(xs, ys) 
}
 
  

graficar_fp(0.2)
graficar_fp(0.3)
graficar_fp(0.01)
graficar_fp(0.05)

#ejercicio3
ejercicio3 <- function(){
  xs = seq(0, 2*pi, length.out= 100)
  ys= sin(xs)
  ys1= cos(xs)
  ys2= cos(xs^2)
  plot (xs, ys, type= "l", col="mediumpurple3", xlab= "x", ylab= "y", main= "funciones en R",
        xlim = c(0, 2*pi), ylim = c(-1, 1))
  lines(xs, ys1, col= "hotpink" )
  lines(xs, ys2, col="darkseagreen")
}

ejercicio3()

#ejercicio4
autos <- read.csv(
  "autos(1).txt", sep=" "
  #"nombree.csv"
)

fila <- function(a){
  autos <- read.csv(
    "autos(1).txt", sep=" "
    #"nombree.csv"
  )
  res <- autos[a,]
  return(res)
}

columna <- function(a){
  autos <- read.csv(
    "autos(1).txt", sep=" "
  )
  res <- autos[,a]
}

sumaALgunasfilas <- function(a,b){
  autos <- read.csv(
    "autos(1).txt", sep=" "
    #"nombree.csv"
  )
  fil <- autos$precio
  res<- sum(fil[a:b])
}

sumacolumnas <- function(){
  autos <- read.csv(
    "autos(1).txt", sep=" "
  )
  
  apply(autos ,2 , sum )
}

sumafilas <- function(autos){
  apply(autos,1, sum )
}

graf4 <- function (autos){
  precio <- autos$precio
  calidad <-autos$calidad
  plot (precio, calidad, col="mediumpurple3", xlab= "precio", ylab= "calidad", main= "calidad de los autos en base a su precio",
        +         xlim = c(0, max(precio)), ylim = c(0, max(calidad)))
}

graf4(autos)
  