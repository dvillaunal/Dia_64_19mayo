## ----eval=FALSE, include=TRUE---------------------------------------
## "Protocolo:
## 
## 1. Daniel Felipe Villa Rengifo
## 
## 2. Lenguaje: R
## 
## 3. Tema: Cree funciones que manejen vectores en R  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)
## 
## 4. Fuentes:https://www.generatedata.com/?lang=es#t1"


## -------------------------------------------------------------------
# Exportamos la base1:
base1 <- read.csv(file = "Base1.csv", header = T, dec = ".", sep = ",")

base1 <- data.frame(base1)

# Ahora vamos a ver si los datos son como los que esperamos:
str(base1$N1)
str(base1$N2)

# Creamos la función atracción entre dos cuerpos:

fatt <- function(m1,m2,r){
  "Fuerza de atracción entre dos cuerpos"
  G <- 6.67*10^-11
  f <- (m1*m2*G)/(r^2)
  return(round(f,  digits = 2))
}

## Como las columnas se manifiestan como vectores calculamos el resultado de la atracion de las columnas:

"Pero antes creamos la masas con sus respectivas notaciones cientificas"

masa1 <- as.vector(base1$M1 * base1$N1)

masa2 <- as.vector(base1$M2 * base1$N2)

## Como la "r" esta dada en millones de metros entonces tenemos que convertirlos en millones de metros:
conversion <- function(x){
  "Convierte de km a metros, 1 km = 1000 m"
  m <- x*1000
  return(round(m, digits = 2))
}

Rm <- as.vector(conversion(base1$R))

    
## Una vez hecho esto, ahora si la funcion:
fuerza_att <- fatt(m1 = masa1, m2 = masa2, r = Rm)

print(fuerza_att)

# Ahora vamos agregar texto y exportamos:
FuerzaDeAtraccion <- paste("La Fuerza de atracción entre el Planeta 1 y Planeta 2 es: F = ", fuerza_att, "N")

FuerzaDeAtraccion <- data.frame("Fuerza de Atraccion entre dos Cuerpos" = FuerzaDeAtraccion)

write.table(FuerzaDeAtraccion,
            file = "FuerzaDeAtraccion.txt",
            row.names = F)


## -------------------------------------------------------------------
# Importamos la base:
base2 <- read.csv(file = "Base2.csv", header = T, sep = ",", dec = ".")


## Ahora vamos a crear la función:

presion <- function(v,n,t){
  "Calculamos la presión de la formula quimica de los gases ideales"
  R <- 0.0082
  p <- round((n*R*t)/v, digits = 4)
  return(p)
  
}

# Calculamos el nuevo vector por medio de vectores:
Presion <- presion(v = base2$vol, n = base2$NÂ., t = base2$tem)

#visualicemos la presion de los gases dados por filas
print(Presion)

## Ahora guardamos el nuevo vector en la base:
base2$Presion <- Presion

## Exportamos el siguiente texto

p <- paste("La Presion de: ", base2$Gas, "es =>", base2$Presion, "atom")

print(p)

p <- data.frame("Presion.Gases.Ideales" = p)

write.table(p, file = "PresionGas.txt", row.names = F)