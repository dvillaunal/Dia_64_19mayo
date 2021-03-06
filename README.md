```{r}
"Protocolo:

1. Daniel Felipe Villa Rengifo

2. Lenguaje: R

3. Tema: Cree funciones que manejen vectores en R  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)

4. Fuentes:https://www.generatedata.com/?lang=es#t1"
```

# Ejercicio 1:

Vamos a calcular la siguiente función de fuerza de atración de dos cuerpos planetarios de la via lactea en km al acercarse con diferentes planetas aletorios

Datos:

+ `m1`: Masa del planeta 1 en Kg

+ `n1`: Notación cientifica para el planeta 1 => _1e+#_ (SOlamente tendedremos el # despues se le añadira el `+1e`) 

+ `m2`: Masa del planeta 2 en Kg 

+ `n2`: Notación cientifica para el planeta 2 => _1e+#_ (Solamente tendedremos el # despues se le añadira el `+1e`)

+ `r`: distancia del centro del planeta 1 al centro del planeta 2 en Millones de Km

+ `g`: Constante gravitacional universal [6.67 * 10^(-11)]

![](data:image/png;base64,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)


```{r}
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

fuerza_att

# Ahora vamos agregar texto y exportamos:
FuerzaDeAtraccion <- paste("La Fuerza de atracción entre el Planeta 1 y Planeta 2 es: F = ", fuerza_att, "N")

FuerzaDeAtraccion <- data.frame("Fuerza de Atraccion entre dos Cuerpos" = FuerzaDeAtraccion)

write.table(FuerzaDeAtraccion,
            file = "FuerzaDeAtraccion.txt",
            row.names = F)
```

# Ejercicio 2:

Vamos a calcular la función de los gases ideales.

![](https://1.bp.blogspot.com/-UDFupRrZB_s/XaOTDJM1utI/AAAAAAAAKFQ/X0snaAfmxn0Ovis4H-aQU0IFpBxpQmUxACLcBGAsYHQ/s320/PV%253DnRT.jpg)

Calcularemos la presión de algunos gases

+ `vol`: volumen en litros

+ `n`: N° de moles

+ `tem` temperatura en Kelvin = t(C°) + 273

+ `R`: una constante 0.082 atom X Litros / mol X Kelvin

```{r}
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

p <- data.frame("Presion Gases Ideales" = p)

write.table(p, file = "PresionGas.txt", row.names = F)
```