##### Ejercicio 1

#Construye una matriz que dada una entrada del tipo

texto <- c('NAME:Maria /COUNTRY:uruguay /EMAIL:mariaUY@gmail.com',
          'NAME:Paul/COUNTRY:UK /EMAIL:PaulUK@gmail.com',
          'NAME:John /COUNTRY:USA /EMAIL:JohnUSA@gmail.com',
          'NAME:Carlos /COUNTRY:Spain /EMAIL:CarlosSP@gmail.com')

#devuelva un tabla con columnas name, country y correo (con los datos correspondientes).

limpiar <- function(x){
  m <- matrix(unlist(strsplit(texto,"[/]")),byrow=T,ncol=3)
  data.frame(names= gsub("([A-Z]+:)|/","",m[,1]),
             countries=gsub("([A-Z]+:)|/","",m[,2]),
             emails=gsub("([A-Z]+:)|/","",m[,3]))}



tabla <- function(x){
  data.frame(names = gsub("NAME: *(.*) */COUNTRY.*", "\\1", x),
             countries = gsub("NAME.*/COUNTRY: *(.*) */EMAIL.*", "\\1", x),
             emails = gsub("NAME.*/COUNTRY.*/EMAIL: *(.*) *", "\\1", x))
}



tabla(texto)

##### Ejercicio 2

#Crea un paquete de R con dos o tres funciones tontas.
#Una de ellas tiene que llamarse suma.dos.numeros y tiene que aceptar dos parámetros: los números que quieres sumar. Luego súbelo a Github.

library(devtools)
create("repo")


install_github('mescolano/master2016/ejerciciosR')

git init

###### Ejercicio 3

#Crea una función que admita como argumento dos cadenas de texto y compruebe si la una es un anagrama de la otra.

anagrama <- function(a,b){
  a.sorted <- toString(sort(strsplit(a,"")[[1]]))
  b.sorted <- toString(sort(strsplit(b,"")[[1]]))
  a.sorted == b.sorted
}

#Más compacto
anagrama <- function(a,b){
  toString(sort(strsplit(a,"")[[1]])) == toString(sort(strsplit(b,"")[[1]]))
}

#Puro DRY
anagrama <- function(a, b){
  foo <- function(x) toString(sort(strsplit(x, "")[[1]]))
  foo(a) == foo(b)
}

anagrama('amor','mora')
anagrama('casa','saco')
anagrama('casa','casaca')

##### Ejercicio 4
#Crea una función que dado la lista de nombres de ficheros tales como

a <- c("ventas_norte_20161225.txt", "propuestas_sur_20161211.csv")

#cree un data.frame con las columnas tipo, zona y fecha con sus formatos correspondientes.
#La extensión del fichero puede ser cualquiera (pero siempre acaba en varias letras, una o más, precedidas de un punto).

lista <- function(x){
  m <- matrix(unlist(strsplit(x,"[_.]")),byrow=T,ncol=4)
  data.frame(tipo=m[,1],
             zona=m[,2],
             fecha=m[,3])
}

lista(a)

##### Ejercicio 5
#Baja datos en Excel de http://www.geoportalgasolineras.es/ y léelos en R.
#Puede que quieras exportarlos a CSV o investigar cómo leer Excel directamente a R.

datos <- read.delim2('C:/Users/Marcos/Documents/Master_Data_Science/R/preciosEESS_es.txt')

colnames(datos)
head(datos)

##5.1
#Limpia lo que tengas que limpiar y crea un diagrama de cajas: precios (del tipo de combustible que quieras) por comunidad autónoma.

boxplot(datos$Precio.gasolina.95 ~ datos$Provincia,las=2)

##5.2
#Usando los datos anteriores, haz un diagrama de barras mostrando el número de gasolineras de las tres marcas más populares y una cuarta con el resto.

por.marca <- summary(datos$Rótulo)[1:3]
por.marca$OTRAS <- sum(summary(datos$Rótulo)[-3:-1])
por.marca
barplot(unlist(por.marca),ylab='Número de EESS en España')

head(datos[order(-datos$Precio.gasolina.95),],3)

##5.3
#Construye a partir de la anterior una tabla que contenga las tres gasolineras más caras por provincia.

#Con data.table
library(data.table)
datos <- datos[!is.na(datos$Precio.gasolina.95),]
setorder(setDT(datos), -Precio.gasolina.95)[, head(.SD, 3), by = Provincia]

#Con R base
datos.ord <- datos[order(-datos$Precio.gasolina.95),]
top3P <- by(datos.ord,datos.ord$Provincia,head,n=3)
top3P.df <- Reduce(rbind,by(datos.ord,datos.ord$Provincia,head,n=3))
top3P.df[,c("Provincia","Precio.gasolina.95")]

##5.4
#Pinta la latitud y longitud de las gasolineras (con un diagrama de dispersión).
#Usa un filtro (sobre las coordenadas, no sobre la columna de CCAA) para excluir las de Canarias y repite el ejercicio.

plot(datos$Longitud,datos$Latitud)

datos.pen <- datos[(datos$Latitud > 33),]
plot(datos.pen$Longitud,datos.pen$Latitud)

top100 <- head(datos[order(-datos$Precio.gasolina.95),],100)
plot(top100$Longitud,top100$Latitud)

###### Ejercicio 6
#En una empresa de seguros, entran siniestros de acuerdo con una distribución de Poisson de parámetro 1000 (al mes).
#Cada siniestro le cuesta a la empresa una cantidad que es lognormal de parámetros mu = 3 y sigma = 3.
#Crea una función que calcule una simulación del coste mensual de los siniestros.
#Obtén una muestra grande y construye el histograma de la distribución de costes.

Nsiniestros <- rpois(1,1000)
coste <- sum(rlnorm(Nsiniestros,meanlog=3,sdlog=3))
coste
res <- replicate(10000,sum(rlnorm(rpois(1,1000),meanlog=3,sdlog=3)))
hist(res,breaks=1000,xlim=c(0,1e7))

#6.1
#Repite el ejercicio anterior para una empresa que vende cachivaches por internet.
#Las visitas son Poisson de parámetro 1000, el 1% de los clientes compra y, cuando compran, el importe es lognormal de parámetros mu = 3 y sigma = 3.

ingresos <- sum(rlnorm(rpois(1,1000)*0.01,meanlog=3,sdlog=3))
res.ing <- replicate(10000,sum(rlnorm(rpois(1,1000)*0.01,meanlog=3,sdlog=3)))
hist(res.ing,breaks=5000,xlim=c(0,5e5))

##### Ejercicio 7
#En un país viven 47M de habitantes; de ellos, 23M pertenecen a la población activa. Se hace una encuesta y se extrae una muestra de 180000.
#De ellos, 90000 pertenecen a la población activa (trabajan o quieren trabajar, tengan o no empleo).
#Resulta que de ellos, 17019 dicen estar en el paro (tasa de paro del 18.91%).
#Calcula el histograma de los posibles resultados que se obtendrían al repetir la encuesta.

#Pista: supón que el 18.91% de los 23M son parados. Extrae muetras de 90000 de ellos y cuenta cuántos están en el paro.
#Saldrán porcentajes similares pero no exactamente iguales al 18.91.
#Mide esa dispersión, que debería ser (parecido a, en una aproximación burda) el error de la encuesta.

v <- rep(0:1, c(23e6*(1-0.1891),23e6*0.1891))
res.enc <- replicate(100000,mean(sample(v,90000)))
hist(res.enc,breaks=50)

##### Ejercicio 8
#Toma un texto largo en Español (p.e., el Quijote) y:

#Ponlo en minúsculas.
#Extrae sus palabras.
#Calcula la frecuencia de cada letra.
#Para cada palabra, calcula la frecuencia promedio de cada letra y crea un histograma.
#Calcula la frecuencia media de las letras de “asacd” y de “kkjazwu”.
#Compáralas con las frecuencias promedio (¿dónde caen en el histograma?).

#¿Te das cuenta de que has creado un sistema que detecta la segunda palabra como no española?

texto <- readLines('texto_sonatas.txt', encoding = 'UTF-8')
texto[100]
letras <- unlist(strsplit(texto,""))
letras <- tolower(letras)
letras <- gsub("[[:punct:]]","",letras)
letras3 <- gsub("[[:blank:]]"," ",letras)
letras3 <- gsub("[[:space:]]"," ",letras3)

alfabeto <- c(letters,'á','é','í','ó','ú','ü','ñ',' ')

?substr

letras2 <- as.factor(letras)
frec.texto <- summary(letras2)
frec.texto <- summary(as.factor(letras3))
frec.texto <- frec.texto[-1:-3]
frec.texto <- frec.texto/sum(frec.texto)
barplot(frec.texto)

##

palabras <- unlist(strsplit(texto," "))
palabras[1:10]
palabras <- gsub("[[:punct:]]","",palabras)
palabras <- tolower(palabras)
summary(as.factor(palabras))

library(stringr)

texto <- unlist(texto)
texto.limpio <- tolower(texto)
texto.limpio <- gsub("[[:punct:]]","",texto.limpio)
sum(str_detect(texto.limpio2,'ab'))
sum(str_detect(texto.limpio2,"[a-z]"))
sum(str_detect(texto.limpio2,'a'))
?str_detect
?gregexpr
texto.limpio2 <- unlist(texto.limpio)
texto.limpio2[10]

sum(str_detect(palabras,'ab'))
sum(str_detect(palabras,'^a'))
sum(str_detect(palabras,'a$'))
sum(str_detect(palabras,'a'))


palabras[str_detect(palabras,'ab')]

matriz <- matrix(, nrow=length(alfabeto), ncol=length(alfabeto))
dimnames(matriz) <- list(alfabeto,alfabeto)
matriz[is.na(matriz)] <- 0
matriz

rastrear <- function(a,b){
  alfabeto <- c(letters,'á','é','í','ó','ú','ü','ñ',' ')
  matriz <- matrix(, nrow=length(alfabeto), ncol=length(alfabeto))
  dimnames(matriz) <- list(alfabeto,alfabeto)
  matriz[is.na(matriz)] <- 0
  matriz[a,b] <- matriz[a,b] + 1
  return(matriz)
}

mapply(rastrear, letras[-length(letras)], letras[-1])

mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))



















