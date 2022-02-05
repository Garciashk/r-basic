## Lectura del archivo cartera 
## Revisar que el archivo cartera se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Copiar los archivos descargados en la ruta que surge de correr el 
## comando anterior

cartera <- read.csv("cartera.csv", header=TRUE, sep=";", dec=",")

## Instalaci?n de paquetes
install.packages("readxl")
# paquetes <- c("psych","e1071", "ggplot2")
install.packages(paquetes)

## Carga de paquetes
library(readxl)
library(psych)
library(e1071)
library(ggplot2)

## Lectura del archivo en excel
telco <- read_excel("services2.xls")

## Visualizaci?n del inicio y el final del conjunto de datos
head(telco, n=3)
tail(telco)

## Dimensi?n del conjunto
dim(telco)

## Nombres de las columnas
colnames(telco)

## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(cartera)

## Revisar el tipo de dato de una variable en particular
## mediante el comando class

class(cartera$yearsadress)
class(cartera$retrasos)

## Cambio en el tipo de dato de las variables "yearadress", "retrasos" y "tipoips"
## mediante los comandos as.numeric() y as.factor()
cartera$yearsadress <- as.numeric(cartera$yearsadress)
cartera$retrasos <- as.factor(cartera$retrasos)
cartera$tipoips <- as.factor(cartera$tipoips)

#Veamos c?mo qued? el conjunto
str(cartera)

# --- Paquete dplyr
# install.packages("dplyr")
library(dplyr)

# Uso de la funci?n glimpse
telco %>% glimpse()
cartera %>% glimpse()

# --- 

#######

## Resumen general de las variables del objeto telco
summary(telco)
summary(cartera)

## Gr?fico de la variable bill
plot(telco$bill)
plot(telco$bill, col="blue", type="p", xlab="cliente", ylab="pago factura", cex=.5, main="Gr?fico")

plot(telco$score)

#Gr?fico usando ggplot
#B?sico
ggplot(telco, aes(x=seq_along(bill), y=bill))+
  geom_point()

#Con formato
ggplot(telco, aes(x=seq_along(bill), y=bill))+
  ggtitle("Gr?fico")+
  labs(x="cliente", y="pago factura")+
  geom_point(size=1, col="red")+
  theme_minimal()


## Hacer que R entienda el nombre de las variables de un data frame
## como si fueran objetos separados
attach(telco)

####### Medidas de Tendencia Central

## C?lculo del promedio de la variable bill que pertenece al data frame
## telco
mean(bill)

## C?lculo de la mediana de la variable bill que pertenece al data frame
## telco
median(bill)

## C?lculo del promedio recortado de la variable bill que pertenece 
## al data frame telco  
mean(bill, trim=0.2)

####### Medidas de dispersi?n

## C?lculo del rango 
range(bill)
range(bill)[2]-range(bill)[1]
## C?lculo de la varianza 
var(bill)
## C?lculo de la desviaci?n est?ndar 
sd(bill)
## C?lculo del rango intercuart?lico
IQR(bill)
## C?lculo del MAD
mad(bill)


## Coeficiente de variaci?n espec?fico para la variable bill
cv=(100*sd(bill))/mean(bill)
cv

## Funci?n del coeficiente de variaci?n
CV <- function(var){(sd(var)/mean(var))*100}
CV(bill)
CV(minutes)

## Realiza el c?lculo del CV para cada columna del data frame telco
apply(telco,2,CV)

### Carga del paquete psych
## Uso de la funci?n describe que calcula m?ltiples descriptivos
describe(telco)

## Revisi?n del objeto que crea la funci?n describe
telcostat <- describe(telco)
str(telcostat)

### Adici?n del CV a los descriptivos previos
cvs <- apply(telco,2,CV)
telcostat2 <- cbind(telcostat,cvs)

## C?lculo de la media geom?trica de los CV 
geometric.mean(telcostat2$cvs)


########

### Revisi?n de los cuantiles de la variable bill
quantile(bill)
quantile(bill, c(.10,.45,.90))

## C?lculo de los cuantiles para las columnas del data frame telco
cuantiles<-apply(telco,2,quantile)
cuantiles

## Descriptivos agrupados por una variable factor
carterastat <- describeBy(cartera, cartera$retrasos)
carterastat


####### Gr?ficos univariados

### Histograma
hist(bill)
hist(bill, breaks=15, col="red", border = FALSE, main="Histograma de facturaci?n", 
     xlab="Facturaci?n", ylab="Frecuencia")

hist(minutes)
hist(score)

## Usando ggplot
# B?sico
ggplot(telco, aes(x=bill))+
  geom_histogram()

# Con formato 
ggplot(telco, aes(x=bill))+
  geom_histogram(bins = sqrt(length(bill)), color="black")+
  ggtitle("Histograma de facturaci?n")+
  labs(x="Facturaci?n", y="Densidad")+
  theme_minimal()

#Agregando la curva de densidad de los datos

ggplot(telco, aes(x=bill, y=..density..))+
  geom_histogram(bins = sqrt(length(bill)), color="black")+
  geom_density(aes(x=bill, y=..density..), color="red", size=1)+
  ggtitle("Histograma de facturaci?n con densidad")+
  labs(x="Facturaci?n", y="Densidad")+
  theme_minimal()

## Comparando con la distribuci?n normal

ggplot(telco, aes(x=bill, y=..density..))+
  geom_histogram(bins = sqrt(length(bill)), color="black")+
  geom_line(aes(x=bill, y=dnorm(bill, mean(bill), sd(bill))), color="red", size=1)+
  ggtitle("Histograma de facturaci?n con curva normal")+
  labs(x="Facturaci?n", y="Densidad")+
  theme_minimal()

### Boxplot
boxplot(bill)
boxplot(bill, horizontal = TRUE, col="green")

boxplot(minutes)

## Usando ggplot

# B?sico
ggplot(telco, aes(bill))+
  geom_boxplot()

# Con formato

#TAREA: pintar el boxplot con formato en ggplot.


### QQ norm
qqnorm(minutes)
qqnorm(bill)

#TAREA: pintar el qqplot usando ggplot.

## Revisi?n de la estructura de los datos de gr?ficos
boxguarda<-boxplot(bill)
str(boxguarda)

## Extracci?n de los datos at?picos
boxguarda$out
length(boxguarda$out)

boxguarda$stats

### Descripci?n de las variables categ?ricas
table(cartera$tipoips)

### Gr?fica de la tabla de contingencia
tabla1 <- table(cartera$tipoips)
barplot(tabla1, main="Cantidad de IPS", xlab="Tipo de IPS")
barplot(tabla1, main="Cantidad de IPS", ylab="Tipo de IPS", xlab="Frecuencia", col="brown", horiz=TRUE)

## Gr?fico con ggplot

ggplot(cartera, aes(tipoips))+ 
  geom_bar()+
  ggtitle("Diagrama de barras tipo de IPS")+
  labs(x="Tipo de IPS", y="")+
  theme_minimal()


### C?lculo de proporci?n de las frecuencias de la tabla 
prop.table(tabla1)
round(prop.table(tabla1),4)


####### Cruces b?sicos de variables

plot(minutes, bill)
plot(minutes, bill, col="purple", cex=.5)

## Diagrama de dispersi?n con ggplot

ggplot(telco, aes(x=minutes, y=bill))+
  geom_point(size=1)+
  ggtitle("Diagrama de dispersi?n minutos vs facturaci?n")+
  labs(x="minutos", y="facturaci?n")+
  theme_minimal()


### boxplot dividido por una variable categ?rica
boxplot(pasivo_sobre_venta~retrasos, data=cartera)


## Con ggplot
ggplot(cartera, aes(x=retrasos, y=MESES))+
  geom_boxplot()+
  ggtitle("Boxplot de meses segmentado por retrasos")+
  labs(x="Retrasos", y="Meses")+
  theme_minimal()

# Boxplots agrupados
ggplot(cartera, aes(x=tipoips, y=MESES, fill=retrasos))+
  geom_boxplot()+
  ggtitle("Boxplot de meses segmentado por retrasos")+
  labs(x="Tipo IPS", y="Meses")+
  theme_minimal()

###### Cruce de variables categ?ricas
table(cartera$tipoips, cartera$retrasos)
table2 <- table(cartera$tipoips, cartera$retrasos)
prop.table(table2) #Proporci?n con respecto al total de datos cruzados
prop.table(table2, margin = 1) #Proporci?n por filas
prop.table(table2, margin = 2) #Proporci?n por columnas

## Diagramas de barras para tablas de contingencia
# IPS Con retraso
ggplot(cartera, aes(tipoips, fill=retrasos))+ 
  geom_bar()+
  ggtitle("Diagrama de barras tipo de IPS")+
  labs(x="Tipo de IPS", y="")+
  theme_minimal()

# Comparativo
ggplot(cartera, aes(tipoips, fill=retrasos))+ 
  geom_bar(position = "fill")+
  ggtitle("Diagrama de barras tipo de IPS con retrasos")+
  labs(x="Tipo de IPS", y="")+
  theme_minimal()

## Dejar de usar las variables del data frame telco
detach(telco)

## Usar los nombres de las variables del data frame cartera como objetos
attach(cartera)

## Carga de la librer?a gmodels
install.packages("gmodels")
library("gmodels")

### Una mejor creaci?n de tablas cruzadas
CrossTable(tipoips, retrasos)

######
install.packages("vcd")
library("vcd")

## Gr?fico de mosaico para tablas cruzadas
mosaic(~tipoips + retrasos,data=cartera, 
       legend=TRUE, shade=TRUE)


#Ejemplo adicional de mosaico
library(MASS)
data("Titanic")
mosaic(Titanic,shade=TRUE)

## Formula interface for tabulated data plus shading and legend:
mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)
