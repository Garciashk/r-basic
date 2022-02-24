if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('e1071')) install.packages('e1071')
if (!require('cluster')) install.packages('cluster')
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('ggpubr')) install.packages('ggpubr')
if (!require('GGally')) install.packages('GGally')

#Directorio de trabajo y lectura de los datos
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
customers <- read_excel("infoclientebanca.xlsx")

#Cambiar nombre a algunas columnas
require(reshape)
customers = rename(customers, c(CLIENTE = "cliente", Numero_de_transacciones = "numero_de_transacciones",
                                Porcentaje_otrafranquicia_nacional = "porcentaje_otra_franquicia_nacional", 
                                porcentaje_otrafranquicia_internacional = "porcentaje_otra_franquicia_internacional", 
                                porcDOMINGO  = "porcentaje_domingo", porcLUNES = "porcentaje_lunes",  
                                porcMARTES = "porcentaje_martes", porcMIERCOLES = "porcentaje_miercoles",  
                                porcJUEVES = "porcentaje_jueves", porcVIERNES = "porcentaje_viernes", 
                                porcSABADO = "porcentaje_sabado", Sitio_consumo_masfrecuente = "sitio_consumo_mas_frecuente"))

#Descripción de datos y preparación de los datos
head(customers, 5)
str(customers)

##Cambio en el tipo de dato de las variables
customers$cliente <- as.character(customers$cliente)
customers$grupo_de_cliente <- as.factor(customers$grupo_de_cliente)
customers$numero_de_transacciones <- as.integer(customers$numero_de_transacciones)
customers$sitio_consumo_mas_frecuente <- as.factor(customers$sitio_consumo_mas_frecuente)

#Seleccionando variables del conjunto de datos
cust1 <- customers[, 1:4]
cust2 <- customers[, 8]
cust3 <- customers[, 10]
cust4 <- customers[,19]
cust5 <- customers[,24:26]
customers_filter <- as.data.frame(cbind(cust1, cust2, cust3, cust4, cust5))

#Resumen general
describe(customers_filter[, 3:9])

#Primera vista a histogramas con ggplot
histo1 <- customers_filter %>% ggplot(aes(x = numero_de_transacciones)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \nNúmero de transacciones') +
  labs(y = "", x = "Número de transacciones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo2 <- customers_filter %>% ggplot(aes(x = promedio_por_transaccion)) +
  geom_histogram(fill = "black") +
  scale_x_continuous(limits = c(1, 1500000)) +
  ggtitle('Histograma \npromedio_por_transaccion') +
  labs(y = "", x = "Promedio por transaccion") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo3 <- customers_filter %>% ggplot(aes(x = porcentaje_visa_nacional)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \nporcentaje_visa_nacional') +
  labs(y = "", x = "Porcentaje Visa nacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo4 <- customers_filter %>% ggplot(aes(x = porcentaje_mastercard_nacional)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \nporcentaje_mastercard_nacional') +
  labs(y = "", x = "Porcentaje MasterCard nacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo5 <- customers_filter %>% ggplot(aes(x = porcentaje_domingo)) +
  geom_histogram(fill = "black") +
   ggtitle('Histograma \nporcentaje_domingo') +
  labs(y = "", x = "Porcentaje domingo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo6 <- customers_filter %>% ggplot(aes(x = porcentaje_viernes)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \nporcentaje_viernes') +
  labs(y = "", x = "Porcentaje viernes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo7 <- customers_filter %>% ggplot(aes(x = porcentaje_sabado)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \nporcentaje_sabado') +
  labs(y = "", x = "Porcentaje sabado") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(histo1, histo2, histo3, histo4, histo5, histo6, histo7, nrow = 2, ncol = 4)

#Asignar un valor a los porcentajes de acuerdo al promedio por transacción
customers_target <- customers_filter %>%
  transmute(cliente, grupo_de_cliente, numero_de_transacciones, promedio_por_transaccion,
            promedio_visa_nacional = porcentaje_visa_nacional * promedio_por_transaccion,
            promedio_mastercard_nacional = porcentaje_mastercard_nacional * promedio_por_transaccion,
            promedio_domingo = porcentaje_domingo * promedio_por_transaccion,
            promedio_viernes = porcentaje_viernes * promedio_por_transaccion,
            promedio_sabado = porcentaje_sabado * promedio_por_transaccion, sitio_consumo_mas_frecuente)

#Descartamos los consumos iguales a 0 en nuestros dias de interés 
#Descartamos la variable promedio por transacción ya que se encuentra distribuida en los porcentajes de consumo
cust_targ <- customers_target %>%
  mutate(target = promedio_domingo + promedio_viernes + promedio_sabado,) %>%
  filter(target > 0) %>%
  select(cliente, grupo_de_cliente, numero_de_transacciones, promedio_visa_nacional, promedio_mastercard_nacional,
         promedio_domingo, promedio_viernes, promedio_sabado, sitio_consumo_mas_frecuente)

## Seleccionamos las variables numéricas
customersnumeric <- sapply(cust_targ, is.numeric)
custnum<- cust_targ[ , customersnumeric]


## Ahora los factores
customersfactors <- sapply(cust_targ, is.factor)
custfact<- cust_targ[ , customersfactors]

library(psych)
# Cálculo de CV's
CV <- function(var){(sd(var)/mean(var))*100}
cvs <- apply(custnum,2, CV)

#Resumen general
numdescriptive <- psych::describe(custnum)
numdescriptive <- cbind(numdescriptive,cvs)
numdescriptive

#Aplicación de reducción de atípicos (logaritmo natural de (1+x) para tener en cuenta los ceros)
custnum1 <-apply(custnum,2,log1p)
custnum_final <- as.data.frame(cbind(custnum1))

#estandarizar
scaled_custnum_final <- as.data.frame(scale(custnum_final))
describe(scaled_custnum_final)

#Segunda vista a histogramas con ggplot
hist1 <- scaled_custnum_final %>% ggplot(aes(x = numero_de_transacciones)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \nNúmero de transacciones') +
  labs(y = "", x = "Número de transacciones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


hist2 <- scaled_custnum_final %>% ggplot(aes(x = promedio_visa_nacional)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \npromedio_visa_nacional') +
  labs(y = "", x = "Promedio Visa nacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist3 <- scaled_custnum_final %>% ggplot(aes(x = promedio_mastercard_nacional)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \npromedio_mastercard_nacional') +
  labs(y = "", x = "Promedio MasterCard nacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist4 <- scaled_custnum_final %>% ggplot(aes(x = promedio_domingo)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \npromedio_domingo') +
  labs(y = "", x = "Promedio domingo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist5 <- scaled_custnum_final %>% ggplot(aes(x = promedio_viernes)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \npromedio_viernes') +
  labs(y = "", x = "Promedio viernes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist6 <- scaled_custnum_final %>% ggplot(aes(x = promedio_sabado)) +
  geom_histogram(fill = "black") +
  ggtitle('Histograma \npromedio_sabado') +
  labs(y = "", x = "Promedio sabado") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist1, hist2, hist3, hist4, hist5, hist6, nrow = 2, ncol = 3)

#Using elbow (Scree) plot

#fijar la semilla
set.seed(20)
#cálculo la suma de cuadrados total
wss <- (nrow(scaled_custnum_final)-1)*sum(apply(scaled_custnum_final,2,var))
#cálculo para cada soluci?n de clustering 
for (i in 1:10) wss[i] <- sum(kmeans(scaled_custnum_final,
                                     centers=i, nstart=20)$withinss)
# Gráfico de codo
# Con ggplot
sumas <- as.data.frame(cbind(wss, k = seq(1,10, by=1)))

sumas %>% ggplot(aes(x=k, y=wss)) +
  geom_point(color = "black") + 
  geom_line(color = "black") +
  scale_x_continuous(breaks = 1 : 10) +
  labs(x = "Número de clústeres", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Ejecuci?n
k <- 4
#ejecuci?n de k-means
set.seed(20)
cellcluster<-kmeans(scaled_custnum_final,centers=k,nstart=10,iter.max=20)
#tama?o de grupos
cellcluster$size
#numero de iteraciones
cellcluster$iter
#centros de grupos
cellcluster$centers

#Gr?fico de calor de centros
centrosg <- as.data.frame(cellcluster$centers)
centrosg$grupo <- as.factor(rownames(centrosg))
centrosheat <- reshape2::melt(centrosg)
colnames(centrosheat) <- c("grupo","variable","centroide")
centrosheat %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

#validar resultados- consistencia
kclusters <- clusterboot(scaled_custnum_final,B=10,clustermethod=kmeansCBI,k=k,seed=5)
#la validaci?n del resultado. >0.75 o .85 muy bueno; <.6 malo
kclusters$bootmean
