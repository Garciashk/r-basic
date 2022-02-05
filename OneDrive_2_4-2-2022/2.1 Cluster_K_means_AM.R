if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('cluster')) install.packages('cluster')
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('ggpubr')) install.packages('ggpubr')
if (!require('GGally')) install.packages('GGally')

# Lectura de los datos que deben estar en la ruta que genera el siguiente código
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cell <- read_excel("cellsegmentation.xlsx")
cell %>% glimpse()

#-------------------------------------------------------------------------------------
# Análisis descriptivo
#-------------------------------------------------------------------------------------

## Revisión de las variables escalares
describe(cell[,6:10])

# Cálculo de CV's
CV <- function(var){(sd(var)/mean(var))*100}
apply(cell[,6:10],2, CV)

#boxplot para ver la posible presencia de atípicos

boxplot(cell[,6:10], 
        names=c("Minutos \npreferido", "Adicionales", "Valor equipo", 
                "Minutos \nno preferido", "Internet"), las=2, cex.axis=0.7, cex=0.5,
        main="Boxplots variables comportamentales")

# para graficar varios plot en una misma ventana
par(mfrow=c(2,3))
hist(cell$minutos_preferido, xlab="Minutos preferido", 
     ylab="Frecuencia", main="Histograma \nminutos preferido", ylim = c(0,800))
hist(cell$adicionales, xlab = "Adicionales", ylab="Frecuencia", 
     main="Histograma \nconsumos adicionales", ylim = c(0,800))
hist(cell$equipo_dolares, xlab = "Valor equipo mensual (dólares)", ylab="Frecuencia", 
     main="Histograma \nvalor mensual del equipo", ylim = c(0,800))
hist(cell$minutos_no_preferido, xlab = "Minutos no preferido", ylab="Frecuencia", 
     main="Histograma \nminutos no preferido", ylim = c(0,800))
hist(cell$internet_gigas, xlab = "Consumo internet (gigas)", ylab="Frecuencia", 
     main="Histograma \nconsumo de internet", ylim = c(0,800))
# parar el attach de las gráficas
dev.off()

# Con ggplot
# Creamos los histogramas
hist1 <- cell %>% ggplot(aes(x=minutos_preferido)) +
  geom_histogram() +
  ggtitle('Histograma \nminutos preferido') +
  labs(y = "", x = "Minutos preferido") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist2 <- cell %>% ggplot(aes(x=adicionales)) +
  geom_histogram() +
  ggtitle('Histograma \nconsumos adicionales') +
  labs(y = "", x = "Adicionales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist3 <- cell %>% ggplot(aes(x=equipo_dolares)) +
  geom_histogram() +
  ggtitle('Histograma \nvalor mensual del equipo') +
  labs(y = "", x = "Valor equipo mensual (dólares)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist4 <- cell %>% ggplot(aes(x=minutos_no_preferido)) +
  geom_histogram() +
  ggtitle('Histograma \nminutos no preferido') +
  labs(y = "", x = "Minutos no preferido") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist5 <- cell %>% ggplot(aes(x=internet_gigas)) +
  geom_histogram() +
  ggtitle('Histograma \nconsumo de internet') +
  labs(y = "", x = "Consumo internet (gigas)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist1, hist2, hist3, hist4, hist5, nrow = 2, ncol = 3)


#-------------------------------------------------------------------------------------
# Preparación de datos
#-------------------------------------------------------------------------------------

#variable de servicios adicionales (empleando dplyr)
cell <- cell %>% 
  mutate(servicios=fijo+largadistancia+internetcasa+numoculto)

#variable de servicios adicionales (empleando base)
#cell$servicios<-cell$fijo+cell$largadistancia+cell$internetcasa+cell$numoculto

#reducción de atípicos
##obtener las variables a reducir
cell3 <- as.data.frame(cell[,c(6:7,9:10)])

#selección de variables usando dplyr
# cell3 <- cell %>% select(c(6:7,9:10))

#obtener el logaritmo de 1+x
cell4 <- apply(cell3,2,log1p)

#boxplot datos transformados
boxplot(cell4,
        names=c("Minutos \npreferido", "Adicionales", 
                "Minutos \nno preferido", "Internet"), las=2, cex.axis=0.7, cex=0.5,
        main="Boxplots variables transformadas")

# Con ggplot
# TAREA

#ensamble final
cellfin <- as.data.frame(cbind(cell4,cell$servicios, cell$equipo_dolares))
colnames(cellfin)[5:6] <- c("servicios","equipo_dolares")
head(cellfin)

#estandarizar
cellfins <- as.data.frame(scale(cellfin))

boxplot(cellfins,
        names=c("Minutos \npreferido", "Adicionales", "Minutos \nno preferido", 
                "Internet", "Servicios", "Valor equipo"), las=2, cex.axis=0.7, cex=0.5,
        main="Boxplots variables transformadas\ny escaladas")

#revisar descriptivos
describe(cellfins)

#y los histogramas

histo1 <- cellfins %>% ggplot(aes(x=minutos_preferido)) +
  geom_histogram() +
  ggtitle('Histograma \nminutos preferido') +
  labs(y = "", x = "Minutos preferido") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo2 <- cellfins %>% ggplot(aes(x=adicionales)) +
  geom_histogram() +
  ggtitle('Histograma \nconsumos adicionales') +
  labs(y = "", x = "Adicionales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo3 <- cellfins %>% ggplot(aes(x=minutos_no_preferido)) +
  geom_histogram() +
  ggtitle('Histograma \nminutos no preferido') +
  labs(y = "", x = "Minutos no preferido") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(histo1, histo2, histo3, nrow = 1, ncol = 3)

# los histogramas con base

# par(mfrow=c(1,3))
# hist(cellfins$minutos_preferido, xlab="Minutos preferido", 
#      ylab="Frecuencia", main="Histograma \nminutos preferido", ylim = c(0,200))
# hist(cellfins$adicionales, xlab = "Adicionales", ylab="Frecuencia", 
#      main="Histograma \nconsumos adicionales", ylim = c(0,600))
# hist(cellfins$minutos_no_preferido, xlab = "Minutos no preferido", ylab="Frecuencia", 
#      main="Histograma \nminutos no preferido", ylim = c(0,400))
# 
# dev.off()


#-------------------------------------------------------------------------------------
# Modelación
#-------------------------------------------------------------------------------------

### Escoger el número de clústeres

#utilizo una semilla para replicar resultados
set.seed(5935)
#cálculo la suma de cuadrados total
wss <- (nrow(cellfins)-1)*sum(apply(cellfins,2,var))
#cálculo para cada solución de clustering 
for (i in 2:15) wss[i] <- sum(kmeans(cellfins,
                                     centers=i, nstart=10)$withinss)
# Gráfico de codo
# Con ggplot
sumas <- as.data.frame(cbind(wss, k = seq(1,15, by=1)))

sumas %>% ggplot(aes(x=k, y=wss)) +
  geom_point() + 
  geom_line() +
  labs(x = "Número de clústeres", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Con base
plot(1:15, wss, type="b", xlab="Número de Clústeres",
     ylab="Suma de cuadrados within")

# Otra forma de hacer el gráfico de codo

if (!require('factoextra')) install.packages('factoextra')
if (!require('NbClust')) install.packages('NbClust')

# Elbow method
fviz_nbclust(cellfins, kmeans, method = "wss", k.max = 15) +
  # geom_vline(xintercept = 4, linetype = 2)+
  labs(title = "Método de codo")+
  xlab("Número de clústeres (k)")+
  ylab("Suma total de cuadrados within")

## otras formas de ver el número de clústeres

# library("cluster")
# library("fpc")

# Evaluar usando el criterio ASW (average sillouethe width)
set.seed(2) #Para evitar aleatoriedad en los resultados
clustering.asw <- kmeansruns(cellfins,
                             krange=2:15,criterion="asw",
                             iter.max=100,runs= 100,critout=TRUE)
clustering.asw$bestk

#Visualmente

set.seed(2)
fviz_nbclust(cellfins, kmeans, method = "silhouette")+
  labs(title = "Método de la silueta")+
  xlab("Número de clusters (k)")+
  ylab("Altura promedio de la silueta (asw)")

### La estadística GAP

#Evaluar con gap statistic
#mira el mínimo k tal que el gap sea mayor que el gap de k+1 restado de su desviación
set.seed(21)
gscar<-clusGap(cellfins,FUN=kmeans,K.max=15,B=60, iter.max=20)
gscar

### Ejecución
#ejecución de k-means
set.seed(45390)
cellcluster<-kmeans(cellfins,centers=5,nstart=10,iter.max=20)
#tamaño de grupos
cellcluster$size
#numero de iteraciones
cellcluster$iter
#centros de grupos
cellcluster$centers

#Gráfico de calor de centros
centrosg <- as.data.frame(cellcluster$centers)
centrosg$grupo <- as.factor(rownames(centrosg))
centrosheat <- reshape2::melt(centrosg)
colnames(centrosheat) <- c("grupo","variable","centroide")
centrosheat %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

# Gráfico de perfiles de centros
grupos <- c(1,2,3,4,5)

centros <- as.data.frame(cbind(grupos, cellcluster$centers))
centros$grupos <- as.factor(centros$grupos)
colnames(centros) <- c("Grupo", "Preferido", "Adicionales", "No preferido", "Internet", 
                       "Servicios", "Equipo")

ggparcoord(centros, columns = 2:7, groupColumn = 1, scale = "globalminmax")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Gráfico de centroides")+
  labs(y="")

#------------------------------------------------------------------------------------
# Perfilamiento
#------------------------------------------------------------------------------------

#guardar el cluster de pertenencia
cell$grupo <- cellcluster$cluster

mosaic(~grupo + jubilado ,data=cell, 
       legend=TRUE, shade=TRUE)
mosaic(~grupo + genero ,data=cell, 
       legend=TRUE, shade=TRUE)
boxplot(log(ingreso_miles)~grupo, data=cell)
boxplot(Edad~grupo, data=cell)

#Descriptivos segmentados
cell %>% 
  group_by(grupo) %>% 
  summarise(ingreso_medio=mean(ingreso_miles),
            desv_ing = sd(ingreso_miles),
            edad_media = mean(Edad),
            desv_edad = sd(Edad))

#Gráfico en componentes principales
clusplot(cellfins,cellcluster$cluster,color=TRUE,col.p=cellcluster$cluster, cex=0.5)

#validar resultados- consistencia
kclusters <- clusterboot(cellfins,B=10,clustermethod=kmeansCBI,k=5,seed=5)
#la validación del resultado. >0.75 o .85 muy bueno; <.6 malo
kclusters$bootmean

