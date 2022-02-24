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

#Descripción de datos
head(customers, 10)
str(customers)

##Cambio en el tipo de dato de las variables
customers$CLIENTE <- as.character(customers$CLIENTE)
customers$grupo_de_cliente <- as.factor(customers$grupo_de_cliente)
customers$Numero_de_transacciones <- as.integer(customers$Numero_de_transacciones)
str(customers)

#Resumen general de las variables
summary(customers)
describe(customers)

sintotal <- customers%>%
  select(CLIENTE, Numero_de_transacciones, promedio_por_transaccion, transaccion_minima, transaccion_maxima,
         desviacion_estandar_por_transaccion, porcentaje_visa_nacional, porcentaje_visa_internacional,
         porcentaje_mastercard_nacional, porcentaje_mastercard_internacional, Porcentaje_otrafranquicia_nacional,
         porcentaje_otrafranquicia_internacional, porcentaje_manana, porcentaje_tarde, porcentaje_noche, )

##Otra opcion de seleccionar
cust3 <- customers_sin_totales %>%
  select(cliente)
cust4 <- customers_sin_totales[, 3:24]
customers_sin_grupo <- as.data.frame(cbind(cust3, cust4))
