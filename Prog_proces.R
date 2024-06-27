#########################################################
## ARTICULO DE INVESTIGACION - MODULO 8 MINERIA DE DATOS
## MARISOL PAREDES
#########################################################

rm(list=ls())
library(shiny)
library(ExPanDaR)#dashboard shiny
library(GGally)# ggplot
library(ggridges)
library(ggplot2)
library(GGally)
library(tidyverse)
library(plotly)
library(visdat)
library(dplyr)
#Recuperando los datos meterologicos del INE en SPSS
library(haven)
library(labelled)
meteo <-read_sav("datos/Base_Datos_METEOROLOGIA.sav")
save(meteo,file = "datos/METEO.RData")
meteo
##Explorando los datos de forma inicial
ExPanD(meteo)

#Estadisticos descriptivos de los datos
summary(meteo)

#Grafica para ver datos faltantes
vis_dat(meteo)
#Tambien podemos analizar la proporcion de faltantes sobre el total de datos
round(sum(is.na(meteo$PRECIP_PLUVIAL))/nrow(meteo)*100,2)
round(sum(is.na(meteo$DIAS_PRECIP))/nrow(meteo)*100,2)
round(sum(is.na(meteo$HUM_RELATIVA))/nrow(meteo)*100,2)
round(sum(is.na(meteo$PRESION_ATMOSF))/nrow(meteo)*100,2)

#Grafica de cajas de algunas variables de interes
boxplot(meteo$PRECIP_PLUVIAL)
boxplot(meteo$DIAS_PRECIP)
boxplot(meteo$HUM_RELATIVA)
ggplot(meteo, aes(x = reorder(ESTACION, PRECIP_PLUVIAL), y = PRECIP_PLUVIAL)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Estacion metereologica", y = "Precipitacion pluvial")

ggplot(meteo, aes(x = reorder(REGION, PRECIP_PLUVIAL), y = PRECIP_PLUVIAL)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Región", y = "Precipitacion pluvial")

##Graficas con algunas variables de interes
ggplot(meteo, aes(x = PRECIP_PLUVIAL, y = REGION, fill=REGION)) + 
  geom_density_ridges()
str(meteo)

ggplot(meteo, aes(x = DIAS_PRECIP, y = REGION, fill=REGION)) + 
  geom_density_ridges()
ggplot(meteo, aes(x = HUM_RELATIVA, y = REGION, fill=REGION)) + 
  geom_density_ridges()

###########SE INTENTO HACER IMPUTACIONES, PERO NO ERA ADECUADO A LOS DATOS
#VALORES FALTANTES:REGISTROS COMPLETOS
#Si quisieramos trabajar unicamente con aquellos registros con datos completos
#eliminamos los NA
meteo.reg_completos <- na.omit(meteo)
#Podemos ver las filas que quedaron
nrow(meteo.reg_completos)
#Tambien se puede calcular la media aritmetica omitiendo los valores faltantes
print(mean(meteo$PRECIP_PLUVIAL, na.rm=TRUE))

#VALORES FALTANTES: IMPUTACION POR HOT DECK
#instalamos la libreria que hace imputacion Hot Deck
install.packages("VIM")
library(VIM)

#Definimos un dataframe auxiliar para no perder la variable original
df_aux <- hotdeck(meteo, variable="PRECIP_PLUVIAL")
#Se realiza la asignacion a los faltantes por Hotdeck
meteo.imp <- meteo
meteo.imp$hotdeck <- df_aux$PRECIP_PLUVIAL
meteo.imp$hotdeckbool <- df_aux$PRECIP_PLUVIAL
#Ahora verificamos que no existen faltantes
sum(is.na(meteo.imp$hotdeck))
summary(meteo.imp$hotdeck)

########ANALISIS INICIAL: COMPONENTES PRINCIPALES

##Reconstruyendo una Base de datos para Componentes Principales
##Eliminando las variables con mayor cantidad de NA`S`
#Opcion1- Sin considerar Dias con precipitacion
meteo1<-meteo %>% select(-DIAS_PRECIP, -PRECIP_MAXIMA24Hrs, 
                         -HUM_RELATIVA, -DIREC_VIENTOS,-DIR_VEL_MAXIMA_VIENTO, -DIAS_HELADA, -PRESION_ATMOSF)
#Opcion1- Considerando Dias con precipitacion
meteo2<-meteo %>% select(-PRECIP_MAXIMA24Hrs, -HUM_RELATIVA, 
                         -DIREC_VIENTOS,-DIR_VEL_MAXIMA_VIENTO, -DIAS_HELADA, -PRESION_ATMOSF)

#Estadisticos descriptivos de los datos
summary(meteo1)
summary(meteo2)
#Grafica para ver datos faltantes
vis_dat(meteo1)
vis_dat(meteo2)
#VALORES FALTANTES:REGISTROS COMPLETOS
#Si quisieramos trabajar unicamente con aquellos registros con datos completos
#eliminamos los NA
meteo1_comp <- na.omit(meteo1)
meteo2_comp <- na.omit(meteo2)
## Visualizacion de que ya no hay datos faltantes
vis_dat(meteo1_comp)
vis_dat(meteo2_comp)
###############################PARA COMPONENTES PRINCIPALES
library(corrplot)
library(gplots)
library(RColorBrewer)
library(FactoMineR,factoextra)
# Tranformacion - Estandarizacion de datos 
meteo1.sc <- meteo1_comp
meteo1.sc[,7:12] <- data.frame(scale(meteo1.sc[,7:12]))
summary(meteo1.sc)

meteo2.sc <- meteo2_comp
meteo2.sc[,7:13] <- data.frame(scale(meteo2.sc[,7:13]))
summary(meteo2.sc)
#Correlacion
library(corrplot)
corrplot(cor(meteo1.sc[, 7:12]), order="hclust")

corrplot(cor(meteo2.sc[, 7:13]), order="hclust")
# COMPONENTES PRINCIPALES
meteo1.cp <- prcomp(meteo1.sc[, 7:12])
summary(meteo1.cp)
plot(meteo1.cp, type="l")
biplot(meteo1.cp)

meteo2.cp <- prcomp(meteo2.sc[, 7:13])
summary(meteo2.cp)
plot(meteo2.cp, type="l")
biplot(meteo2.cp)

#########################################################################
library("FactoMineR")
library("factoextra")

# ACP con PCA de FactoMineR 
meteo1.pca <- PCA(meteo1.sc[, 7:12], graph=FALSE)
print(meteo1.pca) # contiene variada informacion

meteo2.pca <- PCA(meteo2.sc[, 7:13], graph=FALSE)
print(meteo1.pca) # contiene variada informacion
# por ejemplo
# para extraer autovalores y varianzas:
get_eigenvalue(meteo1.pca)
# para visualizar autovalores
fviz_eig(meteo1.pca)
# para visualizar resultados individuales
fviz_pca_ind(meteo1.pca)
# para visualizar resultados por variables
fviz_pca_var(meteo1.pca)

fviz_pca_var(meteo2.pca)
# para hacer un biplot de ind y var
fviz_pca_biplot(meteo1.pca)

# Autovalores y varianza con factoextra 
eig.val <- get_eigenvalue(meteo1.pca)
eig.val

# Grafico de sedimentacion
fviz_eig(meteo1.pca, addlabels = TRUE, ylim=c(0, 50))
fviz_eig(meteo2.pca, addlabels = TRUE, ylim=c(0, 75))
# Grafico de variables Resultados
var <- get_pca_var(meteo1.pca)
var
# para visualizar coordenadas
var$coord

# Para visualizar las variables:
fviz_pca_var(meteo1.pca, col.var = "red")

# Para ver la calidad de la representacion de la var
corrplot(var$cos2, is.corr=FALSE)

# Se puede ver la calidad de las var a traves de barras
fviz_cos2(meteo1.pca, choice = "var", axes = 1:2)

# Ver por colores
fviz_pca_var(meteo1.pca, col.var ="cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_var(meteo2.pca, col.var ="cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# GRAFICO CON INDIVIDUOS
ind <- get_pca_ind(meteo1.pca)
ind

# Ej. para ver coord de ind
head(ind$coord)

#Convertimos a factor la variable LLUVIA
meteo1.pca$LLUVIA <- factor(meteo1.pca$LLUVIA,levels=0:1,labels=c("No LLueve","Llueve") )

#Mostrando la contribución de todas las variables, pero solo
#con una muestra de los datos
fviz_pca_biplot(meteo2.pca,
                geom.ind = "point", 
                col.ind = meteo2_comp$REGION,
                select.ind = list(contrib = 500),
                addEllipses = TRUE,
                gradient.cols = "npg") + 
  scale_shape_manual(values = rep(16:27, len = 12))

###########################################
## REGRESION LOGISTICA
###########################################

##Reconstruyendo una Base de datos para Regresion Logistica
##Eliminando las variables con mayor cantidad de NA`S`
meteo_log<-meteo %>% select(-PRECIP_MAXIMA24Hrs, -HUM_RELATIVA, 
                         -DIREC_VIENTOS,-DIR_VEL_MAXIMA_VIENTO, -DIAS_HELADA, - PRESION_ATMOSF)

#Estadisticos descriptivos de los datos
summary(meteo_log)
#Grafica para ver datos faltantes
vis_dat(meteo_log)
#VALORES FALTANTES:REGISTROS COMPLETOS
#Si quisieramos trabajar unicamente con aquellos registros con datos completos
#eliminamos los NA
meteo_log1 <- na.omit(meteo_log)
## Visualizacion de que ya no hay datos faltantes
vis_dat(meteo_log1)
#Convertimos a factor la variable LLUVIA
meteo_log1$LLUVIA <- factor(meteo_log1$LLUVIA,levels=0:1,labels=c("No LLueve","Llueve") )

## Bases: trainbd, testbd
set.seed(123)
index = sample(1:2, nrow(meteo_log1), replace = TRUE, prob=c(0.7, 0.3))
prop.table(table(index))
trainbd<-meteo_log1 %>% filter(index==1)
testbd<-meteo_log1 %>% filter(index==2)

#Especificar el modelo logit (Con todas las variables-no resulta util)
m1<-glm(LLUVIA~.,data=trainbd,family = binomial(link="logit"))

#Identificar las variables significativas
summary(m1)
# Las elegimos por el conocimiento previo de los datos
glm1 <- glm(LLUVIA ~ REGION+PRECIP_PLUVIAL+TEMP_MEDIA+TEMP_MAX+MAX_EXTREM+TEMP_MIN+MIN_EXTREM,
            family = binomial(link = "logit"),data = trainbd)
summary(glm1)

# Quitando algunas que no son significativas
glm2 <- glm(LLUVIA ~ REGION+PRECIP_PLUVIAL+TEMP_MIN+MIN_EXTREM,
            family = binomial(link = "logit"),data = trainbd)
summary(glm2)
#Quitando la principal que es Precipitacion pluvial
glm3 <- glm(LLUVIA ~ REGION+TEMP_MIN+MIN_EXTREM,
            family = binomial(link = "logit"),data = trainbd)
summary(glm3)

# Predecir la clase de pertenencia en la base de test ($prob>0.5$)
summary(testbd)
clase<-predict(glm1,testbd,type="response")>0.5

clase2<-predict(glm2,testbd,type="response")>0.5
clase3<-predict(glm3,testbd,type="response")>0.5
# Observar la clasificación dada en base a la probabilidad fijada
table(clase)
prop.table(table(clase))
table(clase2)
prop.table(table(clase2))
table(clase3)
prop.table(table(clase3))
#Convertimos a LOGICO la variable LLUVIA
pru<- testbd
levels(pru$LLUVIA) <- c(FALSE,TRUE)
pru$LLUVIA <- as.logical(pru$LLUVIA)

str(pru$LLUVIA)
# Comparar lo observado y lo predicho (testbd)
table(testbd$LLUVIA,clase)
# Generar la matriz de confusión (librería caret)
library(caret)
#Modelo 1
confusionMatrix(table(pru$LLUVIA,clase))
###############################################
#Modelo 2 MODELO ELEGIDO
confusionMatrix(table(pru$LLUVIA,clase2))
#Modelo 3
confusionMatrix(table(pru$LLUVIA,clase3))




