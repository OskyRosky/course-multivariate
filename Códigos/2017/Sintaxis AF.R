#################################
#################################
#   Análisis de factores        #
#################################
#################################

####################
#     Librerias    #
####################

install.packages("princomp")
install.packages("factanal")
install.packages("factor.pa")
install.packages("nFactors")
install.packages("FactoMineR")

library(princomp)
library(factanal)
library(factor.pa)
library(nFactors)
library(FactoMineR)

###########################################################################################
# Datos: corresponde a un estudio de 244 indivudios sobre una autoevaluación de rasgos    #
# de personalidad como "distante", "hablador", "tranquilo", etc.  Se quiere reducir la    #
# dimensión de los datos para llegar a explicar mejor las variables subyacentes           #
###########################################################################################

d = read.table("http://www.stanford.edu/class/psych253/data/personality0.txt")
attach(d)
dim(d)
head(d)

#################################################################
# Estadísticas descriptivas: correlación entre los componentes  #
#################################################################


# Utilizamos el corrplot, dado que puede ser útil para tener una idea de la 
# estructura en los datos (incluyendo la organización a mayor escala)

summary(d)
library(corrplot)
corrplot(cor(d), order = "hclust", tl.col='black', tl.cex=.75) 

#¿Qué otros análasis se le podrian hacer a las variables?}
#¿Sería importante ver la normalidad de las variables?

# Del AF surgen siempre dos preguntas
# 1. ¿Cuántos factores latentes se necesitan para explicar la mayor parte de la variación entre las variables observadas?
# 2. Qué variables parecen definir cada factor; ¿Qué etiquetas debemos dar a estos factores?

##########################
# Análisis de factores   #
##########################

#antes pasamos el archivo de datos a un data.frame

d_stan = as.data.frame(scale(d))
names(d_stan)


######################## 
# Número de factores   #
########################

library(nFactors)
ev <- eigen(cor(d_stan)) 
ap <- parallel(subject=nrow(d_stan),var=ncol(d_stan),  rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#¿Cuál debería ser el número de factores? ¿1, 5 ó 6?

########################################
##  Análisis de factores sin rotación ##
########################################

# El objetivo es averiguar qué variables "pertenecen" a qué distribución / factor latente
#Las rotaciones son: "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", o "cluster"

res1b = factanal(d_stan, factors = 10, rotation = "none", na.action = na.omit)
#Las diversas funciones del análisis de factores:
names(res1b)

# Vamos a ver cuál variable pertene a un determinado factor, gracias a las cargas factoriales
res1b$loadings



# Programando las raices característica del primero factor ¿A que correspondía?
loadings_fac1 = res1b$loadings[,1]
eigenv_fac1 = sum(loadings_fac1^2); eigenv_fac1

# Programas la proporción de la variancia
eigenv_fac1/32

####################################
# La singularidad y la comonalidad #
####################################
res1b$uniquenesses

# Calculadon la  singularidad  y la comunalidad

loadings_distant = res1b$loadings[1,]
communality_distant = sum(loadings_distant^2); communality_distant


uniqueness_distant = 1-communality_distant; uniqueness_distant

#########################################
## Gráfico de cargas en los factores    #
#########################################

load = res1b$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(d_stan),cex=.7) # agregando el nombre de las variables 
# ¿Qué podemos decir de este gráfico? ¿Cómo podemos agrupar las variables?

####################################
# Fijar el número de componentes   #
####################################

### Análisis factorial con rotación y fijación del número de componentes
res1a = factanal(d_stan, factors = 5, rotation = "varimax", na.action = na.omit)
res1a$loadings

### Gráfico de cargas en los factores
load = res1a$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(d_stan),cex=.7) # agregando el nombre de las variables

#########################################################################
# Elección de 5 componentes y denominación de las 5 variables latentes  #
#########################################################################

#Se pueden agrupar las variables a diversos factores, y ver si realmente se hizo una buena 
#discriminación en los factores

shy = rowMeans(cbind(d_stan$distant, d_stan$shy, d_stan$withdrw, d_stan$quiet))
outgoing = rowMeans(cbind(d_stan$talkatv, d_stan$outgoin, d_stan$sociabl))
hardworking = rowMeans(cbind(d_stan$hardwrk, d_stan$persevr, d_stan$discipl))
friendly = rowMeans(cbind(d_stan$friendl, d_stan$kind, d_stan$coopera, d_stan$agreebl, d_stan$approvn, d_stan$sociabl))
anxious = rowMeans(cbind(d_stan$tense, d_stan$anxious, d_stan$worryin))

# Combinando los factores y creando una nueva estructura de datos
combined_data = cbind(shy,outgoing,hardworking,friendly,anxious)
combined_data = as.data.frame(combined_data)

# Volviendo a correr el análisis de factores: análisis de las cargas factoriales
res2 = factanal(combined_data, factors = 2, na.action=na.omit)
res2$loadings

### Gráficando las cargas agrupadas para los primeros factores loadings against one another
load = res2$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(combined_data),cex=.7) # agregando el nombre de las variables

###########################
# ¿Otro tipo de rotación? #
###########################

##############################################################################
# ¿Y si utilizamos el circulo de variables visto en el análisis por factores #
##############################################################################


