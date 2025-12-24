################################################
#                                              #
#               Árboles de decisión            #
#                                              #
################################################

install.packages("rpart")
install.packages("party")

#######################
# Modelos de Árboles  #
#######################

#El particionamiento recursivo es una herramienta fundamental en la minería de
#datos. Nos ayuda a explorar la estructura de un conjunto de datos, al mismo tiempo
#que desarrollamos fácil de visualizar las reglas de decisión para predecir un
#resultado categórico (árbol de clasificación) o continuo (árbol de regresión).
#Esta sección describe brevemente el modelado CART.

#######################
#    Modelos CART     #
#######################

#Los árboles de clasificación y regresión (como se describe por Brieman, Freidman,
#Olshen y Stone) se pueden generar a través del paquete de la rpart. Información
#detallada sobre rpart está disponible en Introducción a la partición recursiva
#mediante las rutinas RPART. A continuación se proporcionan los pasos generales 
#seguidos por dos ejemplos.


##########################################################
# La estructura para la creación de un árbol de desición #
##########################################################

# rpart(formula, data=, method=,control=) 
# en donde
# formula: del tipo outcome ~ predictor1+predictor2+predictor3+ect
# data= specifies the data frame
# method= "class" para un árbol de clasificación
# method= "anova" para un árbol de regresión 
# control= parámetros opcionales para controlar el crecimiento de los árboles.
# Por ejemplo, control = rpart.control (minsplit = 30, cp = 0.001) requiere 
# que el número mínimo de observaciones en un nodo sea 30 antes de intentar
# una división y que una división debe disminuir la falta total de ajuste por
# un factor de 0.001 

#############################
# Ejemplo de clasificación  #
#############################

# Vamos a predecir un tipo de deformación (cifosis) después de la cirugía, 
# desde la edad en meses (Edad), el número de vértebras involucradas (Número)
# y las vértebras superiores operadas en (Inicio).

# Classification Tree with rpart
library(rpart)
data(kyphosis)
head(kyphosis)

# Crecimiento del árbol
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)
names(fit)
fit$method

printcp(fit) # muestra los resultdos
plotcp(fit) # visualize cross-validation results 
summary(fit) # El resumen en lo más importante

#Predicción de las clasificaciones

predicted= predict(fit)
predicted

# Gráfico del árbol
#Es mejor visualizar los árboles mediante un gráfico. Todo esto a continuación. 

plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#La poda del árbol 
#Vamos a evitar la sobre o sub clasificación

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pfit)

# plot the pruned tree 
plot(pfit, uniform=TRUE,  main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#############################
# Ejemplo de regresión      #
#############################

#Los datos son cu.summary
#

# Regression Tree Example
library(rpart)
data(cu.summary)
head(cu.summary)

# Hagamos crecer el árbol
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
             method="anova", data=cu.summary)   #OJO que en method cambiamos a ANOVA
summary(fit)


printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

predicted= predict(fit)
predicted

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#######################
#  La poda del árbol  #
#######################

pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

predicted2= predict(pfit)
predicted2

###################################################

library("party")
str(iris)

iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
print(iris_ctree)

plot(iris_ctree)
plot(iris_ctree, type="simple")

