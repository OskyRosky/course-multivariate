#############################################
#                                           #  
#            ANÁLISIS DISCRIMINANTE         #
#                                           #
#############################################

#####################
# Ejemplo 1       #
######################

#En este primer caso de estudio, el conjunto de datos del vino, tenemos 13 concentraciones
#químicas que describen muestras de vino de tres cultivares.

install.packages("car")
install.packages("rattle")
install.packages("klaR")

library(car)
data(wine, package='rattle')
attach(wine)
head(wine)

#Estadísticas descriptivas
#Empecemos con un análisis de correlación, sin embargo es importante analizar de forma exhausiva las variables de forma individual.

scatterplotMatrix(wine[2:6])

#El propósito del análisis discriminante lineal (LDA) en este ejemplo es encontrar las combinaciones lineales
#de las variables originales (las 13 concentraciones químicas aquí) que dan la mejor separación posible entre
#los grupos (cultivares de vino aquí) en nuestro conjunto de datos.
#Puede realizar un análisis discriminante lineal utilizando la función "lda ()" del paquete R MASS. Para 
#utilizar esta función, primero debemos instalar el paquete MASS R.

#Aplicando el análisis discriminante
library(MASS)

#utilizamos la función lda para proceder con la ecuación del módelo descriminate
wine.lda <- lda(Type ~ ., data=wine)
names(wine.lda)

#Para obtener los valores de las cargas de las funciones discriminantes para los datos del vino, podemos escribir:
wine.lda

#Esto significa que la primera función discriminante es una combinación lineal de las variables:
#-0.403 * Alcohol + 0.165 * Malic ??? -0.003 * Proline-0.403 * Alcohol + 0.165 * Malic ??? -0.003 * Proline.
#Por conveniencia, el valor para cada función discriminante (por ejemplo, la primera función discriminante)
#se escala de modo que su valor medio sea cero y su varianza sea una.

#La "proporción de traza" que se imprime al escribir "wine.lda" (la variable devuelta por la función lda ()) es la
#separación porcentual obtenida por cada función discriminante. Por ejemplo, para los datos del vino obtenemos los
#mismos valores que los calculados (68,75% y 31,25%).


#Primer función discriminante
#Una buena manera de mostrar los resultados de un análisis discriminante lineal (LDA) es hacer un histograma apilado
# de los valores de la función discriminante para las muestras de diferentes grupos (diferentes cultivares de vino en nuestro ejemplo).

#Primera función discriminante
wine.lda.values <- predict(wine.lda)
ldahist(data = wine.lda.values$x[,1], g=Type)

#Segunda función discriminante
ldahist(data = wine.lda.values$x[,2], g=Type)

#Podemos ver que la primera función hace una buena separacíón de los grupos, mientras que la 2nda solo para el grupo 2.


#Gráfico de puntos de la función discriminante
#Podemos obtener un diagrama de dispersión de las dos mejores funciones discriminantes, con los puntos de datos etiquetados
#por cultivar, escribiendo:

plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # 
text(wine.lda.values$x[,1],wine.lda.values$x[,2],Type,cex=0.7,pos=4,col="red")

#Podemos ver que los vinos de los tres cultivares están bien separados en el diagrama de dispersión. La primera función discriminante
#(eje x) separa muy bien los cultivares 1 y 3, pero no separa perfectamente los 2 y 3.
#La segunda función discriminante (eje y) consigue una separación bastante buena de los cultivares 1 y 3, y de los cultivares 2 y 3,
#aunque no es totalmente perfecta.
#Para lograr una muy buena separación de los tres cultivares, sería mejor utilizar tanto la primera como la segunda funciones discriminantes juntas, 
#ya que la primera función discriminante puede separar muy bien los cultivares 1 y 3, y la segunda función discriminante puede separar 
#los cultivares 1 y 2, y los cultivares 2 y 3, razonablemente bien.

#####################
#    Ejemplo 2      #
#####################

#El conjunto de datos proporciona datos de admisión para los solicitantes de las escuelas de posgrado en los negocios. El objetivo 
#es utilizar los puntajes GPA y GMAT para predecir la probabilidad de ingreso (admitir, no admitir y limitar).

url <- 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/admission.csv'
adm <- read.csv(url)
head(adm)

attach(adm)

## Apliquemos la función de discriminación lineal
m1=lda(De~.,adm)
m1

## Realicemos las predicción para saber el grupo de pertenencia
predict(m1,newdata=data.frame(GPA=3.21,GMAT=497))
predict(m1,newdata=data.frame(GPA=2.21,GMAT=297))

# Podemos en vez de una función líneal, aplicar una función cuadrática. Utilizamos el comando "qda"
#Análisis discriminante cuadrático

m2=qda(De~.,adm)
names(m2)
m2$call
m2

## Realicemos las predicción para saber el grupo de pertenencia según la función cuadrática

predict(m2,newdata=data.frame(GPA=3.21,GMAT=497))

#¿Cuál modelo es mejor?
#Con el fin de responder a esta pregunta, se evalúa el análisis discriminante lineal 
#seleccionando al azar 60 de 85 estudiantes, la estimación de los parámetros en los datos 
#de entrenamiento, y la clasificación de los 25 estudiantes restantes de la muestra reservada. 
#Lo repetimos cien veces.

n=85
nt=60
neval=n-nt
rep=100

### LDA
set.seed(123456789)
errlin=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## linear discriminant analysis
  m1=lda(De~.,adm[train,])
  predict(m1,adm[-train,])$class
  tablin=table(adm$De[-train],predict(m1,adm[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin


### Modelo cuadrático
set.seed(123456789)
errqda=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## quadratic discriminant analysis
  m1=qda(De~.,adm[train,])
  predict(m1,adm[-train,])$class
  tablin=table(adm$De[-train],predict(m1,adm[-train,])$class)
  errqda[k]=(neval-sum(diag(tablin)))/neval
}
merrqda=mean(errlin)
merrqda

#Alcanzamos una tasa de error de 10,2% en ambos casos. R también nos dan algunas
#herramientas de visualización. Por ejemplo biblioteca klaR:

# Explorando los gráficos  Graph for LDA or QDA
#install.packages('klaR')
library(klaR)
partimat(De~.,data=adm,method="lda") 

#¿Qué podemos decir del gráfico anterior?


#####################
#    Ejemplo 3      #
#####################

#El conjunto de datos, que contiene atributos y resultados sobre 1000 solicitudes de préstamo,
#fue proporcionado en 1994 por el profesor Hans Hofmann del Institut für Statistik und Oekonometrie
#de la Universidad de Hamburgo. Ha servido como un importante conjunto de datos de prueba para 
#varios algoritmos de puntaje de crédito. Aquí se presenta una descripción de las variables.

## Datos
credit <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/germancredit.csv")
head(credit,2) 

# Como puede verse, sólo las variables: duración, cantidad, cuota y edad son numéricas.

cred1=credit[, c("Default","duration","amount","installment","age")]
head(cred1)

#Algunas estadísticas descriptivas
summary(cred1)

#interesa verificar la normalidad de las variables
hist(cred1$duration)
hist(cred1$amount)
hist(cred1$installment)
hist(cred1$age)

# creando un data.frame
cred1=data.frame(cred1)

#Apliquemos la función discriminante

library(MASS)
attach(cred1)
## LDA: class proportions of the training set used as prior probabilities
zlin=lda(Default~.,cred1)


# Matriz de confusión
table(predict(zlin)$class, Default)

# Predicción de nuebas observaciones en el LDA
predict(zlin,newdata=data.frame(duration=6,amount=100,installment=4,age=50))
predict(zlin,newdata=data.frame(duration=6,amount=100,installment=4,age=50))$class 

## Predicción de nuebas observaciones en el QDA
zqua=qda(Default~.,cred1)

# Matriz de confusión
table(predict(zqua)$class, Default)

# Predicción de nuebas observaciones en el QDA
predict(zqua,newdata=data.frame(duration=6,amount=1100,installment=4,age=67))

predict(zqua,newdata=data.frame(duration=6,amount=1100, installment=4,age=67))$class


