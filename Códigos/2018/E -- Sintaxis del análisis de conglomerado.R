########################################
#                                      #
#   ANÁLISIS POR CONGLOMERADO          #
#                                      #
########################################

install.packages("rattle")
install.packages("cluster")
install.packages("reshape")

##########################################
#  Cluster según el método de k-medias   #
##########################################

data(wine, package='rattle')
head(wine)

#La estandarización 

wine.stand <- scale(wine[-1])  # con scale hacemos la estandarización
k.means.fit <- kmeans(wine.stand, 3) # fijamos en k = 3

# funciones dentro de k.means.fit
attributes(k.means.fit)


#Veamos los valores de los centroides
k.means.fit$centers

# Agrupamiento de cada observación
k.means.fit$cluster

#Tamaño del cluster
k.means.fit$size


# Una pregunta fundamental es cómo determinar el valor del parámetro k. Si consideramos el
# porcentaje de varianza explicada como una función del número de grupos, se debería elegir un número
#de grupos de modo que la adición de otro grupo no da mucho mejor modelado de los datos.  Más precisamente,
#si se traza el porcentaje de varianza explicado por los conglomerados en función del número de agrupaciones,
#los primeros grupos agregarán mucha información (explican mucha varianza), pero en algún momento la ganancia
#marginal disminuirá, dando un ángulo en el grafico.  Utilizamos el criteroio del "codo", como en el PCA.

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6) 

# ¿cuál es el número óptimo de "k"?

# Mediante la librería cluster podemos representar la solución  en 2 dimensiones

library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Con el fin de evaluar el rendimiento de la agrupación, podemos construimos una matriz de confusión:

table(wine[,1],k.means.fit$cluster)

##########################################
#  Cluster según el método jerárquico    #
##########################################

#Los métodos jerárquicos utilizan una matriz de distancia como una entrada para el algoritmo
#de agrupación. La elección de una métrica apropiada influirá en la forma de los racimos, ya 
#que algunos elementos pueden estar cerca uno del otro según una distancia y más lejos de acuerdo 
#con otro.

d <- dist(wine.stand, method = "euclidean") # Matriz de distancias euclidiana
d

#Utilizamos la distancia euclídea como una entrada para el algoritmo de agrupamiento
#(el criterio de varianza mínima de Ward minimiza la varianza total dentro del grupo):

H.fit <- hclust(d, method="ward")

#La salida de agrupación se puede mostrar en un dendrograma

plot(H.fit) # Mostrar dendrograma
groups <- cutree(H.fit, k=3) # Corte el árbol en 5 grupos
# Dibujar dendrograma con bordes rojos alrededor de los 5 grupos
rect.hclust(H.fit, k=3, border="red") 

#Evaluemos los clusters con ayuda de una matriz de confusión:

table(wine[,1],groups)



#####################################################
#  Caso de estudio I: CONSUMO DE PROTEÍNAS EUROPA   #
#####################################################

#Se consideran 25 países europeos (n = 25 unidades) y sus ingestiones de proteínas (en porcentaje) de
#nueve principales fuentes de alimento (p = 9). Los datos se enumeran a continuación.

url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
dim(food)
head(food)

#Comenzamos primero, agrupando sólo carne roja y blanca (p = 2) y k = 3 cúmulos.

set.seed(123456789) ## to fix the random starting clusters
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
grpMeat

#Lista de asignaciones de clúster

o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

#Para ver una representación gráfica de la solución de agrupación, se trazan las asignaciones de 
#los grupos en la carne roja y blanca en un diagrama de dispersión:

plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)

#A continuación, agrupamos los nueve grupos de proteínas y preparamos el programa para crear
#siete grupos. Los clusters resultantes, mostrados en color en un diagrama de dispersión de carne blanca
#contra la carne roja (cualquier otro par de características podrían ser seleccionados), en realidad 
#tiene mucho sentido. Los países en estrecha proximidad geográfica tienden a agruparse en el mismo grupo.

set.seed(123456789)
grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])

#Representación en 2D de la solución de los 7 clusters

library(cluster)
clusplot(food[,-1], grpProtein$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

#Podemos obtener también la solución mediante el método jerárquico

foodagg=agnes(food,diss=FALSE,metric="euclidian")
plot(foodagg, main='Dendrogram')
groups <- cutree(foodagg, k=4) # cut tree into 3 clusters
rect.hclust(foodagg, k=4, border="red") 

#####################################################
#  Caso de estudio 2: SEGMENTACIÓN DE CLIENTES      #
#####################################################

#La segmentación de clientes es tan simple como suena: agrupar a los clientes
#por sus características - ¿y por qué querría hacer eso? Para servir mejor 
#a sus necesidades!

getwd()
setwd(C:/Users/Oscar/Desktop/Conglomerados)


offers<-read.table('offers.csv', sep = ';', header=T)
head(offers)

transactions<-read.table('transactions.csv', sep = ';', header=T)
head(transactions)

# Create transaction matrix (a pivot table like in Excel way!)

#Organización de la información
#Tenemos dos conjuntos de datos: uno para las ofertas y el otro para las transacciones. Primero 
#lo que necesitamos hacer es crear una matriz de transacción. Eso significa que tenemos que
#poner las ofertas que enviamos al lado del historial de transacciones de cada cliente. Esto 
#se consigue fácilmente con una mesa giratoria.

library(reshape)
pivot<-melt(transactions[1:2])

#Utilizar CustomerLastName como variables de id

pivot<-(cast(pivot,value~CustomerLastName,fill=0,fun.aggregate=function(x) length(x)))
pivot<-cbind(offers,pivot[-1])

#Visualisación de los datos y la matriz de distancia

cluster.data<-pivot[,8:length(pivot)]
cluster.data<-t(cluster.data)
head(cluster.data)

#Paso 2: Distancias y Clusters
#Usaremos k = 4k = 4 indicando que usaremos 4 grupos. Esto es algo arbitrario, pero el número
#que elija debe ser representativo del número de segmentos que puede manejar como un negocio.
#Así que 100 segmentos no tiene sentido para una campaña de marketing por correo electrónico.

#Tenemos que calcular cuán lejos está cada cliente de la media del clúster. Para hacer esto
#podríamos usar muchas distancias / índice de disimilitud, una de las cuales es la 
#disimilitud de Gower.

library(cluster)
D=daisy(cluster.data, metric='gower')

#Después de la creación de una matriz de distancia, implementamos un procedimiento de 
#agrupación jerárquica de Ward:

H.fit <- hclust(D, method="ward")

plot(H.fit) # display dendrogram
groups <- cutree(H.fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters
rect.hclust(H.fit, k=4, border="red") 

# Representación 2D de la segmentación:
clusplot(cluster.data, groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Customer segments')


# Para obtener las mejores ofertas que tendremos que hacer un poco de manipulación de datos. 
# Primero tenemos que combinar nuestros clusters y transacciones. Cabe destacar que las longitudes de
# las transacciones y clusters de las "tablas" son diferentes. Así que necesitamos una manera de 
# fusionar los datos. Así que usamos la función merge () y damos a nuestras columnas nombres sensibles:

cluster.deals<-merge(transactions[1:2],groups,by.x = "CustomerLastName", by.y = "row.names")
colnames(cluster.deals)<-c("Name","Offer","Cluster")
head(cluster.deals)

# A continuación, queremos repetir el proceso de pivotar para obtener Ofertas en filas y clústeres
# en columnas contando el número total de transacciones para cada clúster. Una vez que tengamos 
# nuestra mesa pivote, la fusionaremos con la tabla de datos de ofertas como antes: 

cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
cluster.topDeals<-cbind(offers,cluster.pivot[-1])
head(cluster.topDeals)

