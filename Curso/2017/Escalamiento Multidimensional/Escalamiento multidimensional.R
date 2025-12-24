##################################################
#                                                # 
#  Escalamiento multidimensional                 #
#                                                #
##################################################

install.packages("igraph")
install.packages("vegan")

###################################
# Variables métricas  NON-METRIC  #
###################################

##############
# Ejemplo 1  #
##############


#El presente ejemplo muestra la disposición automática de ciudades australianas basadas en las
#distancias entre ellas. El diseño obtenido con MDS está muy cerca de sus ubicaciones en un mapa.
#Los datos de las distancias entre 8 ciudades en Australia se cargan desde
#http://rosetta.reltech.org/TC/v15/Mapping/data/dist-Aus.csv.

url <- "http://rosetta.reltech.org/TC/v15/Mapping/data/dist-Aus.csv"
dist.au <- read.csv(url)

#Alternativamente, podemos descargar el archivo primero y luego leerlo en R desde la unidad local.

dist.au <- read.csv("dist-Aus.csv")
dist.au

#A continuación, eliminamos la primera columna, las siglas de las ciudades, y establecerlos en nombres de fila.

row.names(dist.au) <- dist.au[, 1]
dist.au <- dist.au[, -1]
dist.au

##       A   AS    B    D    H    M    P    S
## A     0 1328 1600 2616 1161  653 2130 1161
## AS 1328    0 1962 1289 2463 1889 1991 2026
## B  1600 1962    0 2846 1788 1374 3604  732
## D  2616 1289 2846    0 3734 3146 2652 3146
## H  1161 2463 1788 3734    0  598 3008 1057
## M   653 1889 1374 3146  598    0 2720  713
## P  2130 1991 3604 2652 3008 2720    0 3288
## S  1161 2026  732 3146 1057  713 3288    0

#Después de eso, ejecutamos Multidimensional Scaling (MDS) con la función cmdscale (),
#y obtenemos coordenadas "x" "y". K es la dimensión máxima del espacio en la que los datos 
#deben ser representados, y eig indica si los valores propios deben ser devueltos.

fit <- cmdscale(dist.au, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

#Entonces visualizamos el resultado, que muestra las posiciones de las ciudades que están 
#muy cerca de sus ubicaciones relativas en un mapa.

plot(x, y, pch = 19, xlim = range(x) + c(0, 600))
city.names <- c("Adelaide", "Alice Springs", "Brisbane", "Darwin", "Hobart", 
                "Melbourne", "Perth", "Sydney")
text(x, y, pos = 4, labels = city.names)

#Al girar los dos ejes X e Y, Darwin y Brisbane se mueven a la parte superior (norte), 
#lo que facilita la comparación con un mapa.

x <- 0 - x
y <- 0 - y
plot(x, y, pch = 19, xlim = range(x) + c(0, 600))
text(x, y, pos = 4, labels = city.names)

# MDS también se implementa en el paquete igraph como layout.mds. A continuación,
#creamos un gráfico de 8 nodos y definimos su disposición con la matriz de distancia anterior. 
#También muestra un diseño muy cercano al de un mapa.

library(igraph)
g <- graph.full(nrow(dist.au))
V(g)$label <- city.names
layout <- layout.mds(g, dist = as.matrix(dist.au))
plot(g, layout = layout, vertex.size = 3)


################################
#    Varibles cualitativas     #
################################

##########################
# Ejemplo 2 : NON-METRIC #
##########################

#El escalamiento multidimensional no-métrico (MDS, también NMDS y NMS) es una técnica de ordenación
#Que difiere de varias maneras de casi todos los otros métodos de ordenación. En la mayoría del 
#Método, se calculan muchos ejes, pero sólo unos pocos se ven, debido a limitaciones gráficas.

#Lo primero es trabajar con el archivo de datos de dune, que se encuentra en la librería de vegn


#El paquete vegano contiene todos los métodos comunes de ordenación: Componente principal
#Análisis (función rda, o prcomp en la base R), análisis de correspondencia
#(Cca), análisis de correspondencia detrended (decorana) y un envoltorio para no métricas
#Escala multidimensional (metaMDS). Las funciones rda y cca son principalmente
#Diseñado para la ordenación restringida, y será discutido más adelante. En este capítulo
#Describo funciones decorana y metaMDS.

#Acá tenemos que pasar de un supuesto de análisis de correspondencia, a un análisis de escalamiento.

library(vegan) 
data(dune)
dim(dune)
ord <- decorana(dune)    #cambiamos el nombre a ord por tener variables no métricas ordenadas.
ord                      # Y con esto logramos el escalamiento

#########################################
# Entonces el análisis des escalamiento #
#########################################

#La función metaMDS es un caso especial. La ordenación real es realizada por
#Función vegan función monoMDS (o alternativamente usando isoMDS de la MASSA
#paquete). La función metaMDS es una envoltura para realizar operaciones multidimensionales no métricas
#Escala (nmds) como se recomienda en la ordenación comunitaria:
#Medidas de disimilitud (función vegdist), entonces ejecuta nmds varias veces
#Con configuraciones de inicio aleatorias, compara los resultados (función procrustes),
#Y se detiene después de encontrar dos veces una solución de tensión mínima similar. Finalmente se escala
#Y rota la solución, y añade puntuaciones de especie a la configuración según la ponderación
#Promedios (función wascores):

ord <- metaMDS(dune)
ord
plot(ord)

plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(ord, display = "spec", cex=0.7, col="blue")

###########################################
# Agregando figuras para la visualización #
###########################################

#Vegan tiene un grupo de funciones para agregar información sobre clasificación o
#Agrupación de puntos en diagramas de ordenación. Función ordihull agrega convexo
#Cascos, ordiellipse agrega elipses que encierran todos los puntos del grupo (elipsoide
#Cascos) o elipses de desviación estándar, áreas de error estándar o de confianza, y
#Ordispider combina elementos con su centroide

data(dune.env)
attach(dune.env)

plot(ord, disp="sites", type="n")
ordihull(ord, Management, col=1:4, lwd=3)
ordiellipse(ord, Management, col=1:4, kind = "ehull", lwd=3)
ordiellipse(ord, Management, col=1:4, draw="polygon")
ordispider(ord, Management, col=1:4, label = TRUE)
points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)

#Además, puede superponer un dendrograma de clúster desde hclust usando ordicluster
#O un árbol de expansión mínimo de spantree con su función de líneas. Segmentario
#Las flechas se pueden agregar con ordiarrows, líneas con ordisegments y regulares
#Rejillas con ordigrid.
