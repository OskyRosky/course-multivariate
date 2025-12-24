install.packages("tidytext",dependencies=TRUE)  
library(gutenbergr)
library(tidyverse)
  library(tidytext)
  library(tidytext)
  library(tm)
  library(lubridate)
  library(zoo)
  library(scales)
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(cluster)
  library(quanteda)
  library(tm)
  library(RColorBrewer)
  library(ggplot2)
  library(wordcloud)
  library(biclust)
  library(igraph)
  library(fpc)

  #install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
  
  library(rvest)
  library(httr)
  library(xml2)
  library(jsonlite)
  library(tidyverse)
  library(tidytext)
  library(lubridate)
  library(scales)
  library(janeaustenr)
  library(dplyr)
  library(stringr)
  
######################################################################
# Análisis de sentimiento con R                                      #
# https://rpubs.com/jboscomendoza/analisis_sentimientos_lexico_afinn #                             #
######################################################################

#Definir tema

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

#Importar datos

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/tuits_candidatos.csv",
              "tuits_candidatos.csv")

tuits <- read.csv("tuits_candidatos.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

#Lexicoon 

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

#Preparando los datos

tuits <- 
  tuits %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
           remove = FALSE) %>%
  mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2018)

#Convertir tuits en palabras

tuits_afinn <- 
  tuits %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)


# Obtenemos también una puntuación por tuit, usando group_by() y summarise() de dplyr,
# y la agregamos tuits para usarla después. Tambien asignamos a los tuits sin puntuación
# positiva o negativa un valor de 0, que indica neutralidad. Por último cambiamos el 
#nombre de la columna screen_name a Candidato

tuits <-
  tuits_afinn %>%
  group_by(status_id) %>%
  summarise(Puntuacion_tuit = mean(Puntuacion)) %>%
  left_join(tuits, ., by = "status_id") %>% 
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit)) %>% 
  rename("Candidato" = screen_name)


#Explorando datos

tuits_afinn %>%
  count(Candidato)

# Únicas
tuits_afinn %>% 
  group_by(Candidato) %>% 
  distinct(Palabra) %>% 
  count()

#  Palabras positivas y negativas más usadas por cada uno de ellos, usando map() 
# de purr, top_n() de  dplyr() y ggplot.

map(c("Positiva", "Negativa"), function(sentimiento) {
  tuits_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Candidato) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Candidato) +
    geom_col() +
    facet_wrap("Candidato", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})


# Quitamos “no” de nuestras palabras. Es una palabra muy comun en español que no
# necesariamente implica un sentimiento negativo. Es la palabra negativa más frecuente
# entre los candidatos, por lo que podría sesgar nuestros resultados.

tuits_afinn <-
  tuits_afinn %>%
  filter(Palabra != "no") 

# Como deseamos observar tendencias, vamos a obtener la media de sentimientos por día,
# usando group_by() y summarise() y asignamos los resultados a tuits_afinn_fecha

tuits_afinn_fecha <-
  tuits_afinn %>%
  group_by(status_id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Candidato, Fecha) %>%
  summarise(Media = mean(Puntuacion))

# Veamos nuestros resultados con ggplot()

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")

# No nos dice mucho. Sin embargo, si separamos las líneas por candidato, usando 
# facet_wrap(), será más fácil observar el las tendencias de los Candidatos.

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf +
  theme(legend.position = "none")

# Regresion LOESS

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

# En realidad, podemos obtener líneas muy similares directamente de las puntuaciones.

tuits_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Candidato) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

# Si separamos las lineas por candidato y mostramos los puntos a partir de los cuales
# se obtienen las líneas de regresión, podemos observar con más claridad la manera en
# que el algoritmo LOESS llega a sus resultado. Haremos esto con facet_wrap() y  geom_point.

tuits_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Candidato) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~Candidato) +
  tema_graf

#Usando media móvil

tuits_afinn_fecha %>%
  group_by(Candidato) %>%
  mutate(MediaR = rollmean(Media, k = 3, align = "right", na.pad = TRUE)) %>%
  ggplot() +
  aes(Fecha, MediaR, color = Candidato) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf

# Comparando sentimientos negativos y positivos

tuits_afinn %>%
  count(Candidato, Tipo) %>%
  group_by(Candidato) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Candidato, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")

#Otra forma 

tuits_afinn %>%
  group_by(Candidato, Fecha) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(Candidato~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  tema_graf +
  theme(legend.position = "top")

# Bloxplots (diagrama caja y bigotes)

tuits %>%
  ggplot() +
  aes(Candidato, Puntuacion_tuit, fill = Candidato) +
  geom_boxplot() +
  tema_graf

# Bloxplots en el tiempo

tuits %>%
  mutate(Mes = factor(Mes)) %>% 
  ggplot() +
  aes(Mes, Puntuacion_tuit, fill = Candidato) +
  geom_boxplot(width = 1) +
  facet_wrap(~Candidato) +
  tema_graf +
  theme(legend.position = "none")

# Tendencia de sentimientos usando densidades

tuits %>%
  ggplot() +
  aes(Puntuacion_tuit, color = Candidato) +
  geom_density() +
  facet_wrap(~Candidato) +
  tema_graf

tuits %>%
  ggplot() +
  aes(Puntuacion_tuit, color = Candidato) +
  geom_density() +
  facet_grid(Candidato~Mes) +
  tema_graf

###########################################################
# Introducción a la mineria de textos con R               #
# https://rpubs.com/jboscomendoza/mineria-de-textos-con-r #                                          #
###########################################################

setwd("C:/Users/oscar.centeno/Desktop/TM")


#Importar datos

nov_raw <- read_lines("49836-0.txt", skip = 419, n_max = 8313-419)

#Preparar los datos

str(nov_raw)

#Creación de "párrafos"

diez <- rep(1:ceiling(length(nov_raw)/10), each = 10)
diez <- diez[1:length(nov_raw)]
nov_text <- cbind(diez, nov_raw) %>% data.frame()
nov_text <- aggregate(formula = nov_raw ~ diez,
                      data = nov_text,
                      FUN = paste,
                      collapse = " ")
nov_text <- nov_text %>% as.matrix

# nov_text <- as.matrix(nov_text)

dim(nov_text)



nov_text <-
  cbind(
    rep(1:ceiling(length(nov_raw)/10), each = 10) %>%
      .[1:length(nov_raw)],
    nov_raw
  ) %>%
  data.frame %>%
  aggregate(
    nov_raw ~ V1,
    data = .,
    FUN = paste,
    collapse=" ") %>%
  nov_raw %>%
  as.matrix

dim(nov_text)


#Limpieza de texto

nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
nov_text <- removeNumbers(nov_text)
nov_text <- stripWhitespace(nov_text)
nov_text <- tolower(nov_text)
nov_text <- removeWords(nov_text, words = stopwords("spanish"))
nov_text <- removePunctuation(nov_text)

#Análisis de Corpus 

nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus



#Nube de palabras

nov_ptd <- tm_map(nov_corpus, PlainTextDocument)
wordcloud(nov_ptd, max.words = 800, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

nov_text <- removeWords(nov_text, words = c("usted", "pues", "tal", "tan", "así", "dijo", "cómo", "sino", "entonces", "aunque", "don", "doña"))

nov_corpus <- nov_text %>% VectorSource() %>% Corpus()
nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)

wordcloud(
  nov_ptd, max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

# Term Document Matrix

nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm

#Gráfica de frecuencias

nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)

nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

wordcloud(
  words = nov_mat$palabra, 
  freq = nov_mat$frec, 
  max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

nov_mat[1:20, ]

nov_mat[1:10, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes en Niebla",  x = "Palabras", y = "Número de usos")

#Otra forma

nov_mat %>%
  mutate(perc = (frec/sum(frec))*100) %>%
  .[1:10, ] %>%
  ggplot(aes(palabra, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Diez palabras más frecuentes en Niebla", x = "Palabras", y = "Porcentaje de uso")

#Asociación de palabras

findAssocs(nov_tdm, terms = c("augusto", "eugenia", "hombre", "mujer"), corlimit = .25)

#Cluster

# Realizaremos análisis de agrupaciones jerárquicas para identificar grupos de palabras relacionados entre sí, a partir de la distancia que existe entre ellos.

#Usaremos las función removeSparseItems para depurar nuestra matriz de términos de aquellas palabras que aparecen con muy poca frecuencia, es decir, son dispersos (“sparse”).
#Esta función requiere que especifiquemos el argumento sparse, que puede asumir valores de 0 a 1. Entre valor representa la dispersión de las palabras que queremos conservar. Si lo fijamos muy alto (cerca de 1, pero no 1), conservaremos muchas palabras, casi todas, pues estamos indicando que queremos conservar términos aunque sean muy dispersos. Naturalmente, lo opuesto ocurre si fijamos este valor muy bajo (cerca de 0, pero no 0), pudiendo incluso quedarnos con ningún término, si las palabras en nuestros documentos son dispersas en general.
#Qué valor fijemos depende del tipo de documento que tengamos, por lo que es aconsejable realizar ensayos hasta encontrar un equilibrio entre dispersión y número de términos. En este caso, he decidido fijarlo en .95 y guardaremos la nueva matriz de términos en el objeto nov_new

#Comparamos cuántos términos teníamos originalmente y con cuántos nos hemos quedado, observando a cuánto equivale terms.

nov_new <- removeSparseTerms(nov_tdm, sparse = .95)
nov_tdm
nov_new

#De 7236 términos que teníamos, nos hemos quedado con 42, lo cual reduce en gran medida la dificultad y complejidad de los agrupamientos, lo cual es deseable. Es poco útil tener agrupaciones que son únicamente visualizaciones del texto original.
#También podemos ver el número de términos pidiéndo el número de renglones de nuestra matriz de términos, que es igual al número de palabras que contiene.

nov_new <- nov_new %>% as.matrix()
  
# Matriz de distancia

#Necesitamos crear una matriz de distancias para empezar agrupar, lo cual requiere que los valores en las celdas sean estandarizados de alguna manera.
#Podríamos usar la función scale, pero realiza la estandarización usando la media de cada columna como referencia, mientras que nosotros necesitamos como referencia la media de cada renglón.
#Así que obtenemos una estandarización por renglones de manera manual.

nov_new <- nov_new %>% as.matrix()

#Hecho esto, nuestra matriz ha sido estandarizada.
#Procedemos a obtener una matriz de distancia a partir de ella, con el método de distancias euclidianas y la asignamos al objeto nov_dist.

nov_dist <- dist(nov_new, method = "euclidian")

#Realizaremos nuestro agrupamiento jerárquico usando la función hclust, de la base de R. Este es en realidad un procedimiento muy sencillo una vez que hemos realizado la preparación.
#Usaremos el método de Ward (ward.D), que es el método por defecto de la función hclust y asignaremos sus resultados al objeto nov_hclust.

nov_hclust <-  hclust(nov_dist, method = "ward.D")

#Graficamos los resultados usando plot para generar un dendrograma.

plot(nov_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")

#De este modo podemos observar los grupos de palabras que existen en Niebla. Por ejemplo, “augusto” y “eugenia” forman un grupo, “puede” y “ser”, forman otro grupo (“puede ser” es una frase común en este libro).
#Además, podemos ver qué palabras pertenecen a grupos lejanos entre sí, por ejemplo, “quiero” y “verdad”.
#Podemos enfatizar los grupos de palabras trazando un rectángulo usando rect.hclust y con especificando cuántos grupos (k) deseamos resaltar.
#Crearemos el mismo gráfico pidiendo diez grupos.

plot(nov_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")
rect.hclust(nov_hclust, k = 10, border="blue")

#El paquete cluster nos proporciona más métodos para realizar agrupamientos. Uno de ellos es agnes, que inicia asumiendo que cada elemento a agrupar por si mismo es un grupo y después crea grupos de grupos a partir de las distancias entre ellos, hasta que no es posible crear más grupos.
#Realizamos prácticamente el mismo procedimiento que con hclust, sólo cambiamos el método a average. Asignaremos nuestros resultados al objeto nov_agnes.

nov_agnes <- agnes(nov_dist, method = "average")

#Ahora graficamos nuestros resultados. Un agrupamiento creado con agnes genera dos gráficos, el primero muestra cómo se obtuvieron los grupos finales y el segundo es un dendrograma.
#Pediremos el segundo gráfico (which.plots = 2).

plot(nov_agnes, which.plots = 2, main = "Dendrograma de Niebla - Agnes", sub = "", xlab = "")


# Enfatizamos diez grupos.

plot(nov_agnes, which.plots = 2, main = "Dendrograma de Niebla - Agnes", sub = "", xlab = "")
rect.hclust(nov_agnes, k = 10, border = "blue")

Las agrupaciones que hemos obtenido usando hclust y agnes son diferentes entre sí. La decisión de qué método usemos depende de nuestros propósitos y de nuestra familiaridad con ellos.

##########################################################
# Webscrapping APIs y minería de texto con R             #
# https://rpubs.com/jboscomendoza/coheed_and_cambria     #
##########################################################



#######################################
# Basic Text Mining in R              #
# https://rpubs.com/pjmurphy/265713   # 
#######################################




#########################################################################
# Text Mining is Fun (with R)!                                          #
# https://medium.com/@actsusanli/text-mining-is-fun-with-r-35e537b12002 #
#########################################################################


######################
# tidytext mining    #
######################

text <- c("Gloriana, mujer joven y hermosa nacida en 1985 es una de las-",
          "mejores, o tal vez la mejor medica general de nuestro pais. En-",
          "sus estudios de maestria obtuvo la mejor calificacion de la-",
          "generacion, y sus actos quedaron felicitados por sus profesores y",
          "compañeros. Ahora Gloriana debe seguir adelante demostrando su gran",
          "valentia, esfuerzo, pasión y dedicación para la medicina.",
          "Ella es Gloriana, o también abrebiada como GMC.")


text

library(dplyr)
text_df <- tibble(line = 1:7, text = text)

text_df



  library(tidytext)

conteo <- text_df %>% 
  unnest_tokens(word, text)

conteo %>%
  count(word, sort = TRUE) 


conteo %>%
  count(word, sort = TRUE) %>%
 # filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Otro ejemplo 





#Caso de jane austenrs

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()
original_books


library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 

library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


###


nov_raw <- read_lines("49836-0.txt", skip = 419, n_max = 8313-419)
type(nov_raw)

as.data.frame(nov_raw)
dim(nov_raw)

library(dplyr)
text_df <- tibble(line = 1:4, text = text)

text_df




#####


