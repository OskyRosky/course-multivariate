
library("factoextra")

library("FactoMineR")
data(wine)
colnames(wine)

library(FactoMineR)
data(wine)
res.mfa <- MFA(wine, 
               group = c(2, 5, 3, 10, 9, 2), 
               type = c("n", "s", "s", "s", "s", "s"),
               name.group = c("origin","odor","visual",
                              "odor.after.shaking", "taste","overall"),
               num.group.sup = c(1, 6),
               graph = FALSE)

print(res.mfa) 

res.mfa.2 <- MFA(wine)



##

library("factoextra")
eig.val <- get_eigenvalue(res.mfa)
head(eig.val)

#Sree Plot


fviz_screeplot(res.mfa)



# Groupe de variables
group <- get_mfa_var(res.mfa, "group")
group

# Coordinates of groups
head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the  dimensions
head(group$contrib)

fviz_mfa_var(res.mfa, "group")


#Contribuciones

# Contribution to the first dimension
fviz_contrib(res.mfa, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.mfa, "group", axes = 2)

fviz_contrib(res.mfa, "group", axes = 1)


### Variables cuantitativas

quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var 


# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factore map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)


#### Correlación entre var cuanti y las dimensiones 

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")


### Contribuciones ####

fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 30,
             palette = "jco")



# Contributions to dimension 2

fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 30,
             palette = "jco")

# Contributions to dimension 3

fviz_contrib(res.mfa, choice = "quanti.var", axes = 3, top = 30,
             palette = "jco")




fviz_mfa_var(res.mfa, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))




###Gráfico de los individuos

ind <- get_mfa_ind(res.mfa)
ind

fviz_mfa_ind(res.mfa, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


fviz_mfa_ind(res.mfa, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) 

fviz_ellipses(res.mfa, c("Label", "Soil"), repel = TRUE)


# Gráfico parcial de los individuos

fviz_mfa_ind(res.mfa, partial = "all") 

fviz_mfa_ind(res.mfa, partial = c("1DAM", "1VAU", "2ING")) 


#Gráfico parcial de los ejes

fviz_mfa_axes(res.mfa)





##################
#  Otro ejemplo  #
##################

library(Hmisc)
library(psych)
library(corrplot)
library(nFactors)

filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/ptechdata.csv"
pdat <- read.csv(file=filepath,header=TRUE)

pdat1 <- pdat[pdat$id==1,-1] 
pdat2 <- pdat[pdat$id==2,-1]


head(pdat1,10)
describe(pdat1)

pairs.panels(pdat1)

round(cor(pdat1),2)
corrplot(cor(pdat1), order = "original", tl.col='black', tl.cex=.75) 

corpdat1 <- cor(pdat1, use="pairwise.complete.obs")
corpdat1

fa.parallel(x=corpdat1, fm="minres", fa="fa")


nScree(x=corpdat1,model="factors")
plot(nScree(x=corpdat1,model="factors"))


#Dos soluciones posibles del FA

#1 

fa.0 <- fa(r=wine.1, nfactors = 10, rotate = "none")
print(fa.0)

#2 

Rotaciones ortogonales

fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "none")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "varimax")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "quartimax")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "bentlerT")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "equamax")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "varimin")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "geominT")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "bifactor")

Rotaciones oblicuas

fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "Promax")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "promax")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "oblimin")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "simplimax")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "bentlerQ")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "geominQ")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "biquartimin")
fa.1 <- fa(r=wine.1, nfactors = 5, rotate = "cluster")



print(fa.1)

# Diagrama 

fa.diagram(fa.1)

#3 

fa.2 <- fa(r=wine.1, nfactors = 5, rotate = "varimax")

fa.diagram(fa.2)



##################################### 

# Mediante el factanal


solution <- fa(r=cor(wine.1), nfactors = 2, rotate = "oblimin", fm="pa")

plot(fa.2, cex=.7, ylim=c(-.1,1))




#######################################

library(princomp)
library(factor.pa)
library(nFactors)
library(FactoMineR)


personality <- read.csv("C:/Users/oscar/Desktop/Labo 3/personality0.txt", sep="")


attach(personality)
dim(personality)
head(personality)


#### Creamos un data.frame 

personality = as.data.frame(scale(personality))
names(personality)

## Análisis de FA

res1b = factanal(personality, factors = 10, rotation = "none", na.action = na.omit)
#Las diversas funciones del an?lisis de factores:
names(res1b)


### Veamos el Scree plot

library(nFactors)
ev <- eigen(cor(personality)) 
ap <- parallel(subject=nrow(personality),var=ncol(personality),  rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Elegimes 5 factores


####################################
# La singularidad y la comonalidad #
####################################

res1b$uniquenesses

# Calculadon la  singularidad  y la comunalidad

loadings_distant = res1b$loadings[1,]
communality_distant = sum(loadings_distant^2); communality_distant


uniqueness_distant = 1-communality_distant; uniqueness_distant


#########################################
## Gr?fico de cargas en los factores    #
#########################################

load = res1b$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(personality),cex=.7) # agregando el nombre de las variables 

# ?Qué podemos decir de este gráfico? ?Cómo podemos agrupar las variables?


####################################
# Fijar el n?mero de componentes   #
####################################

### An?lisis factorial con rotaci?n y fijaci?n del n?mero de componentes
res1a = factanal(personality, factors = 5, rotation = "varimax", na.action = na.omit)
res1a$loadings

### Gr?fico de cargas en los factores
load = res1a$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(personality),cex=.7) # agregando el nombre de las variables


#########################################################################
# Eleccion de 5 componentes y denominacion de las 5 variables latentes  #
#########################################################################

#Se pueden agrupar las variables a diversos factores, y ver si realmente se hizo una buena 
#discriminaci?n en los factores

shy = rowMeans(cbind(personality$distant, personality$shy, personality$withdrw, personality$quiet))
outgoing = rowMeans(cbind(personality$talkatv, personality$outgoin, personality$sociabl))
hardworking = rowMeans(cbind(personality$hardwrk, personality$persevr, personality$discipl))
friendly = rowMeans(cbind(personality$friendl, personality$kind, personality$coopera, personality$agreebl, personality$approvn, personality$sociabl))
anxious = rowMeans(cbind(personality$tense, personality$anxious, personality$worryin))

# Combinando los factores y creando una nueva estructura de datos
combined_data = cbind(shy,outgoing,hardworking,friendly,anxious)
combined_data = as.data.frame(combined_data)

# Volviendo a correr el an?lisis de factores: an?lisis de las cargas factoriales
res2 = factanal(combined_data, factors = 2, na.action=na.omit)
res2$loadings

### Gr?ficando las cargas agrupadas para los primeros factores loadings against one another
load = res2$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(combined_data),cex=.7) # agregando el nombre de las variables


#Podriamos probar diversas rotaciones para ver cual separa mejor los componentes

