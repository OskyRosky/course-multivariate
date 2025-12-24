#####################################################
#                                                   #
#                                                   #
#      Análisis por componentes principales         #
#                                                   #
#                                                   #
#####################################################

#The first part of this article describes quickly how to compute and visualize principal component analysis
#using FactoMineR and factoextra
#The second part shows how to identify the most important
#variables that explain the variations in your data

#Librerías#

install.packages("FactoMineR")
install.packages("factoextra")
install.packages("devtools")
install.packages("ggplot2")

library(FactoMineR)
library(factoextra)

###########################
# Preparación de los datos#
###########################

data(decathlon2)
decathlon2
# Extract active variables/individuals for PCA
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])

###############################
# PCA mediante el FactoMineR  #
###############################

library(FactoMineR)
res.pca <- PCA(decathlon2.active, graph = TRUE)
res.pca
#Todas las funciones de un PCA#
print(res.pca)


###############################
# Interpretación del PCA      #
###############################

##############################################
# Variancias de los componentes principales  #
##############################################

#The proportion of variation retained by the principal components (PCs) can be extracted as follow :

eigenvalues <- res.pca$eig
head(eigenvalues[,])


#The amount of variation retained by each PC is called eigenvalues. The first PC corresponds to the direction with 
#the maximum amount of variation in the data set.

library("factoextra")
fviz_screeplot(res.pca, ncp=10)

# ¿Cuántos componentes debemos escoger?
# 60% of the information (variances) contained in the data are retained by the first two principal components.

###########################################################################
# Gráfico con correlaciones y cargas de las variables con los componentes #
###########################################################################

#Representación de las variables en el circulo cosénico 

# Coordinates of variables
head(res.pca$var$coord)

fviz_pca_var(res.pca)

# Correlation circle can help to visualize the most correlated variables (i.e, variables that group together).

# ¿Qué podemos decir de la intrepretación del círculo?


###########################################################################
#   Calidad de la representación para variables en el mapa factor         #
###########################################################################


head(res.pca$var$cos2)

#The cos2 values are used to estimate the quality of the representation
#The closer a variable is to the circle of correlations, the better its representation on the factor map (and the more important it is to interpret these components)
#Variables that are closed to the center of the plot are less important for the first components.

fviz_pca_var(res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

###########################################################################
#   Contribuciones de las variables a los componentes principales         #
###########################################################################

#The contributions of variables in accounting for the variability in a given
#principal component are (in percentage) : (variable.cos2 * 100) / (total cos2 of the component)

head(res.pca$var$contrib)

#The larger the value of the contribution, the more the variable contributes to the component.

# Contributions of variables on PC1
fviz_pca_contrib(res.pca, choice = "var", axes = 1)

# Contributions of variables on PC2
fviz_pca_contrib(res.pca, choice = "var", axes = 2)

# Total contribution on PC1 and PC2
fviz_pca_contrib(res.pca, choice = "var", axes = 1:2)

#The total contribution of a variable, on explaining the variations
#retained by PC1 an PC2, is calculated as follow : (C1 * Eig1) + (C2 * Eig2)

fviz_pca_contrib(res.pca, choice = "var", axes = 1, top = 7)

# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib")

# Change the gradient color
fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()

#This is helpful to highlight the most important variables in explaining 
#the variations retained by the principal components.


################################
# Descripción de la dimensión  #
################################

res.desc <- dimdesc(res.pca, axes = c(1,2))
# Description of dimension 1
res.desc$Dim.1

#The top significant variables for the dimension 1 are : X100m (p = 2.7E-7) and
#Long.jump(p = 6.1E-6)

# Description of dimension 2
res.desc$Dim.2

#The top significant variables for the dimension 2 are : 
#Pole.vault (p = 3.2E-6) and X1500m(p = 9.4E-6)

#########################################################
#########################################################
#               Gráfico de los individuos               #
#########################################################
#########################################################

head(res.pca$ind$coord)

fviz_pca_ind(res.pca)

##########################################################################################
# Cos2: calidad de la representación para los individuos en los componentes principales  #
##########################################################################################

head(res.pca$ind$cos2)

fviz_pca_ind(res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

#################################################################
# Contribución de los individuos a los componentes principales  #
#################################################################

head(res.pca$ind$contrib)

# Contributions of individuals to PC1
fviz_pca_contrib(res.pca, choice = "ind", axes = 1)

# Contributions of the individuals to PC2
fviz_pca_contrib(res.pca, choice = "ind", axes = 2)

# Total contribution on PC1 and PC2
fviz_pca_contrib(res.pca, choice = "ind", axes = 1:2)

# Contributions of the individuals to PC1
fviz_pca_contrib(res.pca, choice = "ind", axes = 1, top = 10)

# Change the gradient color
fviz_pca_ind(res.pca, col.ind="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()
