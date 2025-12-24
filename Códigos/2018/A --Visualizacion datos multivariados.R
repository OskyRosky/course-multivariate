######################################################################################## 
######################################################################################## 
#                          Visualización de datos multivariados                       #
######################################################################################## 
######################################################################################## 
  
################################################    
#      EL Gráfico de puntos (scatterplot)      #
################################################   
  

attach(mtcars)
plot(wt, mpg, main="Scatterplot Example", xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)


#  Agregamos línea de tendencia  #
  
abline(lm(mpg~wt), col="red") # regression line (y~x) 
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)

library(car) 
scatterplot(mpg ~ wt | cyl, data=mtcars,  xlab="Weight of Car", ylab="Miles Per Gallon", main="Enhanced Scatter Plot", 
            labels=row.names(mtcars))


################################################     
#            La matriz de puntos               #      
################################################   

  pairs(~mpg+disp+drat+wt,data=mtcars, main="Simple Scatterplot Matrix")

#
  
library(car)
scatterplotMatrix(~mpg+disp+drat+wt|cyl, data=mtcars,main="Three Cylinder Options")



#Matriz con colores#

library(gclus)
dta <- mtcars[c(1,3,5,6)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )


#COrrelaciones con tendencias#

library(car)
scatterplotMatrix(~mpg+disp+drat+wt|cyl, data=mtcars, main="Three Cylinder Options")

  
################################################     
#            Puntos tridimensionales           #      
################################################  

#Gráfico tridimensional simple
attach(dta)
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg, main="3D Scatterplot")

#COn bastanes en el plano
scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

#hiperplano
s3d <-scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(mpg ~ wt+disp) 
s3d$plane3d(fit)



################################################     
#            Gráficos de correlogramas         #      
################################################  

library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")



library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Car Milage Data in PC2/PC1 Order")

library(corrgram)
corrgram(mtcars, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Car Milage Data (unsorted)")

# Changing Colors in a Correlogram
library(corrgram) 
col.corrgram <- function(ncol){   
  colorRampPalette(c("darkgoldenrod4", "burlywood1",
                     "darkkhaki", "darkgreen"))(ncol)} 
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt, 
         main="Correlogram of Car Mileage Data (PC2/PC1 Order)")

################################################     
#            Box plot bivariados           #      
################################################ 

# Boxplot of MPG by Car Cylinders
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")

# Notched Boxplot of Tooth Growth Against 2 Crossed Factors
# boxes colored for ease of interpretation 
boxplot(len~supp*dose, data=ToothGrowth, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Suppliment and Dose")


# Gráficos de violín

library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3, names=c("4 cyl", "6 cyl", "8 cyl"), 
        col="gold")
title("Violin Plots of Miles Per Gallon")

library(aplpack)
attach(mtcars)
bagplot(wt,mpg, xlab="Car Weight", ylab="Miles Per Gallon",
        main="Bagplot Example")


##################################
#      Gráficos de contorno      #
##################################

library(MASS)
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .5, .5, 1), 2)  # Covariance matrix
# > Sigma
# [,1] [,2]
# [1,]  1.0  0.1
# [2,]  0.1  1.0

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma )  # from Mass package
head(bivn)                                      
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # from MASS package
#R offers several ways of visualizing the distribution. These next two lines of
#code overlay a contour plot on a "heat Map" that maps the density of points to a gradient of colors.

# Contour plot overlayed on heat map image of results
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package

#Otra forma de ver el contorno

library(hexbin)
x <- rnorm(1000)
y <- rnorm(1000)
bin<-hexbin(x, y, xbins=50) 
plot(bin, main="Hexagonal Binning")


################################
#      Gráficos de densidad    #
################################

# Gráfico de densidad #


library(lattice) 
attach(mtcars)

# create factors with value labels 
gear.f<-factor(gear,levels=c(3,4,5),
               labels=c("3gears","4gears","5gears")) 
cyl.f <-factor(cyl,levels=c(4,6,8),
               labels=c("4cyl","6cyl","8cyl")) 

# kernel density plot 
densityplot(~mpg, 
            main="Density Plot", 
            xlab="Miles per Gallon")

# kernel density plots by factor level 
densityplot(~mpg|cyl.f, 
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon")

# kernel density plots by factor level (alternate layout) 
densityplot(~mpg|cyl.f, 
            main="Density Plot by Numer of Cylinders",
            xlab="Miles per Gallon", 
            layout=c(1,3))



library(ellipse)
rho <- cor(bivn)
y_on_x <- lm(bivn[,2] ~ bivn[,1])    # Regressiion Y ~ X
x_on_y <- lm(bivn[,1] ~ bivn[,2])    # Regression X ~ Y
plot_legend <- c("99% CI green", "95% CI red","90% CI blue",
                 "Y on X black", "X on Y brown")

plot(bivn, xlab = "X", ylab = "Y",
     col = "dark blue",
     main = "Bivariate Normal with Confidence Intervals")
lines(ellipse(rho), col="red")       # ellipse() from ellipse package
lines(ellipse(rho, level = .99), col="green")
lines(ellipse(rho, level = .90), col="blue")
abline(y_on_x)
abline(x_on_y, col="brown")
legend(3,1,legend=plot_legend,cex = .5, bty = "n")
The next bit of code generates a couple of three dimensional surface plots. The second of which is an rgl plot that you will be able to rotate and view from different perspectives on your screen.

# Three dimensional surface
# Basic perspective plot
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA) # from base graphics package




#####################################
#      Histogramas multivariados    #
#####################################

library(lattice) 
attach(mtcars)
data(mtcars)

# create factors with value labels 
gear.f<-factor(gear,levels=c(3,4,5),
               labels=c("3gears","4gears","5gears")) 

# boxplots for each combination of two factors 
bwplot(cyl.f~mpg|gear.f,
       ylab="Cylinders", xlab="Miles per Gallon", 
       main="Mileage by Cylinders and Gears", 
       layout=(c(1,3)))


################################################     
#            Gráficos de Mosaico             #      
################################################ 

library(vcd)
mosaic(HairEyeColor, shade=TRUE, legend=TRUE)


assoc(HairEyeColor, shade=TRUE)




