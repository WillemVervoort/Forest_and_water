# Análisis de componentes principales en las cuencas del proyecto Desafíos
# Basado en el package factoextra:
# http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization


wd = ("G:/DOC-2018/Cuencas_Desafios/PCA-Cuencas/Mensual")
setwd(wd)


library("FactoMineR")
library("factoextra")
library("corrplot")
library("tidyverse")


filelist <- dir(path = "./data_PCAmonth", pattern= ".csv") #Lee los archivos "datosPCA" matrices input para las 3 PCA
filelist

titles = c("April-September", "October-March", "Monthly")

  i=2
  ncuenca <- substr(filelist[i], 10, 11) 
  
  datatable <- read.csv(paste("data_PCAmonth/",filelist[i],sep=""))
  
  datatable <- datatable %>%
    filter(RRc <= 2) %>%
    select(Cuenca_date,Cuenca,BS, SS, TC, AW, Pmedia, Res, RRc, FORP, FORN, GRAS)
  
  #data standarization & PCA analisis
  res.pca <- PCA(datatable, scale.unit = TRUE, ncp = 4, quali.sup = 1:2, graph=FALSE)
  print(res.pca)
  
  ##Visualization and Interpretation
  #Eigenvalues / Variances
  eig.val <- get_eigenvalue(res.pca)
  eig.val
  
  #Scree Plot: the plot of eigenvalues ordered from largest to the smallest.
  scree.plot <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 41))
  scree.plot
  
  # Print plots to png files
  #png("pca-scree-todo.png")
  #print(scree.plot)
  #dev.off()
  
  ##Graph of variables
  #Results
  var <- get_pca_var(res.pca)
  var
  # Coordinates
  head(var$coord)
  # Cos2: quality on the factore map
  head(var$cos2)
  # Contributions to the principal components
  head(var$contrib)
  
  ##Correlation circle
  # Coordinates of variables
  head(var$coord, 10)
  
  # plot variables,
  var.plot <- fviz_pca_var(res.pca, axes = c(1, 2), col.var = "black")
  var.plot
  
  # Print plots to png files
  #png("pca-variables-todo.png")
  #print(var.plot)
  #dev.off()
  
  ##Quality of representation
  qualiVar.plot <-corrplot(var$cos2, is.corr=FALSE)
  qualiVar.plot
  # Print plots to png files
  #png("quality-pca-variables-todo.png")
  #print(qualiVar.plot)
  #dev.off()
  
  # Total cos2 of variables on Dim.1 and Dim.2
  fviz_cos2(res.pca, choice = "var", axes = 1:2)
  
  ##Contributions of variables to PCs
  head(var$contrib,10)
  png(paste("contriVars_",ncuenca,".png", sep=""))
  plotcontriVar <- corrplot(var$contrib[,1:4], is.corr=FALSE, col=c("#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                                                     "#4393C3", "#2166AC", "#053061"), 
                             tl.col="black",tl.offset=0.3, cl.pos="b", cl.cex=0.8, cl.length=6, cl.lim = c(0,100))
  # Print plots to png files
  print(plotcontriVar)
  dev.off()
  plotcontriVar
  
  # Contributions of variables to PC1
  fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
  # Contributions of variables to PC2
  fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
  # Contributions of variables to PC3
  fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
  
  #correlation plot 
  fviz_pca_var(res.pca, col.var = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  
  ##Color by groups
  # Create a grouping variable using kmeans
  # Create 3 groups of variables (centers = 3)
  set.seed(123)
  res.km <- kmeans(var$coord, centers = 3, nstart = 25)
  grp <- as.factor(res.km$cluster)
  
  # Color variables by groups
  fviz_pca_var(res.pca, col.var = grp, 
               palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
               legend.title = "Cluster")
  
  #Biplot
  titulo =titles[i]
  png(paste("biplot1-2_",ncuenca,".png", sep=""))
  biplot12 <- fviz_pca_biplot(res.pca, axes = c(1, 2), repel = TRUE, habillage = 2,
                            col.var = "black", # Variables color
                            label="var", #"var" 
                            col.ind = "#696969", # Individuals color
                            title = paste("PCA-Biplot PC1 & PC2 - ",titulo)) +
    coord_fixed(ratio = 1)
  #April-September  Annual
  
  print(biplot12)
  dev.off()
  biplot12
  
  png(paste("biplot2-3_",ncuenca,".png", sep=""))
  biplot23 <- fviz_pca_biplot(res.pca, axes = c(2, 3), repel = TRUE, habillage = 2,
                            col.var = "black", # Variables color
                            label="var", 
                            col.ind = "#696969", # Individuals color
                            title = paste("PCA-Biplot PC2 & PC3 - ",titulo)) +
    coord_fixed(ratio = 1)
  
  print(biplot23)
  dev.off()
  biplot23

  png(paste("biplot2-4_",ncuenca,".png", sep=""))
  biplot24 <- fviz_pca_biplot(res.pca, axes = c(2, 4), repel = TRUE, habillage = 2,
                            col.var = "black", # Variables color
                            label="var", 
                            col.ind = "#696969", # Individuals color
                            title = paste("PCA-Biplot PC2 & PC4 - ",titulo)) +
    coord_fixed(ratio = 1)   
  
  print(biplot24)
  dev.off()
  biplot24
  
  png(paste("biplot3-4_",ncuenca,".png", sep=""))
  biplot34 <- fviz_pca_biplot(res.pca, axes = c(3, 4), repel = TRUE, habillage = 2,
                            col.var = "black", # Variables color
                            label="var", 
                            col.ind = "#696969", # Individuals color
                            title = paste("PCA-Biplot PC3 & PC4 - ",titulo)) +
    coord_fixed(ratio = 1)   
  
  print(biplot34)
  dev.off()
  biplot34
  
  png(paste("biplot1-4_",ncuenca,".png", sep=""))
  biplot14 <- fviz_pca_biplot(res.pca, axes = c(1, 4), repel = TRUE, habillage = 2,
                              col.var = "black", # Variables color
                              label="var", 
                              col.ind = "#696969", # Individuals color
                              title = paste("PCA-Biplot PC1 & PC4 - ",titulo)) +
    coord_fixed(ratio = 1)   
  
  print(biplot14)
  dev.off()
  biplot14
  




