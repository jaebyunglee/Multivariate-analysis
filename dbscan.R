rm(list=ls())
#install.packages("dbscan")
#install.packages("fpc")
library(dbscan)
library(factoextra)

#####################################################################
###이 데이터를 k mean 또는 Model based로 하면 클러스터링이 잘 안됨###
#####################################################################
###kmeans
data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
z1=fviz_cluster(km.res, df,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic(),main="k-means")
###model based
library("mclust")
mc <- Mclust(df)
z2=fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco",main="model-based")

###plot
library(gridExtra)
grid.arrange(z1,z2,ncol=2)



###############################
#####DBSCAN(density based)#####
###############################
# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic(),main="dbscan")

##############################################
#Method for determining the optimal eps value#
##############################################
dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)
