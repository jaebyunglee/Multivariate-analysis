# Load the data
library("MASS")
data("geyser")

# Scatter plot
library("ggpubr")
ggscatter(geyser, x = "duration", y = "waiting")+
  geom_density2d() # Add 2D density

##########################
##model-based clustering##
##########################
library("mclust")
data("diabetes")
head(diabetes, 3)

df <- scale(diabetes[, -1]) # Standardize the data
mc <- Mclust(df) 
summary(mc)

mc$modelName                # Optimal selected model ==> "VVV"
mc$G                        # Optimal number of cluster => 3
head(mc$z, 30)              # Probality to belong to a given cluster
head(mc$classification, 30) # Cluster assignement of each observation


#################
##visualization##
#################

library(factoextra)
library(gridExtra)
# BIC values used for choosing the number of clusters
z1=fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
z2=fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
z3=fviz_mclust(mc, "uncertainty", palette = "jco")
grid.arrange(z1,z2,z3,nrow=2,ncol=2)


