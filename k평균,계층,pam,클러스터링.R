rm(list=ls())
# install.packages("factoextra")
# install.packages("cluster")
# install.packages("magrittr")
# install.packages("NbClust")
library("cluster")
library("factoextra")
library("dplyr")
library("magrittr")
library(graphics)
library(ggplot2)
library(NbClust)


data("USArrests")
head(USArrests,3)
my_data <- USArrests %>%
  na.omit() %>%          # Remove missing values (NA)
  scale()  

#거리행렬
res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
#거리행렬 시각화
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

##########################
###k - means 클러스터링###
##########################
#클러스터 갯수 확인 1
fviz_nbclust(my_data, kmeans)
#클러스터 갯수 확인 2
nb=NbClust(my_data,min.nc = 2,max.nc = 6,method = "kmeans")
#opt.nb 시각화
fviz_nbclust(nb, ggtheme = theme_minimal())
opt.nb=which.max(table(nb$Best.nc[1,]))

km.res <- kmeans(my_data, opt.nb, nstart = 25)
# Visualize

fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

####################
###pam 클러스터링###
####################
pam.res <- pam(my_data, opt.nb)
# Visualize
fviz_cluster(pam.res,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#####################
###계층 클러스터링###
#####################
#거리를 사용한 계층 클러스터링
res.hc <- USArrests %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2") 

#덴드로그램
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

#####################
### 실루엣 시각화 ###
#####################
# 실루엣 평균 값이 높은것이 좋음
# Enhanced hierarchical clustering, cut in 3 groups
res.hc <- USArrests %>%
  scale() %>%
  eclust("hclust", k = 2, graph = FALSE)

# Visualize with factoextra
fviz_dend(res.hc, palette = "jco",
          rect = TRUE, show_labels = TRUE)
fviz_silhouette(res.hc)
