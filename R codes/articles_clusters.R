#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factorextra")
#install.packages("ggrepel")

library(tidyverse)  
library(cluster)    
library(factoextra) 
library(ggrepel)

############## IMPORT DATASET ######################

df <- read.csv('articles_data.csv', row.names='Reference')
df

############## SOLO DISTRIBUIDOS ############################################

###### CLUSTERS ############
df0 = df[-c(3,7,24,20), ]
df0

df0 = df0[-c(5,8,11,17,18), ]



df0

df0 = df0[-c(2,13,8,4,1,11), ]
df0
df0 = df0[-c(1,4,5), ]
df0

df0 <- scale(df0)
head(df0)

distance <- get_dist(df0)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df0, centers = 3, nstart = 25)
str(k2)
k2

fviz_cluster(k2, data = df0)


###### PRINCIPAL COMPONENTS #############

apply(X = df0, MARGIN = 2, FUN = mean)

pca <- prcomp(df0, scale = TRUE)
names(pca)

pca$center
pca$scale
pca$rotation
head(pca$x)
dim(pca$x)
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))


pca$rotation <- -pca$rotation
pca$x        <- -pca$x
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))



################################ TODOS ########################################

#### CLUSTERS #####
df1 <- scale(df)
head(df1)

distance1 <- get_dist(df1)
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k21 <- kmeans(df1, centers = 3, nstart = 25)
str(k21)
k21

fviz_cluster(k21, data = df1)



##### PRINCIPAL COMPONENTS #####

apply(X = df1, MARGIN = 2, FUN = mean)

pca1 <- prcomp(df1, scale = TRUE)
names(pca1)

pca1$center
pca1$scale
pca1$rotation
head(pca1$x)
dim(pca1$x)
biplot(x = pca1, scale = 0, cex = 0.6, col = c("blue4", "brown3"))


pca1$rotation <- -pca1$rotation
pca1$x        <- -pca1$x
biplot(x = pca1, scale = 0, cex = 0.6, col = c("blue4", "brown3"))



#################### TODOS CON ESCALA LOG10 ###############################

df2 <- log10(df)
df2

df2 <- scale(df2)
head(df2)

distance2 <- get_dist(df2)
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k22 <- kmeans(df2, centers = 4, nstart = 25)
str(k22)
k22

fviz_cluster(k22, data = df2, repel=TRUE)