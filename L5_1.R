#link   https://rpubs.com/tijahbus/L5_1

df <- USArrests
df <- na.omit(df)
df <- scale(df)
# nstart is the number of random starting points
clusters <- kmeans(df, 4, nstart = 10)
library(factoextra)

fviz_cluster(clusters, df)


library(animation)
kmeans.ani(df, 4)

fviz_nbclust(df, kmeans, method = "wss")



library(ggplot2)
library(ggfortify)
library(gridExtra)
df <- USArrests
df <- na.omit(df)
df <- scale(df)
pc <- prcomp(df, scale=TRUE)
autoplot(pc, data=df)
pc$loadings
pc <- princomp(df)
pc$loadings



library(ISLR)
data(Hitters)
D <- na.omit(Hitters)
dim(Hitters)

library(pracma)
res <- odregress(as.matrix(D[,1:2]), as.matrix(D$Salary))
res$coeff

