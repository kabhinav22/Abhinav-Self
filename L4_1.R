#link  https://rpubs.com/tijahbus/L4_1

df <- USArrests
dim(df)

df <- na.omit(df)
df <- scale(df)

d <- dist(df, method = "euclidean")
hc <- hclust(d)
sub_grp <- cutree(hc, k = 10)
library(factoextra)


fviz_cluster(list(data = df, cluster = sub_grp))



library(ISLR)
data(Auto)
dim(Auto)

auto <- Auto[,1:7]
summary(auto)

new.data <- Auto[, 1:8]
new.data$origin <- as.character(new.data$origin)
summary(new.data)
### Practical Applications
pc <- prcomp(auto, scale=TRUE)
plot(pc)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))

library(ggfortify)
autoplot(pc, data=new.data, colour = "origin")





library(rgl)
# Create some dummy data
dat <- replicate(2, 1:3)
# Initialize the scene, no data plotted
plot3d(dat, type = 'n', xlim = c(-1, 8), ylim = c(-1, 8), zlim = c(-10, 20), xlab = '', ylab = '', zlab = '')
# Define the linear plane
planes3d(1, 1, 1, 0, col = 'red', alpha = 0.6)






