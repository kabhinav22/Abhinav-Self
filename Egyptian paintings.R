library(rvest)
library(stringr)
library(imager)
library(dplyr)


html <- read_html("https://www.wikiart.org/en/ancient-egyptian-painting/all-works/text-list")

code <- html %>% html_elements(".painting-list-text-row") %>% html_elements("a") %>% html_attr("href")
code

pic_links <- array(0)

for(i in 1:169){
  print(paste("starting",i))
  pic_links_web <- read_html(paste("https://www.wikiart.org/",code[i], sep = ""))
  
  pic_links[i] <- (pic_links_web %>% html_elements("img") %>% html_attr("src"))[2]
  
}

names <- html %>% html_elements(".painting-list-text-row a") %>% html_text()
names
years <- html %>% html_elements(".painting-list-text-row span") %>% html_text()
years



years <- as.numeric(gsub("-","",gsub(",","",gsub(" ","",years))))
years


egypt_data <- data.frame(Names = names,Year = years,Links = pic_links)
View(egypt_data)

egyplt_final_df <- egypt_data %>% filter(!row_number() %in% c(1:4,grep("Scarab",names)))
egyplt_final_df

egypt_col_analysis <- egypt_data %>% filter(!row_number() %in% c(1:4,30,31,33,58,61,120,grep("Scarab",names),
                                                                 grep("Mummy",names),grep("Coffin",names)))

egypt_col_analysis

save(egypt_col_analysis, file = "egypt_working_data.Rdata")
View(egypt_col_analysis)
attach(egypt_col_analysis)



######################################################################

diff.col = function(img,col.vec){
  img.mat <- as.array(img[,,1,])
  dist.mat <- matrix(0, nrow = dim(img.mat)[1], ncol = dim(img.mat)[2])
  for (i in 1:dim(img.mat)[1]) {
    for (j in 1:dim(img.mat)[2]) {
      dist.mat[i,j] <- norm(img.mat[i,j,] - col.vec, "2" )
    }
  }
  return(dist.mat)
}




prop.color <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  prop <- length(dist[which(dist <= 0.3, arr.ind = TRUE)])/(dims[1]*dims[2])
  
  # return the required proportion of pixel 
  return(prop)
}


## different colors used
## http://www.visual-arts-cork.com/artist-paints/egyptian-colour-palette.htm

##  https://www.worldhistory.org/article/999/color-in-ancient-egypt/  more pics

dist.brown1 <- diff.col(load.image(Links[29]),c(150,75,0)/255)
brown.coor1 <- which(dist.brown1 <= 0.3, arr.ind = TRUE)

dist.brown2 <- diff.col(load.image(Links[29]),c(204,51,51)/255)
brown.coor2 <- which(dist.brown2 <= 0.3, arr.ind = TRUE)

dist.brown3 <- diff.col(load.image(Links[29]),c(165,0,33)/255)
brown.coor3 <- which(dist.brown3 <= 0.3, arr.ind = TRUE)

dist.brown4 <- diff.col(load.image(Links[29]),c(204,0,51)/255)
brown.coor4 <- which(dist.brown4 <= 0.3, arr.ind = TRUE)

brown.coor <- unique(rbind(brown.coor1, brown.coor2, brown.coor3, brown.coor4))

brown.prop <- nrow(brown.coor)/(dim(load.image(Links[29]))[1] * dim(load.image(Links[29]))[2])


plot(load.image(Links[29]))
points(brown.coor[,1],brown.coor[,2], col = "red", cex = 0.1)