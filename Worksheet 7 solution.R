

library(tidyverse)
library(rvest)


html <- read_html("https://www.iitk.ac.in/math/faculty")
name <- html_elements(html, ".head3")
name
name <- html_elements(name, "a")
name
name <- html_text(name)
name
name <- html_elements(html, ".head3 a")
name
name <- html_text(name)
name


# 1

html = read_html("https://www.iitk.ac.in/math/visitors-post-doctoral-fellow")
name = html_elements(html, ".head2")
name
name <- html_text(name)
name


# 2
html <- read_html("https://www.imdb.com/chart/top/")
name = html_elements(html, ".titleColumn")
name
name <- html_elements(name, "a")
name
name <- html_text(name)
name
name <- html %>% html_elements(".titleColumn a") %>% html_text()
name

# 3
(a)
html = html_elements(html, ".titleColumn a")
name

htl = html_table(html)
htl

year <- html %>% 
  html_elements(".secondaryInfo") %>% 
  html_text() %>%
  substring(2,5) %>%  # removing brackets
  as.numeric()  # changing to number


(b)
name = html_elements(html, ".secondaryInfo")
name
name = html_text(name)
name
x = substring(name, 2, 5)
x
as.numeric(x)

(c)
name = html_elements(html, ".ratingColumn strong")
name
name = html_text(name)
name
as.numeric(name)


name = html_elements(html, ".ratingColumn.imdbRating")
name


(d)
name = html_elements(html, ".ratingColumn strong")
votes = html_attr(name,"title")
votes
y = substring(votes, 13, 21)
y
z = gsub(",","",y)
z



votes <- html %>% 
  html_elements(".ratingColumn strong") %>%
  html_attr("title") %>%
  substring(14) # removing the first 13 characters












library(rvest)

html <- read_html("https://www.imdb.com/title/tt0111161/ratings")
x = numeric(length = 250)
all_ratings <- html %>% html_table()
for (i in 1:250) 
{
  
  
}
all_ratings




html <- read_html("https://www.imdb.com/title/tt0111161/ratings")
all_ratings <- html %>% html_table()
length(all_ratings)
all_ratings[[1]] # First item in list
all_ratings[[2]] # Second item in list
all_ratings[[3]] 
dim(all_ratings[[1]])
x = all_ratings[[1]][,1]
x
y = all_ratings[[1]][,3]
y
paste(x , y)

all_ratings[[1]][,c(1,3)]

dim(all_ratings[[2]])





library(rvest)

# describe how many movies to scrape
# first make it work for a few, and then
# set nmov = 250
nmov <- 100

## Finding the codes of all the movies
html <- read_html("https://www.imdb.com/chart/top/")
codes <-  html %>% 
  html_elements(".seen-widget") %>%
  html_attr("data-titleid")
codes <- codes[1:nmov]

# name, year, rating, and votes is available 
# in the top 250 page
name <- html %>%  
  html_elements(".titleColumn a") %>% 
  html_text()
name <- name[1:nmov]

year <- html %>% 
  html_elements(".secondaryInfo") %>% 
  html_text() %>%
  substring(2,5) %>%  # removing brackets
  as.numeric()  # changing to number
year <- name[1:nmov]

# getting rating
rating <-  html %>% 
  html_elements(".ratingColumn.imdbRating") %>% 
  html_elements("strong") %>% 
  html_text() %>%
  as.numeric()  # convering to numeric
rating <- rating[1:nmov]

over.votes <- html %>% 
  html_elements(".ratingColumn strong") %>%
  html_attr("title") %>%
  substring(14) # removing the first 13 characters

over.votes <- substring(over.votes, 1, nchar(over.votes) - 13)
over.votes <- gsub(',', "", over.votes)%>%  
  as.numeric()
over.votes <- over.votes[1:nmov]

## Creating all URLS for 250 movies
urls <- paste("https://www.imdb.com/title/", codes, "/ratings", sep = "")

ranks <- 1:nmov
all_ratings <- list(length = nmov) # list of lists
for(i in 1:nmov)
{
  print(paste("Starting movie", i))
  movie_html <- read_html(urls[i])
  all_ratings[[i]] <- movie_html %>% html_table()
}

# all_ratings is a list where each element of 
# the list, is a list of all tables in the movie

# extracting number of votes for 1-10
votes <- matrix(0, ncol = 10, nrow = nmov)
colnames(votes) <- all_ratings[[1]][[1]]$Rating

rating_gen <- matrix(0, nrow = nmov, ncol = 2)
colnames(rating_gen) <-  c("men_rating", c("women_rating"))


votes_gen <- matrix(0, nrow = nmov, ncol = 2)
colnames(votes_gen) <-  c("men_votes", c("women_votes"))


for(i in 1:nmov)
{
  votes1to10 <- all_ratings[[i]][[1]]$Votes
  votes1to10 <- gsub(',', '',votes1to10) %>% as.numeric()
  votes[i, ] <- votes1to10
  
  sub <- all_ratings[[i]][[2]][, 1:2]
  sub_rating <- sub$`All Ages`
  sub_rating <- gsub('\n', '', sub_rating)
  sub_rating <- gsub(' ', '', sub_rating)
  
  gen1 <- substring(sub_rating, 1, 3) %>%
    as.numeric()
  rating_gen[i,] <- gen1[-1]
  
  gen2 <- substring(sub_rating, 4)
  gen2 <- gsub(',', '', gen2) %>%
    as.numeric()
  votes_gen[i, ] <- gen2[-1]
}

dat <- data.frame(ranks, name, year, 
                  rating, over.votes,
                  votes, rating_gen,
                  votes_gen)



##### Problem 2  ####

# URL of the images
urls_img <- paste("https://www.imdb.com/title/", codes, sep = "")
images <- character(length = nmov) # will store the vector of image locations
for(i in 1:nmov)
{
  print(paste("Starting movie", i))
  movie_html <- read_html(urls_img[i])
  images[i] <- movie_html %>% html_element(".ipc-image") %>% html_attr("src")
}


##### Problem 3  ####

# creating a function that calculates the distance from
# a given color. Code was in Worksheet 4 solutions
diff.col <- function(img, col)
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
  # return the mean distance from color
  return(mean(dist))
}


## Now calculating distance average distance black
library(imager)
black <- numeric(length = nmov)
for(i in 1:nmov)
{
  img <- load.image(images[i])
  black[i] <- diff.col(img, col = c(0,0,0))
}

# Which movies are within .2 distance to black on average
index <- which(black < .2)
dat[index, ]

