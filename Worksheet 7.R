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


