library(rvest)
library(tidyverse)
html <- read_html("https://fineartamerica.com/art/paintings/babylonian?page=2")
name <- html_elements(html ,".flowArtworkName" )
name <- html_text(name )
name
head(name)



img <- html_elements(html , ".flowImage.lazy")
img_link <- html_attr(img , "data-src")
head(img_link)


## Data frame


Babylonian <- data.frame(name , img_link)
head(Babylonian)

save(Babylonian , file = "Babylonian.Rdata")
