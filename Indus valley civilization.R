

library(rvest)
library(tidyverse)
html <- read_html("https://fineartamerica.com/art/paintings/indus+valley")
name <- html_elements(html ,".flowArtworkName" )
name <- html_text(name )
name
head(name)



img <- html_elements(html , ".flowImage.lazy")
img_link <- html_attr(img , "data-src")
head(img_link)


## Data frame


Indus_valley_civilization <- data.frame(name , img_link)
view(Indus_valley_civilization)
head(Indus_valley_civilization)

save(Indus_valley_civilization , file = "Indus_valley_civilization.Rdata")