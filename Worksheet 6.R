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
name <- html_text(name)
name
name <- html %>% html_elements(".titleColumn a") %>% html_text()
name

# 3
(a)
html = html_elements(html, ".titleColumn a")
name

(b)
name = html_elements(html, ".secondaryInfo")
x = substring(name, 30, 33)
x
as.numeric(x)

(c)
name = html_elements(html, ".ratingColumn strong")
name
x = substring(name, 16, 18)
x

z
as.numeric(x)


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
