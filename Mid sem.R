install.packages("tidyverse")
install.packages("rvest")
library(tidyverse)
library(rvest)


titanic = read_csv("https://github.com/dootika/midsem-exam-kabhinav22/blob/main/titanic.csv")
titanic
html = load("https://github.com/dootika/midsem-exam-kabhinav22/blob/main/titanic.csv")
html
tab = html_table(html)
tab
passengerid = tab[[1]]
survived = tab[[2]]
pclass = tab[[3]]
sex = tab[[4]]
age = tab[[5]]
sibsp = tab[[6]]
parch = tab[[7]]
fare = tab[[8]]
embarked = tab[[9]]

dat = data.frame(passengerid, survived, pclass, sex, age, sibsp, parch, fare, embarked)
save(dat , file = "titanic.Rdata")

  
finalP = ggplot(dat$passengerid ,aes(passengerid))+boxplot()

ggplot()








# 1


birthday = function(n)
{
  samebirthday = 0
  for (i in 1:n)
  {
    same = sample(n, size = 1 , replace = "TRUE", prob = (c(1)/365))
    samebirthday = samebirthday + same
  }
  return(samebirthday)
}
ux <- unique(samebirthday)




# b 

ans25 = numeric(length = 1000)
for (i in 1:1000) 
{
  ans25[i] = birthday()
}



#c

ans50 = numeric(length = 1000)
for (i in 1:1000) 
{
  ans25[i] = birthday(50)
}






install.packages("imager")
library(imager)
boat = load.image("/users/math/msc/kabhinav22/Downloads/images/boat.jpeg")
boat
dims = dim(boat)
dims
dims[1]
plot(boat)
col.mat = array(i,j, , )
col.mat
mat = matrix(0, nrow = dims[1], ncol = dims[2])
for (i in 1:dims[1])
{
  for (j in 1:dims[2])
  {
    diff.col = norm()
    
  }
  
}
