# 1

odd = numeric(length = 1000)

j = 1
for (i in 1:2000)
  {
  if(i%%2 != 0)
   { odd[j] = i
  j = j + 1
  }
}
odd


# 2

fib = numeric(length = 500)
fib[1] = 1
fib[2] = 1

j = 2
while(fib[500] == 0)
{
fib[j+1] = fib[j] + fib[j-1]
j = j + 1
print(fib)
}
  

# 3

Ram = function()
{
  num = sample(1:6,size = 1)
  print(num)
  if (num %% 2 == 0)
   return = 1
  else
    return = "the number is not even again roll the die"
  
}
print(Ram())
  
# 4

Sita = function()
{
  coin = rbinom(1 , 15, 0.5)
  print(coin)
  if (coin <= 8)
    return = "lose"
  else
    return = "win"
  
}
print(Sita())

# 5
A = matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),nrow = 5,ncol = 5)
A

mat = matrix(0,ncol = 5, nrow = 5)
for(i in 1:5)
  for (j in 1:5)
    {
    mat[i,j] = 1
    
  }
mat


# 6
mat = matrix(0,ncol = 5, nrow = 5)
for (i in 1:5)
  {
  for (j in 1:5)
    {
    if(i == j)
      {
      mat[i,j] = i

      
    }
    
  }
  
}
mat  

# 7

v = numeric(length = 100)

mat = matrix(0,ncol = 10, nrow = 10)

for (i in 1:10)
{
  for (j in 1:10){
    mat[i,j] = sample(1:6,size=1)
  }
    
}
mat



mat2 =matrix(sample(1:6, size=100,replace=TRUE),nrow=10)
mat2


v = numeric(length = 100)
for (i in 1:100)
  {
  v[i] = sample(1:6,size = 1)
}
mat3 = matrix(v,ncol = 10 )
mat3


# 8

input = function(n,rho){
  
mat = matrix(0,ncol = n, nrow = n)
for (i in 1:n)
{
  for (j in 1:n)
  {
    if(i == j)
    {
      mat[i,j] = 1
    }else {
      mat[i,j]= rho
    }
    
  }
}
return(mat)
}

input(5,2)

#9
input = function(n,rho)
  {
  mat = matrix(0,ncol = n, nrow = n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      
        mat[i,j] = rho^abs(i-j)
      
      
    }
  }
  return(mat)
}

mat5 = input(5,2)
mat5
dims=dim(mat5)


#10

newmat=  (0 ,nrow=dims[1] ,ncol=(dims[2]+1)/2
        
funct = function(matrix)
{
  dim(matrix)
  dims = dim(matrix)
  row = dims[1]
if(dims[2]%%2 == 0)
{
  col = dims[2]/2
}  else  {
      col = ((dims[2]+1)/2)
  }
  new = matrix(nrow = row,ncol = col)
  j = 1
  for (i in 1:dims[2])
    {
    if(i%%2 != 0)    {
      new[,j] = matrix[,i]
      j = j+1
    }
    
  }
  return(new)
}

  
funct(mat5)
  

#  11

 arr = array(dim = c(10,4,6,5))
for (i  in 1:10)
  {
  for (j in 1:4) 
    {
    for (k in 1:6)
      {
      for (l in 1:5)
        {
        arr[i,j,k,l] = 1
      }
      
    }
    
  }
  
}
 arr





# Workshrrt based questions

# 1

area = function(r)
{
  area1 = 22/7*r^2
  return(area1)
}
 area(5)


# 2

x = function(x1,x2)
{
  max = max(x1,x2)
  return(max)
}
 x ( 5,78 )



# 3

func = function()
{
  ra = sample(1:6, size = 1000,replace = TRUE)
  
  count = 0
  for (i in 1:1000)
    {
    if(ra[i]%%2 == 0)
    {
      count = count + 1
    }
  }
  return(count)
}

# 4

func1 = function()
{
  num = runif(n = 1000,min = 0, max = 1)
  count = 0
  for (i in 1:1000) 
    {
    if(num[i] > 0.1 & num[i] < 0.2)
    {
      count = count + 1
    }
   
  }
  return(count)
}
 n um
func1()


# 5

