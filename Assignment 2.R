image = load.image("/users/math/msc/kabhinav22/Downloads/images/campus.jpeg")
plot(image)
dim(image)
dims = dim(col.mat)

prop.color = function(img,col)
{
dist = matrix(0 , nrow = dims[1], ncol = dims[2])
    
    for (i in 1:dims[1]) 
      {
      for (j in 1:dims[2])
        {
dist[i,j] = norm(col.mat[i,j, ] - c(0,1,0), "2")
      if(mod(xij - c) < 0.5)
      {
        return(img)
      }
    else
    {
        return("image is not valid")
      }
      
      }
    }
}

  