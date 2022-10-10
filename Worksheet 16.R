library(profvis)
profvis({
  attempts <- function(age)
  {
    count <- 0
    remain <- age # age no. of candles remain in the beginning
    while(remain > 0)
    {
      count <- count + 1
      # randomly choose any number between 1 and remain
      blow_out <- sample(1:remain, size = 1)
      remain <- remain - blow_out
    }
    return(count)
  }
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }
})


library(rbenchmark)
benchmark({
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }},
  replicate(1e3, attempts(25)), replications = 100)



att_vec <- NULL
for(i in 1:1e4)
{
  att_vec <- c(att_vec, attempts(25))
}





library(rbenchmark)
benchmark({
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }},
  {
    att_vec <- NULL
    for(i in 1:1e4)
    {
      att_vec <- c(att_vec, attempts(25))
    }}, 
  replicate(1e3, attempts(25)), replications = 100)



library(rbenchmark)
benchmark({
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }},
  {
    att_vec <- NULL
    for(i in 1:1e3)
    {
      att_vec <- c(att_vec, attempts(25))
    }}, 
  replicate(1e3, attempts(25)), replications = 100)








library(profvis)
profvis({
  attempts <- function(age)
  {
    count <- 0
    remain <- age # age no. of candles remain in the beginning
    while(remain > 0)
    {
      count <- count + 1
      # randomly choose any number between 1 and remain
      blow_out <- sample(1:remain, size = 1)
      remain <- remain - blow_out
    }
    return(count)
  }
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4)
  {
    att_vec[i] <- attempts(25)
  }
})


library(rbenchmark)
benchmark({
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4)
  {
    att_vec[i] <- attempts(25)
  }},
  {
    att_vec <- NULL
    for(i in 1:1e4)
    {
      att_vec <- c(att_vec, attempts(25))
    }}, 
  replicate(1e4, attempts(25)), replications = 100)