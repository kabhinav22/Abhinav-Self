install.packages("ggplot2")
library(ggplot2)
load("IMDB_movies.Rdata")
ggplot(dat, aes(x = rating))
ggplot(dat, aes(x = rating)) +
  geom_histogram()
ggplot(dat, aes(x = rating)) +
  geom_boxplot()
ggplot(dat, aes(x = rating)) +
  geom_bar()
ggplot(dat, aes(x = year, y = over.votes)) +
  geom_point()
ggplot(dat, aes(x = year, y = over.votes)) +
  geom_point() +
  coord_cartesian(xlim = c(1996, 2025))
Year <- dat$year < 2000
Year <- as.factor(Year)
levels(Year) <- c("Before 2000", "After 2000")
ggplot(dat, aes(x = over.votes, y = rating)) +
  geom_point(aes(shape = Year, col = Year)) +
  labs(title = "Votes vs Rating", y = "Rating", x = "Number of Votes")
