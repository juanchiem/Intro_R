# install.packages("esquisse")
library(esquisse)
esquisser(iris)


library(tidyverse)

dat <- rio::import(file.choose()) %>% janitor::clean_names()
str(dat)

dat <- dat %>% mutate_if(is.character, as.numeric) 
str(dat)
esquisser(dat)


ggplot(iris) +
  aes(x = Species, y = Sepal.Width) +
  geom_col(fill = "#112446") +
  theme_minimal()

ggplot(iris) +
  aes(x = Sepal.Width) +
  geom_histogram(bins = 31L, fill = "#461139") +
  theme_bw() +
  facet_wrap(vars(Species))

ggplot(iris) +
  aes(x = Petal.Length, y = Sepal.Width, colour = Species) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(legend.position = "top")
