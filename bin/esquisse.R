install.packages("esquisse")
library(esquisse)
library(tidyverse)

esquisser(iris)
dat <- rio::import(file.choose()) %>% janitor::clean_names()
str(dat)

dat <- dat %>% mutate_if(is.character, as.numeric) 
str(dat)
esquisser(dat)
