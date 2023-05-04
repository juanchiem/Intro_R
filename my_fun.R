cv <- function(x){
  sd(x)/mean(x)*100
}

cv_rounded <- function(x){
  cv0 <- sd(x)/mean(x)*100
  round(cv0, 1)
}

# raices_largo <- c(8.5, 9, 11)
# cv(raices_largo)
# cv_rounded(raices_largo)
