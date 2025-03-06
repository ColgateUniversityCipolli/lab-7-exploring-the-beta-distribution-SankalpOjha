library(tidyverse)
library(ggplot2)
library(patchwork)

calculate.data <- function(alpha, beta) {
  mean <- alpha/(alpha + beta)
  varience <- (alpha * beta) / (((alpha + beta)**2) * (alpha + beta + 1))
  skewness <- (2 * (beta - alpha) * ((alpha + beta + 1) ** (1/2))) / 
    ((alpha + beta + 2) * ((alpha * beta) ** (1/2)))
  kurt <- (6 * (((alpha + beta)**2) * (alpha + beta + 1) - (alpha * beta) * (alpha + beta + 2)) / 
             ((alpha * beta) * (alpha + beta + 2) * (alpha + beta + 3))) - 3
  
  data <- tibble(
    mean = mean,
    varience = varience,
    skewness = skewness,
    kurt = kurt
  )
  
  
  return(data)
}

make.plots <- function(alpha, beta) {
  beta.figure.data <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>
    mutate(beta.pdf = dbeta(x, alpha, beta))
  
  ggplot(data = beta.figure.data) +
    geom_line(aes(x=x, y=beta.pdf))
}

data1 <- calculate.data(2, 5)
plot.data1 <- make.plots(2, 5)
data2 <- calculate.data(5, 5)
plot.data2 <- make.plots(5, 5)
data3 <- calculate.data(5, 2)
plot.data3 <- make.plots(5, 2)
data4 <- calculate.data(0.5, 0.5)
plot.data4 <- make.plots(0.5, 0.5)

plot.data1 + plot.data2 + plot.data3 + plot.data4

all.data <- rbind(data1,
                  data2,
                  data3,
                  data4)

beta.moment <- function(alpha, beta, k, centered) {
  
  if(centered == FALSE){
    integrand = function(x) {(x**k) * dbeta(x, alpha, beta)}
    value <- integrate(integrand, lower = -Inf, upper = Inf)$value
  }else if(centered == TRUE){
    integrand = function(x) {(x) * dbeta(x, alpha, beta)}
    mu <- integrate(integrand, lower = -Inf, upper = Inf)$value
    integrand = function(x) {((x-mu)**k) * dbeta(x, alpha, beta)}
    value <- integrate(integrand, lower = -Inf, upper = Inf)$value
  }
  
  return(value)
}


beta.moment(2,5,3,FALSE)



set.seed(7272)
n <- 500

get.data <- function(alpha, beta, n) {
  beta.sample <- rbeta(n = n, # sample size shape1 = alpha, # alpha parameter
                     shape1 = alpha,
                     shape2 = beta) # beta parameter
}

sample1 <- data.frame(get.data(2,5,n))
sample2 <- data.frame(get.data(5,5,n))
sample3 <- data.frame(get.data(5,2,n))
sample4 <- data.frame(get.data(0.5,0.5,n))


