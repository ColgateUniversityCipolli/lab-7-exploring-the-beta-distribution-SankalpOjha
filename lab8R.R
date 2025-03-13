library(tidyverse)
library(nleqslv)

drates.csv <- read_csv("worldbankdata/deathrates.csv")


drates.2022 <- drates.csv |>
  mutate(rate = `2022`/1000) |>
  select(rate)

view(drates.2022)

mom.estimate <- function(data, par){
  alpha = par[1]
  beta = par[2]
  
  ex = alpha/(alpha + beta)
  ex2 = ((alpha + 1) * alpha)/((alpha + beta + 1) * (alpha + beta))
  
  m1 <- mean(data, na.rm = T)
  m2 <- mean(data^2, na.rm = T)
  
  return(c(ex - m1, ex2 - m2))
}

nleqslv(x = c(1, 1),
        fn = mom.estimate,
        data = drates.2022$rate)

mle.estimate <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik = sum(log(dbeta(x=data, shape1=alpha, shape2=beta)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}


optim(par = c(1, 1),
      fn = mle.estimate,
      data = drates.2022$rate,
      neg = T)

for(i in 1:1000){
  set.seed(7272+i)
  
}


summary <- tibble(
  bias = numeric(),
  precision = numeric(),
  MSE = numeric()
)





)