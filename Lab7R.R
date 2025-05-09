library(tidyverse)
library(e1071)
library(cumstats)
library(ggplot2)
library(patchwork)

calculate.data <- function(alpha, beta) {
  mean <- alpha/(alpha + beta)
  varience <- (alpha * beta) / (((alpha + beta)**2) * (alpha + beta + 1))
  skewness <- (2 * (beta - alpha) * ((alpha + beta + 1) ** (1/2))) / 
    ((alpha + beta + 2) * ((alpha * beta) ** (1/2)))
  kurt <- (6 * (((alpha - beta)**2) * (alpha + beta + 1) - (alpha * beta) * (alpha + beta + 2)) / 
             ((alpha * beta) * (alpha + beta + 2) * (alpha + beta + 3)))
  
  data <- tibble(
    mean = mean,
    varience = varience,
    skewness = skewness,
    excess.kurtosis = kurt
  )
  
  
  return(data)
}

make.plots <- function(alpha, beta) {
  beta.figure.data <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>
    mutate(beta.pdf = dbeta(x, alpha, beta))
  
  ggplot(data = beta.figure.data) +
    geom_line(aes(x=x, y=beta.pdf))+
    ylab("Density")
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
    value <- integrate(integrand, lower = 0, upper = 1)$value
  }else if(centered == TRUE){
    integrand = function(x) {(x) * dbeta(x, alpha, beta)}
    mu <- integrate(integrand, lower = 0, upper = 1)$value
    integrand = function(x) {((x-mu)**k) * dbeta(x, alpha, beta)}
    value <- integrate(integrand, lower = 0, upper = 1)$value
  }
  
  return(value)
}

sample1 <- tibble(alpha = 2,
                  beta = 5, 
                  mean = beta.moment(2,5,1,F),
                  variance = beta.moment(2,5,2,T),
                  skewness = (beta.moment(2,5,3,T))/
                    ((beta.moment(2,5,2,T))^(3/2)),
                  excess.kurtosis = ((beta.moment(2,5,4,T))/
                                  ((beta.moment(2,5,2,T))^2))-3
)

# beta(5,5)
sample2 <- tibble(alpha = 5,
                  beta = 5, 
                  mean = beta.moment(5,5,1,F), 
                  variance = beta.moment(5,5,2,T),
                  skewness = (beta.moment(5,5,3,T))/
                    ((beta.moment(5,5,2,T))^(3/2)),
                  excess.kurtosis = ((beta.moment(5,5,4,T))/
                              ((beta.moment(5,5,2,T))^2))-3
)

# beta(5,2)
sample3 <- tibble(alpha = 5,
                  beta = 2, 
                  mean = beta.moment(5,2,1,F), 
                  variance = beta.moment(5,2,2,T),
                  skewness = (beta.moment(5,2,3,T))/
                    ((beta.moment(5,2,2,T))^(3/2)),
                  excess.kurtosis = ((beta.moment(5,2,4,T))/
                                  ((beta.moment(5,2,2,T))^2))-3
)

# beta(0.5,0.5)
sample4 <- tibble(alpha = 0.5,
                  beta = 0.5, 
                  mean = beta.moment(0.5,0.5,1,F), 
                  variance = beta.moment(0.5,0.5,2,T),
                  skewness = (beta.moment(0.5,0.5,3,T))/
                    ((beta.moment(0.5,0.5,2,T))^(3/2)),
                  excess.kurtosis = ((beta.moment(0.5,0.5,4,T))/
                                  ((beta.moment(0.5,0.5,2,T))^2))-3
)

histo.for.3 <- function(alpha, beta){
  set.seed(7272) 
  sample.size <- 500 
  
  fig.data <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   
    mutate(beta.pdf = dbeta(x, alpha, beta),                      
           normal.pdf = dnorm(x,                                    
                              mean = alpha/(alpha+beta),    
                              sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
  
  sample.beta <- rbeta(n = sample.size,  
                       shape1 = alpha,   
                       shape2 = beta) 
  
  beta.name = paste("Beta(", as.character(alpha), ", ", as.character(beta), ")",  sep = "")
  
  
  histogram <- ggplot(tibble(plot.data = sample.beta), aes(x=plot.data))+
    geom_histogram(aes(y=after_stat(density), color = "Sample Data Histogram"))+
    geom_density(aes(color = "Sample Data Density"), key_glyph = draw_key_path)+
    geom_line(data = fig.data, aes(x = x, y = beta.pdf, color = beta.name))+
    xlab("x")+
    ylab("Density")
  (histogram)
}

numsummary.for.3 <- function(alpha, beta){
  set.seed(7272) 
  sample.size <- 500 
  
  sample.beta <- rbeta(n = sample.size, 
                       shape1 = alpha,
                       shape2 = beta)
  
  summary = summarize(tibble(sample = sample.beta), 
                      mean = mean(sample),
                      variance = var(sample),
                      skewness = skewness(sample),
                      excess.kurtosis = kurtosis(sample) - 3)
  
  list(sample.beta, summary)
}

sample1.summary = numsummary.for.3(2,5)[[2]]
sample2.summary = numsummary.for.3(5,5)[[2]]
sample3.summary = numsummary.for.3(5,2)[[2]]
sample4.summary = numsummary.for.3(0.5,0.5)[[2]]

# histograms
sample1.histo = histo.for.3(2,5)
sample2.histo = histo.for.3(5,5)
sample3.histo = histo.for.3(5,2)
sample4.histo = histo.for.3(0.5,0.5)

# create histograms using patchwork
task3.histograms <- sample1.histo / sample2.histo | 
  sample3.histo / sample4.histo

(task3.histograms)

sample1.beta = numsummary.for.3(2,5)[[1]]

beta.cumstats.t <- tibble(n=(1:length(sample1.beta)), 
                          mean=cummean((sample1.beta)), 
                          variance=cumvar((sample1.beta)),
                          skewness=cumskew((sample1.beta)),
                          kurtosis=cumkurt((sample1.beta)))

# cum mean plot 
cum.mean <- ggplot(beta.cumstats.t)+
  geom_line(aes(x=n, y = mean))+
  geom_hline(data=data1, aes(yintercept = mean))  

# cum var plot
cum.var <- ggplot(beta.cumstats.t) +
  geom_line(aes(x=n, y = variance))+
  geom_hline(data=data1, aes(yintercept = variance))

# cum skewness plot
cum.skew <- ggplot(beta.cumstats.t) +
  geom_line(aes(x=n, y = skewness))+
  geom_hline(data=data1, aes(yintercept = skewness))

# cum kurtosis plot (not excess kurtosis)
cum.kurt <- ggplot(beta.cumstats.t) +
  geom_line(aes(x=n, y = kurtosis)) +
  geom_hline(data=data1, aes(yintercept = excess.kurtosis + 3))

# for loop
# sample size = 500
alpha <- 2
beta <- 5
sample.size <- 500

for (i in 2:50){
  set.seed(7272 + i)
  
  beta.sample.data <- rbeta(n = sample.size,  
                            shape1 = alpha,  
                            shape2 = beta)  
  
  beta.cumstats <- tibble(n=(1:length(beta.sample.data)), 
                          mean=cummean((beta.sample.data)), 
                          variance=cumvar((beta.sample.data)),
                          skewness=cumskew((beta.sample.data)), 
                          kurtosis=cumkurt((beta.sample.data)))
  
  # update plots
  cum.mean <- cum.mean + 
    geom_line(data = beta.cumstats, aes(x=n, y=mean), color = i)
 
  cum.var <- cum.var + 
    geom_line(data = beta.cumstats, aes(x=n, y=variance), color = i)

  cum.skew <- cum.skew + 
    geom_line(data = beta.cumstats, aes(x=n, y=skewness), color = i)

  cum.kurt <- cum.kurt + 
    geom_line(data = beta.cumstats, aes(x=n, y=kurtosis), color = i)
  
}

all.cum.plots <- cum.mean / cum.var | cum.skew / cum.kurt

(all.cum.plots)

alpha <- 2
beta <- 5
sample.size <- 500

stats = tibble(mean = numeric(),
               variance = numeric(),
               skewness = numeric(),
               kurtosis = numeric())

for (i in 1:1000){
  set.seed(7272 + i)
  sample.beta.5 <- rbeta(n = sample.size,  
                         shape1 = alpha,  
                         shape2 = beta)   
  
  mean=mean((sample.beta.5)) 
  variance=var((sample.beta.5))
  skewness=skewness((sample.beta.5))
  kurtosis=kurtosis((sample.beta.5))
  
  stats <- bind_rows(stats, tibble(mean, variance, skewness, kurtosis))
}

mean.distribution <- ggplot(stats)+
  geom_histogram(aes(x = mean, y=after_stat(density), 
                     color = "Mean Distribution"))+
  geom_density(aes(x=mean, color = "Mean Density"), 
               key_glyph = draw_key_path)
# histogram + density for variance
variance.distribution <- ggplot(stats)+
  geom_histogram(aes(x = variance, y=after_stat(density),
                     color = "Variance Distribution"))+
  geom_density(aes(x=variance, color = "Variance Density"), 
               key_glyph = draw_key_path)
# histogram + density for skewness
skewness.distribution <- ggplot(stats)+
  geom_histogram(aes(x = skewness, y=after_stat(density),
                     color = "Skewness Distribution"))+
  geom_density(aes(x=skewness, color = "Skewness Density"), 
               key_glyph = draw_key_path)
# histogram + density for kurtosis
kurtosis.distribution <- ggplot(stats)+
  geom_histogram(aes(x = kurtosis, y=after_stat(density),
                     color = "Kurtosis Distribution"))+
  geom_density(aes(x=kurtosis, color = "Kurtosis Density"), 
               key_glyph = draw_key_path)

# combine plots
stats.distribution <- mean.distribution / variance.distribution | skewness.distribution / kurtosis.distribution
(stats.distribution)