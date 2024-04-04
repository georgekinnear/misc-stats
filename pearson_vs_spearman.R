# https://psychometroscar.com/2018/09/04/the-spearman-correlation-doesnt-need-to-tell-you-anything-about-the-pearson-correlation/

# Neat little example showing that Spearman and Pearson correlations can be quite different!

N <- 1000000
Z <- rnorm(N, mean=0, sd=0.1)
X <- Z^201
Y <- exp(Z)

cor(X,Y, method="pearson")

cor(X,Y, method="spearman")

# Looking at it doesn't help much!
library(tidyverse)

tibble(x = X, y = Y) %>% 
  head(n = 10000) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_vline(xintercept = 0) +
  geom_point()


# This is a toned down example - fewer points, and a lower power (z^9 rather than z^201)
# (I also flipped x and y just to make the picture easier to see)
# Varying the power is quite instructive

dat <- tibble(z = Z) %>% 
  head(n = 100) %>% 
  mutate(y = z^9) %>% 
  mutate(x = exp(z))

dat %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

cor(dat$x, dat$y, method = "pearson")
cor(dat$x, dat$y, method = "spearman")


# It's possible to cook up a simpler, but less impressive, example

dat2 <- tibble(x = seq(from = -1, to = 1, by = 0.0001)) %>% 
  mutate(y = x^99)

cor(dat2$x, dat2$y, method = "pearson")
cor(dat2$x, dat2$y, method = "spearman")

dat2 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()
