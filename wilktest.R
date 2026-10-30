library(effectsize)
library(stats)
library(dplyr)
library(rstatix)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)
cohens_d(mpg ~ am, data = mtcars)
t.test(mpg ~ am, data = mtcars)

# More options:
cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
cohens_d(mpg ~ am, data = mtcars, mu = -5)
cohens_d(mpg ~ am, data = mtcars, alternative = "less")
hedges_g(mpg ~ am, data = mtcars)
glass_delta(mpg ~ am, data = mtcars)
interpret_r(-1.41,rules="cohen1988")

wilcox.test(mpg ~ am, data = mtcars)
wilcox_effsize(mpg ~ am, data = mtcars,ci=T)

dt1<-mtcars |> select(mpg,am)
dt2<-dt1 |> 
     mutate(rank= rank(mpg)) |> as.data.frame() |> arrange(am,rank)
dt3<-dt2 |> 
  group_by(am) |> 
  summarise(n = n(),
            R = sum(rank))
n1 <- dt3$n[1]
n2 <- dt3$n[2]
N
R1 <- dt3$R[1]
W  <- R1 - n1*(n1 + 1)/2
W
mu    <- n1 * n2 / 2
t_cnt <- table(dt1$mpg)                                   # sizes of tie groups
sigma <- sqrt((n1 * n2 / 12) *
                ((N + 1) - sum(t_cnt^3 - t_cnt) / (N * (N - 1))))

z_raw <- W - mu
z     <- (z_raw - sign(z_raw) * 0.5) / sigma             # continuity correction
p     <- 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE)) # two-sided
