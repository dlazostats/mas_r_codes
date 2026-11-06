library(ggplot2)
library(FNN)
m<-50
x<-runif(m,0,2*pi)
e<-rnorm(m)
y<-2*sin(x)+e
plot(x,y)

df<-data.frame(x=x,y=y)
ggplot(df,aes(x=x,y=y))+
  geom_point()+
  geom_smooth(se=F)+
  theme_bw()

# KNN regression: predict on a grid of x values
x_grid  <- seq(0, 2*pi, length.out = 200)
knn_fit <- knn.reg(train = as.matrix(df$x),
                   test  = as.matrix(x_grid),
                   y     = df$y,
                   k     = 5)
pred_df <- data.frame(x = x_grid, y = knn_fit$pred)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(aes(color = "LOESS"), se = FALSE) +
  geom_line(data = pred_df, aes(color = "KNN (k = 5)"), linewidth = 1) +
  stat_function(fun = function(x) 2*sin(x),
                aes(color = "True: 2sin(x)"), linetype = "dashed") +
  labs(color = NULL) +
  theme_bw()
