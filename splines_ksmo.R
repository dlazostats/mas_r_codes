library(earth)
library(rpart)
library(SoftBart)
library(Cubist)
library(ranger)
X = runif(200, min=0, max=4*pi)
Y = sin(X) + rnorm(200, sd=0.3)
data0_reg = ksmooth(x=X,y=Y,kernel = "normal")
plot(X,Y)
dfrm<-data.frame(x=X,y=Y)
lines(data0_reg, lwd=3, col="limegreen")
ss<-smooth.spline(X,Y)

plot(X,Y)
lines(data0_reg, lwd=3, col="limegreen")
lines(ss, lwd=3, col="blue")

marsfit<-earth(y~.,data=data0_reg)
xgrid <- seq(min(X), max(X), length.out = 500)
pred  <- predict(marsfit, newdata = data.frame(x = xgrid))
summary(marsfit)


plot(X,Y)
lines(data0_reg, lwd=3, col="limegreen")
lines(ss, lwd=3, col="blue")
lines(xgrid, pred, lwd = 3, col = "red")

plot(X,Y)
lines(data0_reg, lwd=3, col="limegreen")
lines(ss, lwd=3, col="blue")

plot(X,Y)
lines(data0_reg, lwd=3, col="limegreen")
lines(xgrid, pred, lwd = 3, col = "red")

marsfit<-earth(y~.,data=dfrm)
xgrid <- seq(min(X), max(X), length.out = 500)
pred  <- predict(marsfit, newdata = data.frame(x = xgrid))

tr1<-rpart(y~.,data=dfrm)
predtree<-predict(tr1, newdata = data.frame(x = xgrid))

plot(X,Y)
lines(xgrid, pred, lwd = 3, col = "red")
lines(xgrid,predtree, lwd = 3, col = "blue")

tr1<-rpart(y~.,data=data0_reg)
predtree<-predict(tr1, newdata = data.frame(x = xgrid))
cub<-cubist(x=X |> as.data.frame(),y=Y)  
predcub<-predict(cub,newdata = data.frame(x = xgrid|> as.data.frame() |> rename("X"="xgrid")))

plot(X,Y)
lines(xgrid,predtree, lwd = 3, col = "blue")
lines(xgrid, predcub, lwd = 3, col = "red")

rf<-ranger(y~x,data=dfrm)
prerf<-predict(rf, newdata = data.frame(x = xgrid))

plot(X,Y)
lines(xgrid,predtree, lwd = 3, col = "blue")
lines(xgrid, prerf, lwd = 3, col = "red")


rf <- ranger(y ~ x, data = dfrm)
prerf <- predict(rf,  data = data.frame(x = xgrid))$predictions
plot(X, Y)
lines(xgrid, predtree,
      lwd = 3,
      col = "blue")
lines(xgrid, prerf,
      lwd = 3,
      col = "red")


sig <- sqrt(rf$prediction.error)   # OOB MSE -> residual SD

set.seed(1)
sb <- softbart(
  X      = X_train,
  Y      = dfrm$y,
  X_test = X_grid,
  hypers = Hypers(X_train, dfrm$y, sigma_hat = sig, num_tree = 50),
  opts   = Opts(num_burn = 2500, num_save = 2500)
)

presb <- sb$y_hat_test_mean