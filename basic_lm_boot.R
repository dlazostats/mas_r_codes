R<-10000
lcof<-list()
lmm<-lm(mpg~wt+disp+cyl,data=mtcars)
for(i in 1:R){
  fitted<-predict(lmm) |> as.vector()
  res<-residuals(lmm) |> as.vector()
  resbot<-sample(res,size=length(res),replace=T)
  ybot<-fitted+resbot
  mtcarsboot<-mtcars |> mutate(mpg=ybot)
  rfit<-lm(mpg~wt+disp+cyl,data=mtcarsboot)  
  lcof[[i]]<-coef(rfit)  |> as.data.frame() |> t()
}
dfcod<-do.call("rbind",lcof)
rownames(dfcod)<-NULL
dfcodm<-dfcod |> as.data.frame()
hist(dfcodm$`(Intercept)`,breaks="FD")
hist(dfcodm$wt,breaks="FD")
hist(dfcodm$disp,breaks="FD")
hist(dfcodm$cyl,breaks="FD")
quantile(dfcodm$`(Intercept)`,probs = c(0.025,0.975))
quantile(dfcodm$wt,probs = c(0.025,0.975))
quantile(dfcodm$disp,probs = c(0.025,0.975))
quantile(dfcodm$cyl,probs = c(0.025,0.975))

## Wild bootstrap
R <- 10000
lcof <- vector("list", R)
lmm <- lm(mpg ~ wt + disp + cyl, data = mtcars)
fitted <- fitted(lmm)
res <- residuals(lmm)
for(i in 1:R){
  v <- sample(c(-1, 1),
              size = length(res),
              replace = TRUE)
  mpgboot <- fitted + res*v
  lmboot <- lm(mpgboot ~ wt + disp + cyl,
               data = mtcars)
  lcof[[i]] <- coef(lmboot)
}
coef_boot <- do.call(rbind, lcof)
apply(coef_boot, 2, mean)
apply(coef_boot, 2, sd)
dcoef_boot<-coef_boot |> as.data.frame()
hist(dcoef_boot$`(Intercept)`,breaks="FD")
hist(dcoef_boot$disp,breaks="FD")
