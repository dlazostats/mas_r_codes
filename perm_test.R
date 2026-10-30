## Permutation test
#--------------------
# Example 1
#------------
sx <- c(-2, -1, -1, 0, -2, -1, 0, -1, -2, 100)
sy <- c(7, 13, 11, 5, 14, 9, 8, 10, 12, 11)
tst<-median(sx)-median(sy)
tst
# histogram of 10000 permutations
totsl<-c(sx,sy)
res<-list()
funperm<-function(){
  indx<-sample(1:length(totsl),10)
  sxr<-totsl[indx]
  syr<-totsl[-indx]
  return(median(sxr)-median(syr))
}
res<-replicate(10000,funperm())
hist(res,breaks="FD")
abline(v = tst, col = "red", lwd = 2, lty = 2)  # vertical line at x = 0
abline(v = -tst, col = "red", lwd = 2, lty = 2)  # vertical line at x = 0

# estimate the pvalue
# count the cases below and above the vertical lines
pval<-c(sum(res<=tst)+sum(res>=-tst))/length(res)
pval

# Example 2
#------------
sx <- c(-1.8, 0.4, 3.2, -2.3, -0.2, 0.3, 1.4, -0.5, 4.0, -0.3)
sy <- c(0.2, 0.5, -0.2, -0.5, 0.9, -1.2, 0.4, 0.0, 0.5, 0.2, 1.0, -0.6)
nx<-length(sx)
tst<-sd(sx)-sd(sy)
tstsam<-c(sx,sy)
fun_dsv<-function(){
  indx<-sample(1:length(tstsam),nx)
  sxr<-tstsam[indx]
  syr<-tstsam[-indx]
  return(sd(sxr)-sd(syr))
}
ressd<-replicate(10000,fun_dsv())
hist(ressd,breaks="FD")
abline(v = tst, col = "red", lwd = 2, lty = 2) 
abline(v = -tst, col = "red", lwd = 2, lty = 2) 
pval<-c(sum(ressd>=tst)+sum(ressd<=-tst))/length(ressd)
pval

