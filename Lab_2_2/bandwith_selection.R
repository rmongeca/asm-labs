# bandwith_selection.R Computing of best bandiwth parameter for 
#             Local polynomial regression model (with locpolreg)
#            
# Input: 
#      x,y  Observed data (two (n,1) vectors)
#      h.v  Smoothing parameter candidates
#      q    degree of the local polynomial to be fitted (default: 1)
#      type.kernel "normal"  (Gaussian, default), 
#                  "epan"    (Epanechnikov) or 
#                  "rs.epan" (re-scaled Epanechnikov)
#                  "unif"    (Uniform Kernel in [-1,1])
#
# Output:  A list with three elements:
#      h.v  Smoothing parameter candidates
#      cv   LOOCV error estimate
#      gcv  GCV error estimate
#
# Use:
#      result <- h.cv.gcv(x,y,h.v,q,type.kernel)
# 

h.cv.gcv <- function(x,y,h.v = exp(seq(log(diff(range(x))/20),
                                       log(diff(range(x))/4),l=10)), 
                     q=1,type.kernel="normal") {
  n <- length(x)
  cv <- h.v*0
  gcv <- h.v*0
  for (i in (1:length(h.v))){
    h <- h.v[i]
    aux <- locpolreg(x=x,y=y,h=h,q=q,tg=x,
                     type.kernel=type.kernel, doing.plot=FALSE)
    S <- aux$S
    h.y <- aux$mtgr
    hii <- diag(S)
    av.hii <- mean(hii)
    cv[i] <- sum(((y-h.y)/(1-hii))^2)/n
    gcv[i] <- sum(((y-h.y)/(1-av.hii))^2)/n
  }
  return(list(h.v=h.v,cv=cv,gcv=gcv))
}

h.k.fold.cv <- function(x,y,h.v = exp(seq(log(diff(range(x))/20),
                                          log(diff(range(x))/4),l=10)), 
                        k=10,q=1,type.kernel="normal"){
  n <- length(x)
  perm <- sample(1:n)
  xperm <- x[perm]
  yperm <- y[perm]
  
  k.cv <- h.v*0
  for (i in (1:length(h.v))){
    h <- h.v[i]
    k.cv[i] <- k.fold.cv(x=xperm,y=yperm,k=k,h=h,q=q,
                         type.kernel=type.kernel)
  }
  return(list(k=k,h.v=h.v,k.cv=k.cv))
}