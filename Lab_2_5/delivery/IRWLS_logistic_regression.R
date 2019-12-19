
p.from.theta <- function(theta.x){
  p.x <- 1/(1+exp(-theta.x))
  return(p.x)
}


logistic.IRWLS.splines <- function(x,y,weights.out=1,x.new=x,
                           df=6,spar=NULL, 
                           all.knots = FALSE, nknots = .nknots.smspl,  
                           max.iter=10,eps.beta=1e-5,
                           plts=FALSE){
  if (plts&(dim(as.matrix(x))[2]>1)){
    plts<-FALSE
    warning("Plots are valid only when dim(x)[2]==1")
  }
  # Step 0
  stop.rule <- FALSE
  iter <- 0
  theta.0 <- fitted(lm(y~x)) 
  
  while (!stop.rule){
    iter <- iter + 1 
    # step 1
    p <- p.from.theta(theta.0)
    ps.e <- (y-p)/(p*(1-p))
    z <- theta.0 + ps.e 
    wt <- p*(1-p) *weights.out
    
    if (plts){
      op<-par(mfrow=c(1,2))
      plot(x,y,cex=8*wt)
      lines(x,p,col=2)
      plot(x,z,cex=8*wt)
      lines(x,theta.0,col=2)
      par(op)
    }
    
    spline.1 <- smooth.spline(x,z,w=wt,df=df,spar=spar,
                              all.knots = all.knots,
                              nknots = nknots) 
    theta.1 <- predict(spline.1,x=x)$y
    
    # checkin stop rules
    if ((iter>=max.iter)|(sum((theta.1-theta.0)^2)<eps.beta)){
      stop.rule<-TRUE
    } else {
      theta.0 <- theta.1
    }
  }
  
  p <- p.from.theta(theta.1)
  resid.devi <- sum(-2*dbinom(y,1,p,log=TRUE))
  
  return(list(fitted.values=p,
              theta.x=theta.1,
              df=spline.1$df,
              predicted.values=p.from.theta(predict(spline.1,x=x.new)$y),
              residual.deviance=resid.devi)
  )
} 

