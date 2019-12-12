#
# logistic regression using our own IRWLS algorithm
#
p.from.beta <- function(beta,x){
  lin.term <- beta[1] + x %*% beta[-1]
  e.lt <- exp(lin.term)
  p <- e.lt/(1+e.lt)
  return(list(p=p,lt=lin.term))
}

logistic.IRWLS <- function(x,y,weights.out=1,x.new=x,
                           max.iter=10,eps.beta=1e-5,
                           plts=FALSE){
  if (plts&(dim(as.matrix(x))[2]>1)){
    plts<-FALSE
    warning("Plots are valid only when dim(x)[2]==1")
  }
  # Step 0
  stop.rule <- FALSE
  iter <- 0
  beta.0 <- coef(lm(y~x)) 
  
  while (!stop.rule){
    iter <- iter + 1 
    # step 1
    p.lt <- p.from.beta(beta.0,x)
    p <- p.lt$p
    lt <- p.lt$lt
    ps.e <- (y-p)/(p*(1-p))
    z <- lt + ps.e 
    wt <- p*(1-p) *weights.out
    
    if (plts){
      op<-par(mfrow=c(1,2))
      plot(x,y,cex=8*wt)
      lines(x,p,col=2)
      plot(x,z,cex=8*wt)
      lines(x,lt,col=2)
      par(op)
    }
    
    lm.1 <- lm(z~x,weights = wt) 
    beta.1 <- coef(lm.1)
    
    # checking stop rules
    if ((iter>=max.iter)|(sum((beta.1-beta.0)^2)<eps.beta)){
      stop.rule<-TRUE
    } else {
      beta.0 <- beta.1
    }
  }
  
  aux <- summary(lm.1)
  p.lt <- p.from.beta(beta.1,x)
  p <- p.lt$p
  lt <- p.lt$lt
  se.beta <- diag(aux$cov.unscaled)^.5
  null.dev <- sum(-2*dbinom(y,1,mean(y),log=TRUE))
  resid.devi <- sum(-2*dbinom(y,1,p,log=TRUE))
  
  return(list(coefficients=beta.1, se.coef=se.beta,
              fitted.values=p,linear.predictors=lt,
              predicted.values=p.from.beta(beta.1,x.new)$p,
              residual.deviance=resid.devi,
              iter=iter)
         )
} 


######

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

