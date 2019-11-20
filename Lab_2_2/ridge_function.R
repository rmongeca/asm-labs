ridge <- function(x, y, lambdas){
    n <- nrow(x)
    p <- nrow(y)
    X <- scale(x, center = TRUE, scale = TRUE)
    Y <- scale(y, center = TRUE, scale = TRUE)
    n_lambdas <- length(lambdas)
    
    XtX <- t(X)%*%X
    # estimated coefficients path
    beta.path <- matrix(0,nrow=n.lambdas, ncol=p)
    diag.H.lambda <- matrix(0,nrow=n.lambdas, ncol=n)
    for (l in 1:n_lambdas){ 
        lambda <- lambda.v[l]
        H.lambda.aux <- t(solve(XtX + lambda*diag(1,p))) %*% t(X) 
        beta.path[l,] <-  H.lambda.aux %*% Y
        H.lambda <- X %*% H.lambda.aux 
        diag.H.lambda[l,] <- diag(H.lambda)
    }
    
    coeff <- as.matrix(beta.path)
    return(coeff)
}

ridge_lambda_search_cv <- function(
  x, y, lambda.v,
  cv=10,
  plot=TRUE
) {
  # Ensure x/y are matrix/vector
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if(!is.vector(y)) {
    y <- y[,,drop=TRUE]
  }
  
  # Create data.frame to store output
  if(!is.vector(lambda.v)) {
    lambda.v <- lambda.v[,,drop=TRUE]
  }
  n <- length(lambda.v)
  out <- data.frame(lambda=lambda.v, mspe=rep(0,n), df=rep(0,n))

  p <- ncol(x)

  # Create folds for CV
  ## a typical choice is cv=10
  folds <- sample(rep(1:cv, length=nrow(x)), nrow(x), replace=FALSE) 

  # Per lambda candidate compute coefficients, MSPE and df
  for(i in 1:n) {
    lambda <- lambda.v[i]
    
    # Create fold out data.frame
    out.cv <- data.frame(mspe=rep(0,cv), df=rep(0,cv))
    for(j in 1:cv) {
      # Get training and validation data for fold
      x.train <- x[folds!=j,]
      y.train <- y[folds!=j]
      x.valid <- x[folds==j,]
      y.valid <- y[folds==j]
      # Get x Singular Value Decomposition
      x.svd <- svd(x.train)
      d <- x.svd$d
      v <- x.svd$v
      # Compute (D^2 - lambda*Id)^-1
      d_inv <- diag(1/(d*d - lambda))
      # Compute (X^TX + lambda*Id)^-1
      xx_inv <- t( solve( t(x.train) %*% x.train + lambda*diag(1,p) ))
      # Compute beta 
      beta <- xx_inv %*% t(x.train) %*% y.train
      # Compute y prediciton y.hat
      y.hat <- x.valid %*% beta
      # Compute MSPE
      mspe <- sum((y.valid - y.hat)^2) / length(y.valid)
      # Compute df depending on the singular values
      df <- sum(d^2 / (d^2 +lambda))
      # Add results to output
      out.cv$mspe[j] <- mspe
      out.cv$df[j] <- df
    }
    # Add mean of mspe/df to out data.frame
    out$mspe[i] <- mean(out.cv$mspe)
    out$df[i] <- mean(out.cv$df)
    
  }
  
  if(plot) {
    # Plot mspe-log(lamba+1)
    plot(mspe~log(1+lambda), out, col=2)
    lambda.min <- out$lambda[which.min(out$mspe)]
    abline(v=log(1+lambda.min),col=2,lty=2)
    # Plot mspe-df
    plot(mspe~df, out, col=3)
    df.min <- out$df[which.min(out$mspe)]
    abline(v=df.min,col=3,lty=2)
  }
  
  return(out)
  
}

ridge_regression <- function(
  x.train,
  y.train,
  x.test,
  y.test,
  lambda
) {
  # Ensure x/y are matrix/vector
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if(!is.vector(y)) {
    y <- y[,,drop=TRUE]
  }

  p <- ncol(x)  
  # 1-row df to store performance measures
  performance <- data.frame(lambda=lambda, mspe=0, df=0)

  # Get x Singular Value Decomposition
  x.svd <- svd(x.train)
  d <- x.svd$d
  v <- x.svd$v
  # Compute (D^2 - lambda*Id)^-1
  d_inv <- diag(1/(d*d - lambda))
  # Compute (X^TX + lambda*Id)^-1
  xx_inv <- t( solve( t(x.train) %*% x.train + lambda*diag(1,p) ))
  # Compute beta 
  beta <- xx_inv %*% t(x.train) %*% y.train
  # Compute y prediciton y.hat
  y.hat <- x.test %*% beta
  # Compute MSPE
  mspe <- sum((y.test - y.hat)^2) / length(y.test)
  # Compute df depending on the singular values
  df <- sum(d^2 / (d^2 +lambda))
  # Add results to output
  performance$mspe[1] <- mspe
  performance$df[1] <- df

  output <- list()
  output$performance <- performance
  output$coefficients <- beta
  return(output)
  
}