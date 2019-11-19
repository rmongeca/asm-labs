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