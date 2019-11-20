# load libraries
library(glmnet)
library(caret)

# load data
boston <- load("boston.Rdata")
#
columns <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO",
             "B","LSTAT","CMEDV")
boston <- boston.c[,columns]
boston$CHAS <- as.numeric(boston$CHAS==1) # going wild here, don't know if this is correct

# casual check for NAs
sapply(boston,function(x){sum(which(is.na(x)))})

Y <- scale(boston$CMEDV, center = TRUE, scale = FALSE)
X <- scale(as.matrix(boston[,1:14]), center = TRUE, scale = TRUE)

lasso.2 <- glmnet(X,Y, standardize=FALSE, intercept=FALSE)
cv.lasso.2 <- cv.glmnet(X,Y, standardize=FALSE, intercept=TRUE) 

mean.Y <- mean(Y)
mean.X <- apply(boston[,1:14], 2, mean)
sd.X <- apply(boston[,1:14], 2, sd)

# intercept from the fitted model centering and scaling
mean.Y - sum((mean.X/sd.X) * coef(lasso.2,s=cv.lasso.2$lambda.1se)[-1])

# coefficients from the fitted model centering and scaling
coef(lasso.2,s=cv.lasso.2$lambda.1se)[-1] / sd.X

op <- par(mfrow=c(2,1))
plot(cv.lasso.2)
plot(lasso.2,xvar="lambda")
abline(v=log(cv.lasso.2$lambda.min),col=2,lty=2)
abline(v=log(cv.lasso.2$lambda.1se),col=2,lty=2)
print(coef(lasso.2,s=cv.lasso.2$lambda.min))
print(coef(lasso.2,s=cv.lasso.2$lambda.1se))
par(op)


# prediction in the validation set
# fix issues before continuing
