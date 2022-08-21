xufunction <- function(Y, Xk){
  # Christina Xu, 4/1/2, MA 575 Lab 08
  n <- length(Y)
  v1s <- rep(1,n)
  X <- cbind(v1s, Xk)
  S <- svd(t(X)%*%X)
  U <- S$u
  V <- S$v
  D <- diag(S$d)
  
  detX <- sqrt(abs(det(t(X)%*%X)))
  # if det=0, cols and rows are colinear
  kappa <- sqrt(max(S$d)/min(S$d))
  # condition large, model is unstable
  
  betahat <- V%*%solve(D)%*%t(U)%*%t(X)%*%Y
  H <- X%*%V%*%solve(D)%*%t(U)%*%t(X)
  lv <- diag(H)
  Yhat <- X%*%betahat
  
  resid <- Y - Yhat
  SSE <- sum((resid)^2)
  p <- dim(Xk)[2]
  SST <- sd(Y)^2*(n-1)
  SSM <- SST - SSE
  # SST is not always equal SSM and SSE
  
  MST <- SST/(n-1)
  MSE <- SSE/(n-p-1)
  MSM <- SSM/p
  
  SEbetahat <- sqrt(MSE)*sqrt(diag(V%*%solve(D)%*%t(U)))
  sresid <- resid/ (sqrt(MSE)*sqrt(1-lv))
  r2 <- 1 - SSE/SST
  r2adj <- 1 - MSE/MST
  
  Fstat <- MSM/MSE
  pval <- pf(Fstat, p, n-p-1, lower.tail = F)
  
  
  result <- list('predicted'= Yhat,
                 'residual' = resid,
                 'std. residual' = resid,
                 'condtion' = kappa,
                 'determinant' = detX,
                 'leverage' = lv,
                 'SSE' = SSE,
                 'SSM' = SSM,
                 'SST' = SST,
                 'MSE' = MSE,
                 'MSM' = MSM,
                 'MST' = MST,
                 'p-value' = pval,
                 'betahat' = betahat,
                 'SEbetahat' = SEbetahat,
                 'rsquared' = r2,
                 'rsquaredadj' = r2adj)
  return(result)}
