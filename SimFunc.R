##########################
# Simulation Function
##########################

#n <- 100
#corr_xe <- .3
#corr_xz <- .3
#corr_ze <- .3
#beta <- 1
#gamma1 <- .5
#gamma2 <- .5
#firststage <- "quadratic"
#k <- 100
#kfunc <- "normal"
#bandwidth <- .5

SimFunc <- function(n,corr_xe,corr_xz,corr_ze,beta,gamma1=.5,gamma2=.5
                    ,exogenous=FALSE,firststage=c("linear"
                                                  ,"quadratic"
                                                  ,"cubic"
                                                  ,"logarithmic"
                                                  ,"exponential")
                    ,k=100,kfunc="normal",bandwidth=.5){
  if(!exogenous){
    ### SIMULATE TRUE DGP - SINGLE INSTRUMENT ###
    # variables (x,z,e) with variance-covariance structure
    Sigma <- matrix(c(1,corr_xz,corr_xe
                      ,corr_xz,1,corr_ze
                      ,corr_xe,corr_ze,1),ncol=3,byrow=T)
    vars <- MASS::mvrnorm(n=n, mu=c(0,0,0), Sigma=Sigma)
    xprime <- vars[,1]; z <- vars[,2]; e <- vars[,3]
    
    # first stage
    if(firststage=="linear") x <- xprime
    if(firststage=="quadratic") x <- xprime*gamma1 + z^2*gamma2
    if(firststage=="cubic") x <- xprime*gamma1 + z^3*gamma2
    if(firststage=="logarithmic") x <- xprime*gamma1 + log(abs(z))*gamma2
    if(firststage=="exponential") x <- xprime*gamma1 + exp(z)*gamma2
    
    # second stage
    y <- x*beta + 2*e
    
    ### ESTIMATION ###
    # OLS
    ols <- lm(y ~ x)
    beta_ols <- coef(ols)["x"]
    se_ols <- sqrt(diag(vcov(ols)))["x"]
    
    # 2SLS
    twoSLS <- AER::ivreg(y ~ x | z)
    beta_twoSLS <- coef(twoSLS)["x"]
    
    # kernel
    kernel_inst <- ksmooth(x=z, y=x, kernel=kfunc,bandwidth=bandwidth)$y
    beta_kernel <- coef(lm(y ~ kernel_inst))["kernel_inst"]
    # alternative: KernSmooth package
    
    # lowess
    lowess_inst <- lowess(x=z,y=x,f=bandwidth)$y
    lowess <- lm(y ~ lowess_inst)
    beta_lowess <- coef(lowess)["lowess_inst"]
    se_lowess <- sqrt(diag(vcov(lowess)))["lowess_inst"]
    # bootstrapped SEs
    SE2_beta <- betas <- NULL
    for(i in seq_len(k)){
      ind <- sample(seq_len(n),size=n,replace=TRUE)
      inst <- lowess(x=z[ind],y=x[ind],f=bandwidth)$y
      lowess <- lm(y[ind] ~ inst)
      betas[i] <- coef(lowess)["inst"]
      sigma2 <- as.numeric(crossprod(lowess$residuals)/(n-ncol(lowess$model)))
      SE2_beta[i] <- (sigma2 * solve(crossprod(cbind(1,inst))))["inst","inst"]
    }
    boot_se_lowess <- sqrt(mean(SE2_beta)+var(betas))
    
    # Kernel Regularized Least Squares (KRLS)
    krls_inst <- suppressWarnings(KRLS::krls(X=z,y=x,whichkernel="gaussian"
                                             ,print.level=0)$fitted)
    beta_krls <- coef(lm(y ~ krls_inst))["krls_inst"]
    
  } else {
    
    ### SIMULATE TRUE DGP - INSTRUMENT & EXOGENOUS COVARIATE ###
    # coefficients of exogenous covariate w
    delta1 <- 1 # second stage
    
    # variables (x,z,w,e) with variance-covariance structure
    Sigma <- matrix(c(1,corr_xz,.2,corr_xe
                      ,corr_xz,1,.2,corr_ze
                      ,.2,.2,1,0
                      ,corr_xe,corr_ze,0,1),ncol=4,byrow=T)
    vars <- MASS::mvrnorm(n=n, mu=c(0,0,0,0), Sigma=Sigma)
    xprime <- vars[,1]; z <- vars[,2]; w <- vars[,3]; e <- vars[,4]
    
    # first stage
    if(firststage=="linear") x <- xprime
    if(firststage=="quadratic") x <- xprime*gamma1 + z^2*gamma2
    if(firststage=="cubic") x <- xprime*gamma1 + z^3*gamma2
    if(firststage=="logarithmic") x <- xprime*gamma1 + log(abs(z))*gamma2
    if(firststage=="exponential") x <- xprime*gamma1 + exp(z)*gamma2
    
    # second stage
    y <- x*beta + w*delta1 + 2*e
    
    ### ESTIMATION ###
    # OLS
    ols <- lm(y ~ x + w)
    beta_ols <- coef(ols)["x"]
    se_ols <- sqrt(diag(vcov(ols)))["x"]
    
    # 2SLS
    twoSLS <- AER::ivreg(y ~ x + w | z + w)
    beta_twoSLS <- coef(twoSLS)["x"]
    
    ### non-parametric first stages
    # 1. regress z on w and compute residuals epsilon
    epsilon <- lm(z ~ w)$residuals
    
    # 2. lowess, kernel, and KRLS of x on epsilon and predict values for x
    # kernel
    kernel_xhat <- ksmooth(y=x, x=epsilon, kernel=kfunc,bandwidth=bandwidth)$y
    
    # lowess
    lowess_xhat <- lowess(y=x,x=epsilon,f=bandwidth)$y
    
    # Kernel Regularized Least Squares (KRLS)
    krls_xhat <- suppressWarnings(KRLS::krls(y=x,X=epsilon
                                             ,whichkernel="gaussian",print.level=0)$fitted)
    
    # 3. regress y on predicted x and w
    # kernel
    beta_kernel <- coef(lm(y ~ kernel_xhat + w))["kernel_xhat"]
    
    # lowess
    lowess <- lm(y ~ lowess_xhat + w)
    beta_lowess <- coef(lowess)["lowess_xhat"]
    se_lowess <- sqrt(diag(vcov(lowess)))["lowess_xhat"]
    # bootstrapped SEs
    SE2_beta <- betas <- NULL
    for(i in seq_len(k)){
      ind <- sample(seq_len(n),size=n,replace=TRUE)
      xhat <- lowess(x=x[ind],y=epsilon[ind],f=bandwidth)$y
      lowess <- lm(y[ind] ~ xhat + w[ind])
      betas[i] <- coef(lowess)["xhat"]
      sigma2 <- as.numeric(crossprod(lowess$residuals)/(n-ncol(lowess$model)))
      SE2_beta[i] <- (sigma2 * solve(crossprod(cbind(1,xhat,w))))["xhat","xhat"]
    }
    boot_se_lowess <- sqrt(mean(SE2_beta)+var(betas))
    
    # Kernel Regularized Least Squares (KRLS)
    beta_krls <- coef(lm(y ~ krls_xhat + w))["krls_xhat"]
  }
  
  ### Output ###
  out <- data.frame(beta_ols=beta_ols
                    ,beta_twoSLS=beta_twoSLS
                    ,beta_kernel=beta_kernel
                    ,beta_lowess=beta_lowess
                    ,beta_krls
                    ,se_ols=se_ols
                    ,se_lowess=se_lowess
                    ,boot_se_lowess=boot_se_lowess
  )
  # return
  return(out)
}

