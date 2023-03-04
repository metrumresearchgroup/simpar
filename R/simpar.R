
#' @importFrom MASS mvrnorm
#' @importFrom stats rchisq rgamma rnorm
NULL

glue <- function(...) {
  paste0(...)
  #function(...,sep='',collapse=NULL)paste(...,sep=sep,collapse=NULL)
}

#' Create Parameters for Simulation with Uncertainty
#'
#' @param nsim scalar numeric specifying the number of sets to attempt
#' @param theta vector of point estimates of fixed effect parameters
#' @param covar variance-covariance matrix for fixed effect parameters
#' @param omega list of variance-covariance matrices for first level random effects
#' @param sigma list of variance-covariance matrices for second level random effects
#' @param odf vector of omega degrees of freedom, one per matrix
#' @param sdf vector of sigma degrees of freedom, one per matrix
#' @param digits number of significant digits to include in output
#' @param min lower limit for parameter estimates
#' @param max upper limit for parameter estimates
#'
#' @details
#' If min or max are non-default (see below), you may want to set nsim
#' marginally higher to allow for dropped sets. covar is coerced to matrix
#' using as.matrix.
#'
#' If omega and sigma are not lists, they are coerced using list. Then each
#' element is coerced using as.matrix.
#'
#' By default, each element in odf and sdf will be the length (number of
#' elements) in the corresponding matrix.
#'
#' `min` and `max` may be given as scalar values, in which case they apply to
#' all parameters (as do the defaults). Alternatively, the first n limits may
#' be specified as a vector, in which case the remaining (if any) will be the
#' default. If any simulated parameter does not fall between its limits, inclusive,
#' the entire parameter set (row) is dropped from the result, with warning.
#'
#' @return
#' matrix, with column names indicating parameters, and row names indicating
#' set number before filtering by min and max.
#'
#' @examples
#'
#'set.seed(100)
#'simpar(
#'  nsim=10,
#'  theta=c(13,75,1),
#'  covar=matrix(c(10,7,2,7,30,1,2,1,0.5),ncol=3,nrow=3),
#'  omega=list(
#'    0.1,
#'    matrix(c(0.04,0.02,0.02,0.04),ncol=2,nrow=2)
#'  ),
#'  odf=c(50,20),
#'  sigma=list(0.04,1),
#'  sdf=c(99,99),
#'  min=rep(0,3),
#'  max=rep(90,3)
#')
#'simpar(
#'  nsim=1,
#'  theta=c(13,75,1),
#'  covar=matrix(c(10,7,2,7,30,1,2,1,0.5),ncol=3,nrow=3),
#'  omega=list(
#'    0.1,
#'    matrix(c(0.04,0.02,0.02,0.04),ncol=2,nrow=2)
#'  ),
#'  odf=c(50,20),
#'  sigma=list(0.04,1),
#'  sdf=c(99,99),
#'  min=rep(0,3),
#'  max=rep(90,3)
#')
#'simpar(
#'  nsim=1,
#'  theta=c(13,75,1),
#'  covar=matrix(c(10,7,2,7,30,1,2,1,0.5),ncol=3,nrow=3),
#'  omega=list(
#'    0.1,
#'    matrix(c(0.04,0.02,0.02,0.04),ncol=2,nrow=2)
#'  ),
#'  odf=c(50,20),
#'  sigma=list(0.04,1),
#'  sdf=c(99,99),
#'  min=Inf,
#'  max=-1
#')
#'
#' @export
simpar <- function(nsim,theta,covar,omega,sigma,odf=NULL,sdf=NULL,digits=4,min=-Inf,max=Inf){
  covar <- as.matrix(covar)
  if(ord(covar)!=length(theta))stop('order of covar is not equal to length theta')
  if(det(covar) < 0) stop("covar is not positive-definite.")
  if(!is.list(omega)) omega <- list(omega)
  if(!is.list(sigma)) sigma <- list(sigma)
  omega <- lapply(omega,as.matrix)
  sigma <- lapply(sigma,as.matrix)
  if(is.null(odf))odf <- sapply(omega,length)
  if(is.null(sdf))sdf <- sapply(sigma,length)
  npar <- length(theta) + sum(sapply(omega,function(x)length(half(x)))) + sum(sapply(sigma,function(x)length(half(x))))
  if(length(min)==1) min <- rep(min,npar)
  if(length(max)==1) max <- rep(max,npar)
  if(length(min)!=npar)min <- c(min, rep(-Inf,npar-length(min)))
  if(length(max)!=npar)max <- c(max, rep( Inf,npar-length(max)))
  if(length(max)!=npar)stop('length of max should be one, or number of parameters')
  if(length(min)!=npar)stop('length of min should be one, or number of parameters')
  if(!all(sapply(omega,is.square)))stop('not all omega blocks are square')
  if(!all(sapply(sigma,is.square)))stop('not all sigma blocks are square')
  if( any(sapply(omega,function(x)det(x)<0)))stop('not all omega blocks are positive-definite')
  if( any(sapply(sigma,function(x)det(x)<0)))stop('not all sigma blocks are positive-definite')
  if(length(odf)!=length(omega))stop('length odf differs from length omega')
  if(length(sdf)!=length(sigma))stop('length sdf differs from length sigma')
  if(any(odf < sapply(omega,nrow)))stop('odf[n] is less than number of rows in corresponding matrix')
  if(any(sdf < sapply(sigma,nrow)))stop('sdf[n] is less than number of rows in corresponding matrix')
  mvr <- mvrnorm(nsim,theta,covar)
  if(nsim==1)mvr <- t(as.matrix(mvr))
  # replace 120-130 using simulate_matrix()
  omg <- lapply(1:length(odf),function(x)list(n=nsim,df=odf[[x]],cov=omega[[x]]))
  sig <- lapply(1:length(sdf),function(x)list(n=nsim,df=sdf[[x]],cov=sigma[[x]]))
  omg <- do.call(cbind,lapply(omg,function(x)do.call(sblock,x)))
  sig <- do.call(cbind,lapply(sig,function(x)do.call(sblock,x)))
  dimnames(mvr) <- dimnames(omg) <- dimnames(sig) <- list(NULL,NULL)
  dimnames(mvr)[[2]] <- paste('TH',seq(length.out=dim(mvr)[[2]]),sep='.')
  dimnames(mvr)[[1]] <- seq(length.out=dim(mvr)[[1]])
  dimnames(omg)[[2]] <- glue('OM',impliedNames(sapply(omega,ord)))
  dimnames(omg)[[1]] <- seq(length.out=dim(omg)[[1]])
  dimnames(sig)[[2]] <- glue('SG',impliedNames(sapply(sigma,ord)))
  dimnames(sig)[[1]] <- seq(length.out=dim(sig)[[1]])

  sim <- cbind(mvr,omg,sig)
  sim <- round(signif(sim,digits),6)
  sim <- sim[apply(t(t(sim)-min)>=0,MARGIN=1,all),,drop=FALSE]
  if(dim(sim)[[1]]==0){
    warning(nsim,' of ',nsim,' rows dropped',call.=FALSE,immediate.=TRUE)
    return(sim)
  }
  sim <- sim[apply(t(t(sim)-max)<=0,MARGIN=1,all),,drop=FALSE]
  loss <- nsim - nrow(sim)
  if(loss)warning(loss,' of ',nsim,' rows dropped',call.=FALSE,immediate.=TRUE)
  sim
}

#' Draw sample from a inverse wishart distribution
#'
#' @param s pending
#' @param df degrees of freedom; passed to rgamma
#' @param prec pending
#'
#' @noRd
#'
riwish <- function(s,df,prec){
  if (df<=0) stop ("Inverse Wishart algorithm requires df>0")
  R <- diag(sqrt(2*rgamma(s,(df + s  - 1:s)/2)))
  R[outer(1:s, 1:s,  "<")] <- rnorm (s*(s-1)/2)
  S <- t(solve(R))%*% chol(prec)
  t(S)%*%S
}

#' Implement riwish for a diagonal matrix
#' When we find that all off diagonal elements are 0, we will simulate
#' as a series of independent invchisq simulations
#'
#' @param n number of simulations to perform; passed to rinvchisq
#' @param df degrees of freedom; passed to rinvchisq
#' @param cov the matrix to simulate
#'
#' @noRd
riwish_diag <- function(n, df, cov) {
  # implement riwish for a diagonal matrix
  # this is a series of calls to metrumrg::rinvchisq
  x <- cov[upper.tri(cov, diag = TRUE)]
  lapply(x, function(xx) rinvchisq(n, df, xx))
}

#' Draw samples from a inverse chi-square distribution
#'
#' @param n number of simulations to perform; passed to rchisq
#' @param df degrees of freedom; passed to rchisq
#' @param cov the matrix to simulate
#'
#' @noRd
rinvchisq <- function(n,df,cov) df*as.vector(cov)/rchisq(n, df)

#' Handle diagonal omega or sigma as metrumrg
#'
#' @param n number of simulations to perform
#' @param df degrees of freedom
#' @param cov the matrix to simulate
#' @param diagnal TRUE or FALSE; whether cov is a diagnal matrix or not
#'
#' @noRd
simblock <- function(n,df,cov,diagnal = FALSE) {
  diagnal = isTRUE(diagnal)
  if(df < nrow(cov)) stop('df is less than matrix length')
  if(length(cov)==1) return(rinvchisq(n,df,cov))
  # Handle diagonal omega or sigma
  if(is.diagonal(cov) && diagnal) {
    res <- do.call(cbind, riwish_diag(n, df, cov))
    return(res)
  }
  # Handle omega or sigma with off diagonal elements
  s <- dim(cov)[1]
  ncols <- s*(s+1)/2
  res <- matrix(nrow=n, ncol=ncols)
  for(i in 1:n)res[i,] <- half(posmat(riwish(s,df-s+1,df*cov)))
  res
}

#' metrumrg::simblock replacement that handles diagonal omega or sigma
#'
#' @param n number of simulations; scalar
#' @param df degrees of freedom; scalar
#' @param cov a single matrix to simulate
#'
#' @noRd
sblock <- function(n,df,cov) {
  if(df < nrow(cov)) stop('df is less than matrix length')
  if(length(cov)==1) return(rinvchisq(n,df,cov))
  # Handle diagonal omega or sigma
  if(is.diagonal(cov)) {
    res <- do.call(cbind, riwish_diag(n, df, cov))
    return(res)
  }
  # Handle omega or sigma with off diagonal elements
  s <- dim(cov)[1]
  ncols <- s*(s+1)/2
  res <- matrix(nrow=n, ncol=ncols)
  for(i in 1:n)res[i,] <- half(posmat(riwish(s,df-s+1,df*cov)))
  res
}

#' make positive definite matrix
#'
#' @param x matrix for calculation
#' @noRd
posmat <- function(x,...) {
  if(any(diag(x) <=0)) stop("matrix cannot be made positive-definite")
  if(!is.square(x))stop('x is not square')
  sign <- sign(x)
  x <- abs(x)
  characteristic <- trunc(log(x,10))
  mantissa <- log(x,10) - characteristic
  scale <- 10^characteristic
  digits <- 10^mantissa * 1e5
  diagonal <- round(diag(digits))
  digits <- floor(digits)
  diag(digits) <- diagonal
  digits <- digits/1e5
  x <- sign * scale * digits
  diagonal <- diag(x)
  y <- 0.97 * x
  diag(y) <- diagonal
  if(det(x)>0) x else posmat(y)
}

#' Internal metrumrg function to generate names for outputs
#'
#' @param x list of matrix for calculation
#' @noRd
impliedNames <- function(x){
  vars <- sum(x)
  crit <- cumsum(x)-x+1
  diag <- diag(rep(crit,x))
  good <- outer(colSums(diag),rowSums(diag),FUN='==')
  half <- half(good)
  names(half[half])
}

