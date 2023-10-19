
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
#' @param omega_diag logical; default is FALSE, if TRUE, simulated omega off-diagonal elements will be 0
#' @param sigma_diag logical; default is FALSE, if TRUE, simulated sigma off-diagonal elements will be 0
#' @param format default is "df", which will provide output as a data frame, if "list", the output will be provided as a list
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
simpar <- function(nsim,theta,covar,omega,sigma,odf=NULL,sdf=NULL,
                   digits=4,min=-Inf,max=Inf,
                   omega_diag=FALSE,sigma_diag=FALSE,format="df"){
  covar <- as.matrix(covar)

  # Error checks 1
  if(ord(covar)!=length(theta)){
    stop('order of covar is not equal to length theta', call. = FALSE)}

  if(det(covar) < 0){
    stop("covar is not positive-definite.", call. = FALSE)}

  if(!is.list(omega)) omega <- list(omega)
  if(!is.list(sigma)) sigma <- list(sigma)
  omega <- lapply(omega,as.matrix)
  omega_dim <- lapply(lapply(omega,as.matrix), nrow)
  sigma <- lapply(sigma,as.matrix)
  sigma_dim <- lapply(lapply(sigma,as.matrix), nrow)
  if(is.null(odf))odf <- sapply(omega,length)
  if(is.null(sdf))sdf <- sapply(sigma,length)

  # Error checks 2
  if(!all(sapply(omega,is.square))){
    stop('not all omega blocks are square', call. = FALSE)}

  if(!all(sapply(sigma,is.square))){
    stop('not all sigma blocks are square', call. = FALSE)}

  npar <- length(theta) +
    sum(sapply(omega,function(x)length(half(x)))) +
    sum(sapply(sigma,function(x)length(half(x))))
  if(length(min)==1) min <- rep(min,npar)
  if(length(max)==1) max <- rep(max,npar)
  if(length(min)!=npar)min <- c(min, rep(-Inf,npar-length(min)))
  if(length(max)!=npar)max <- c(max, rep( Inf,npar-length(max)))

  # Error checks 3
  ### not needed since the length of `min` and `max` was fixed upfront
  # if(length(max)!=npar){
  #   stop('length of max should be one, or number of parameters', call. = FALSE)}
  # if(length(min)!=npar){
  #   stop('length of min should be one, or number of parameters', call. = FALSE)}
  if( any(sapply(omega,function(x)det(x)<0))){
    stop('not all omega blocks are positive-definite', call. = FALSE)}
  if( any(sapply(sigma,function(x)det(x)<0))){
    stop('not all sigma blocks are positive-definite', call. = FALSE)}
  if(length(odf)!=length(omega)){
    stop('length odf differs from length omega', call. = FALSE)}
  if(length(sdf)!=length(sigma)){
    stop('length sdf differs from length sigma', call. = FALSE)}
  if(any(odf < sapply(omega,nrow))){
    stop('odf[n] is less than number of rows in corresponding matrix', call. = FALSE)}
  if(any(sdf < sapply(sigma,nrow))){
    stop('sdf[n] is less than number of rows in corresponding matrix', call. = FALSE)}

  # Simulate
  mvr <- mvrnorm(nsim,theta,covar)
  if(nsim==1)mvr <- t(as.matrix(mvr))
  omg <- lapply(1:length(odf),function(x)list(n=nsim,df=odf[[x]],
                                              cov=omega[[x]],diagonal=omega_diag))
  sig <- lapply(1:length(sdf),function(x)list(n=nsim,df=sdf[[x]],
                                              cov=sigma[[x]],diagonal=sigma_diag))
  omg <- do.call(cbind,lapply(omg,function(x)do.call(simblock,x)))
  sig <- do.call(cbind,lapply(sig,function(x)do.call(simblock,x)))
  dimnames(mvr) <- dimnames(omg) <- dimnames(sig) <- list(NULL,NULL)
  dimnames(mvr)[[2]] <- paste('TH',seq(length.out=dim(mvr)[[2]]),sep='.')
  dimnames(mvr)[[1]] <- seq(length.out=dim(mvr)[[1]])
  dimnames(omg)[[2]] <- glue('OM',impliedNames(sapply(omega,ord)))
  dimnames(omg)[[1]] <- seq(length.out=dim(omg)[[1]])
  dimnames(sig)[[2]] <- glue('SG',impliedNames(sapply(sigma,ord)))
  dimnames(sig)[[1]] <- seq(length.out=dim(sig)[[1]])

  # Output in metrumrg style
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

  # Output in mrgsolve style
  sim2 <- as.data.frame(sim)

  if (format == "list"){

    theta <- sim2[,grepl("TH",colnames(sim2))]
    names(theta) <- gsub("\\.", "", names(theta))
    names(theta) <- gsub("TH", "THETA", names(theta))
    theta <- split(theta, seq(nsim))
    theta <- unname(theta) # remove names

    omega <- trans_matrix(sim = sim2, nsim = nsim,
                          matrix_type = "OM", dim = omega_dim)
    sigma <- trans_matrix(sim = sim2, nsim = nsim,
                          matrix_type = "SG", dim = sigma_dim)

    xx <- list()
    xx_omega <- rep_len(list(), nsim)
    xx_sigma <- rep_len(list(), nsim)

    for (i in 1:nsim){

      for (j in 1:length(omega)){
        xx_omega[[i]][[j]] <- omega[[j]][[i]]
      }

      for (k in 1:length(sigma)){
        xx_sigma[[i]][[k]] <- sigma[[k]][[i]]
      }

      xx[[i]] <- list(theta[[i]], xx_omega[[i]], xx_sigma[[i]])

      names(xx[[i]]) <- c("param", "omega", "sigma")

    }

    sim <- xx
  }

  return(sim)
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
  if (df<=0){
    stop ("Inverse Wishart algorithm requires df>0", call. = FALSE)}
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
#' @param diagonal TRUE or FALSE; whether cov is a diagonal matrix or not
#'
#' @noRd
simblock <- function(n,df,cov,diagonal = FALSE) {
  diagonal = isTRUE(diagonal)
  if(df < nrow(cov)){
    stop('df is less than matrix length', call. = FALSE)}
  if(length(cov)==1) return(rinvchisq(n,df,cov))
  # Handle diagonal omega or sigma
  if(is.diagonal(cov) && diagonal) {
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
  if(any(diag(x) <=0)){
    stop("matrix cannot be made positive-definite", call. = FALSE)
  }
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

#' Convert a omega / sigma matrix from simpar format to a mrgsolve format
#'
#' @param sim simulated matrix in simpar style
#' @param nsim number of simulation
#' @param matrix_type type of matrix, either "OM" or "SG"
#' @param dim dimensions of matrix
#'
#' @noRd
trans_matrix <- function(sim, nsim, matrix_type, dim){

  matrix <- as.data.frame(sim[,grepl(matrix_type,colnames(sim))])

  dim2 <- lapply(dim, function(x) x*(x+1)/2)
  index <- lapply(cumsum(dim2), seq)

  xx <- list()
  for (i in 1:length(dim2)){
    xx[[i]] <- tail(index[[i]], n = dim2[[i]])
  }

  matrix <- lapply(xx, function(x) as.data.frame(matrix[,x]))

  matrix <- lapply(matrix, function(x) mrgsolve::as_bmat(x))


  # matrix <- lapply(seq(nsim), function(nn) {
  #   lapply(matrix, function(x) {
  #     x[[nn]]
  #   })
  # })

  return(matrix)

}

#' metrumrg::simblock replacement that handles diagonal omega or sigma
#'
#' @param n number of simulations; scalar
#' @param df degrees of freedom; scalar
#' @param cov a single matrix to simulate
#'
#' @noRd
sblock <- function(n,df,cov) {
  if(df < nrow(cov)){
    stop('df is less than matrix length', call. = FALSE)}
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

#' Simulate OMEGA or SIGMA matrices
#'
#' @param n number of simulations
#' @param df degrees of freedom; must be a list of numeric values with same
#' length as `cov`
#' @param cov a list of numeric matrices
#' @param prefix added to numbered outputs; only used when `style` is `"simpar"`
#'
#' @noRd
#'
simulate_matrix <- function(n, df, cov, prefix = "OM", style = "simpar") {

  # Error checks
  if (!is.list(cov)) stop("`cov` must be a list", call. = FALSE)
  if (!all(sapply(cov, inherits, "matrix"))){
    stop("Elements in `cov` must be matrices", call. = FALSE)}
  if (length(df)!=length(cov)){
    stop("Length of `df` must be the same as `cov`", call. = FALSE)}
  if (!all(sapply(df, is.numeric))){
    stop("Elements in `df` must be numeric", call. = FALSE)}
  if (length(n) != 1)stop("`n` must be a scalar", call. = FALSE)
  if (!is.numeric(n))stop("`n` must be numeric", call. = FALSE)

  # stopifnot(is.list(cov))
  # stopifnot(all(sapply(cov, inherits, "matrix")))
  # stopifnot(length(df)==length(cov))
  # stopifnot(all(sapply(df, is.numeric)))
  # stopifnot(length(n)==1)
  # stopifnot(is.numeric(n))

  style <- match.arg(style)
  omg <- lapply(seq_along(cov), function(x) list(n = n, df = df[[x]], cov = cov[[x]]))

  # simpar style output
  if(style=="simpar") {
    omg <- do.call(cbind,lapply(omg, function(x) do.call(sblock, x)))
    labels <- paste0(prefix, impliedNames(sapply(cov, ord)))
    dimnames(omg) <- list(NULL,labels)
    return(omg)
  }

  # mrgsolve style output
  if(style=="mrgsolve") {
    requireNamespace("mrgsolve")
    omg <- lapply(omg, function(x) do.call(sblock,x))
    omg <- lapply(omg, function(x) mrgsolve::as_bmat(as.data.frame(x)))
    omg <- lapply(seq(n), function(nn) {
      lapply(omg, function(x) {
        x[[nn]]
      })
    })
    return(omg)
  }

  stop("something bad happened")
}

#' Build a matrix
#'
#' @noRd

bmat <- function (..., context = NULL) {
  x <- list(...)
  x <- as.numeric(x)
  if (length(x) == 1)
    return(matrix(x, nrow = 1, ncol = 1))
  n <- 0.5 * (sqrt(1 - 4 * (-2 * length(x))) - 1)
  if (!n == as.integer(n)) {
    stop(paste0("Block matrix has invalid specification (",
                context, ")."), call. = FALSE)
  }
  mat <- diag(n)
  mat[upper.tri(mat, diag = TRUE)] <- x
  mat <- mat + t(mat) - diag(diag(mat))
  mat
}

