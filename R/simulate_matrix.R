
#' Simulate OMEGA or SIGMA matrices
#'
#' @param n number of simulations
#' @param df degrees of freedom; must be a list of numeric values with same
#' length as `cov`
#' @param cov a list of numeric matrices
#' @param prefix added to numbered outputs; only used when `style` is `"simpar"`
#'
#' @details
#'
#' @return
#'
#' @example
#'
#' @export
simulate_matrix <- function(n, df, cov, prefix = "OM", style = c("simpar", "mrgsolve")) {
  stopifnot(is.list(cov))
  stopifnot(all(sapply(cov, inherits, "matrix")))
  stopifnot(length(df)==length(cov))
  stopifnot(all(sapply(df, is.numeric)))
  stopifnot(length(n)==1)
  stopifnot(is.numeric(n))
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

#' metrumrg::simblock replacement that handles diagonal omega or sigma
#'
#' @param n number of simulations; scalar
#' @param df degrees of freedom; scalar
#' @param cov a single matrix to simulate
sblock <- function(n,df,cov) {
  if(df < nrow(cov)) stop('df is less than matrix length')
  if(length(cov)==1) return(rinvchisq(n,df,cov))
  # Handle diagonal omega or sigma
  if(is_diagonal(cov)) {
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
