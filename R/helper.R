

is.square <- function(x,...)UseMethod('is.square')
#' Check if this is a square matrix
#' @param x matrix for testing
#' @noRd
#' @export
is.square.matrix <- function(x,...)dim(x)[[1]]==dim(x)[[2]]

#' Check if this is a diagonal matrix
#' @param matrix the object to check
#' @param eps tolerance for checking that off diagonals are zero
#' @noRd
#' @export
is.diagonal <- function(matrix, eps = 1e-12) {
  # check if this is a diagonal matrix
  off <- matrix[upper.tri(matrix)]
  all(abs(off)  < eps)
}

ord <- function(x,...)UseMethod('ord')
#' Calculate order of a square matrix
#' @param x matrix for testing
#' @noRd
#' @export
ord.matrix <- function(x,...){
  if(!is.square(x))stop('matrix is not square')
  dim(x)[[1]]
}

half <- function(x,...)UseMethod('half')
#' Calculate half matrix
#' @param x matrix for calculation
#' @noRd
#' @export
half.matrix <- function(x,...) {
  if(!isSymmetric(x))stop('matrix is not symmetric')
  d <- ord(x)
  dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
  dex <- dex[dex$col <= dex$row,]
  x <- x[as.matrix(dex)]
  names(x) <- do.call(paste,c(dex,list(sep='.')))
  structure(x,class='halfmatrix')
}

offdiag <- function(x,...)UseMethod('offdiag')
#' Extract off diagonal elements of a a half matrix
#' @param x half matrix for calculation
#' @noRd
#' @export
offdiag.halfmatrix <- function(x,...)x[sapply(strsplit(names(x),'\\.'),`[`,1)!=sapply(strsplit(names(x),'\\.'),`[`,2)]

#'Calculate order of half matrix
#' @param x half matrix for calculation
#' @noRd
#' @export
ord.halfmatrix <- function(x,...){
  ord <- sqrt(0.25+2*length(x))-0.5
  if(as.integer(ord)!=ord)stop('invalid length for half matrix')
  ord
}

#' make matrix based on halfmatrix
#' @param x half matrix for calculation
#' @noRd
#' @export
as.matrix.halfmatrix <- function(x,...){
  d <- ord.halfmatrix(x)
  y <- matrix(nrow=d,ncol=d)
  dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
  dex <- dex[dex$col <= dex$row,]
  y[as.matrix(dex)] <- x
  y[is.na(y)] <- t(y)[is.na(y)]
  y
}

#' @export
print.halfmatrix <- function(x,...)print(unclass(x))
as.halfmatrix <- function(x,...)UseMethod('as.halfmatrix')
#' @export
as.halfmatrix.halfmatrix <- function(x,...)x
#' @export
as.halfmatrix.default <- function(x,...)half(as.matrix.halfmatrix(x))

