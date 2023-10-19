

is.square <- function(x,...)UseMethod('is.square')
#' Check if this is a square matrix
#' @param x matrix for testing
#' @noRd
#' @export
is.square.matrix <- function(x,...)dim(x)[[1]]==dim(x)[[2]]

#' Check if this is a diagonal matrix
#' @param matrix the object to check
#' @param eps tolerance for checking that off diagonals are zero
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


#' Quick access to example model output
#' @param file.type file type to extract, can choose from c("nmdata", "ext", "cov", "cor", "tab", "ctl", "lst", "mod", "simmod")
#' @param matrix.type default is "block" matrix, can choose "diag" matrix
#' @noRd
ex <- function(file.type, matrix.type = "block"){

  if(!(file.type %in% c("nmdat", "ext", "cov", "cor", "tab", "ctl", "lst", "mod", "simmod"))){
    stop('invalid file.type', call. = FALSE)}

  if(!(matrix.type %in% c("block", "diag"))){
    stop('invalid matrix.type', call. = FALSE)}

  path <- system.file("example-model", package = "simpar")

  if (file.type == "nmdat"){
    data <- utils::read.csv(file.path(path, "analysis3.csv"))
  } else if (file.type %in% c("ext", "cov", "cor", "tab") & matrix.type == "block"){
    data <- utils::read.table(file.path(path, "106", paste0("106.", file.type)), skip = 1, header = TRUE)
  } else if (file.type %in% c("ctl", "lst", "mod") & matrix.type == "block"){
    data <- readLines(file.path(path, "106", paste0("106.", file.type)))
  } else if (file.type == "simmod" & matrix.type == "block"){
    if(!requireNamespace("mrgsolve")){
      stop('mrgsolve must be installed to run this function', call. = FALSE)}
    mod <- mrgsolve::mread(file.path(path, "simmod", "106.mod"))
  } else if (file.type %in% c("ext", "cov", "cor", "tab") & matrix.type == "diag"){
    data <- utils::read.table(file.path(path, "107", paste0("107.", file.type)), skip = 1, header = TRUE)
  } else if (file.type %in% c("ctl", "lst", "mod") & matrix.type == "diag"){
    data <- readLines(file.path(path, "107", paste0("107.", file.type)))
  } else if (file.type == "simmod" & matrix.type == "diag"){
    if(!requireNamespace("mrgsolve")){
      stop('mrgsolve must be installed to run this function', call. = FALSE)
    }
    mod <- mrgsolve::mread(file.path(path, "simmod", "107.mod"))
  }

  if (file.type == "simmod"){
    return(mod)
  }else{
    return(data)
  }
}


