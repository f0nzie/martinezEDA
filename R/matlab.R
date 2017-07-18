mlab_eig <- function(M) {
  # get the eigenvectors and eigenvalues of the covariance matrix
  eig <- eigen(M)  # calculate the eigenVv of the covariance of X
  # shift the columns in the eigenvectors as in Matlab
  V <- eig$vectors
  VV <- V[, 2:1]
  # square root of the diagonal of eigenvalues withut infinite
  eigen_values <- eig$values
  eigen_values_mlab <- rev(eigen_values)   # reverse the eigen values as in Matlab
  D <- diag(eigen_values_mlab)
  # DD <- diag((diag(D))^(-1/2))

  return(list(V = VV, D = D))
}

m.eig <- function(M) {
  # replicate the Matlab eig() function
  # Returns:
  #       the diagonal of eigen$values
  #       the negative of eigen$vectors in descending order
  eig <- eigen(M)
  # evalues <- matrix(eig$values, ncol = 1)
  evalues <- diag(rev(eig$values))
  evectors <- eig$vectors
  evectors <- - evectors[, dim(evectors)[1]:1] # count down
  return(list(values = evalues, vectors = evectors))
}

"%^%" <- function(x, n)
  # create a special operator for raising matrix to a `n` power
  with(eigen(x),
       vectors %*%
         (values^n * t(vectors))
  )



segmentInf <- function(xs, ys, ...){
  # draw infinite line given two point coordinates
  fit <- stats::lm(ys~xs)
  graphics::abline(fit, ...)
}


projectionMatrixLine <- function(M, ...) {
  # draw the projection line given the projection matrix of angle theta
  segmentInf(M[,1], M[,2], ...)
}

#' Replicates the repmat function in Matlab
#'
#' @param M a matrix
#' @param n number of rows
#' @param m number of columns
#' @export
m.repmat <- function(M, n, m) {
  # replicates the repmat function in Octave
  # M should be a matrix
  kronecker(matrix(1, n, m), M)
}

m.size <- function(A) {
  # replicates the Matlab size function
  dim(A)
}

m.flipud <- function(M) {
  # replicate flipud
  apply(M, 2, rev)
}


#' Replicate the Matlab zeros function
#' @param m number of rows
#' @param n number of columns
#' @export
m.zeros <-function(m, n) {
  # replicate the Matlab zeros function
  matrix(0,nrow=m,ncol=n)
}


#' Replicate Matlab function svd including the diag(d)
#'
#' @param M a matrix
#' @export
m.svd <- function(M) {
  # replicate Matlab function svd including the diag(d) which is not
  # included in R svd()
  svd_ <- svd(M)
  list(d = diag(svd_$d), u = svd_$u, v = svd_$v)
}
