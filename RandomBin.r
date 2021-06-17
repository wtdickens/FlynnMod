RandomBin <- function(dim1, dim2){
  # This function creates a matrix of ones and zeros
  # of the size specified in dim1 and dim2.
  #
  #            William Dickens
  #                6/17/21
  # Parameters
  # ----------
  # dim1 - integer scalar
  #  The size of the first dimension of the matrix to be returned
  # dim2 - integer scalar
  #  The size of the second dimension of the matrix to be returned
  #
  # Returns
  # -------
  # RetMat - Binsary matrix
  #
  
  library(MASS)
  
  RetMat <- lapply(runif(dim1 * dim2), 
                   gtpt5 <- function(x){
                     result <- 0
                     if (x > .5) {result <- 1}
                     return(result)}
                  )
  RetMat <- matrix(RetMat, nrow=dim1, ncol=dim2)
  return(RetMat)
}