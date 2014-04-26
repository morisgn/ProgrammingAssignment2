makeCacheMatrix <- function (x = matrix()) {
        #### This function creates a special "matrix" object that can cache its inverse.
        #### For this assignment, assume that the matrix supplied is always invertible.
        ## - set the value of the matrix
        ## - get the value of the matrix
        ## - set the value of the inverse
        ## - get the value of the inverse
        ################################################################
        ## ------- Example of test values:
        ##              > source("CacheMatrix.R")
        ##              > m1 <- matrix(c(1,0,30,40,1,3,45,3,4),3,3)
        ##              > x1 <- makeCacheMatrix(m1)
        ##
        ##              > m2 <- matrix(c(1,1,1,1),2,2)
        ##              > x2 <- makeCacheMatrix(m2)
        ################################################################
        
        # If the matrix is NOT SQUARE, stop the execution of the current expression and executes an error action.
        nrwow <- nrow(x)
        ncol <- ncol(x)
        if (identical(nrwow,ncol) == FALSE){
                stop ("Sorry! Your Matrix must be square: NxN - N rows and N columns")
        }
        
        #If the matrix is square, check if it is inversible
        # A square matrix that does not have a matrix inverse is called SINGULAR matrix
        # A matrix is singular if its determinant is 0. 
        if (det (x) ==0 ){
                stop ("Sorry! This matrix is square but not inversible. Please try another square matrix")
        }
        
        # Initialize the  inverse of the matrix to NULL
        matInv <- NULL
        
        # Set the value of the matrix. There is no cache yet
        set <- function(y) {
                x  <<- y
                matInv <<- NULL
                
        }
        
        # Get the value of the matrix
        get <- function() x
        
        # Set the inverse of the matrix
        setInv <- function(mat) matInv <<- mat
        
        # Get the inverse of the matrix
        getInv <- function() matInv
        
        # return a list of all the above functions
        list(set = set, get = get, setInv = setInv,getInv = getInv) 
                
}


cacheSolve <- function (x,...) {
        #### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        #### If the inverse has already been calculated (and the matrix has not changed), 
        #### then the cachesolve should retrieve the inverse from the cache.
        ################################################################
        ## ------- Example of test values:
        ##              > source("CacheMatrix.R")
        ##              > m1 <- matrix(c(1,0,30,40,1,3,45,3,4),3,3)
        ##              > x1 <- makeCacheMatrix(m1)
        ##              > cacheSolve(x1)
        ##              > cacheSolve(x1)  : To get cached data when called a second time
        ##
        ##              > m2 <- matrix(c(1,1,1,1),2,2)  # Not an inversible matrix
        ##              > x2 <- makeCacheMatrix(m2)
        ##              > cacheSolve(x2)
        ################################################################
        
        # If the inverse of the matrix is already cached, return its value
        matInv <- x$getInv()
        
        if(!is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        
        
        myMat <- x$get()             # If the inverse was not cached , get its value 
        matInv <- solve (myMat, ...) # Calculate the inverse
        x$setInv(matInv)             # Cahe the inverse
        matInv                       # Return the inverse   
        
}


