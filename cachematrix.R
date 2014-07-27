## This program consists of two functions; makeCacheMatrix and cacheSolve. makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix  <- function(x = matrix()) {
        
        #initialize inverse property 'i' to NULL
        i <- NULL
        
        #This is to set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #This is to get the contents of the matrix
        get <- function()
        {
                x
        }
        
        #This Method sets the inverse of the matrix
        set_Inverse <- function(inverse)
        {
                i <<- inverse
        }
        
        #This method gets the inverse of the matrix
        get_Inverse <- function()
        {
                i
        }
        
        #Return a list of the methods
        list(set = set, get = get,
             set_Inverse = set_Inverse,
             get_Inverse = get_Inverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_Inverse()
        
        #Retrieve the inverse from the cache if the matrix has not changed
        if(!is.null(i)) 
        {
                message("getting cached data")
                return(i)
        }
        
        #Compute the inverse of the matrix if the matrix has changed
        
        data <- x$get()
        
        #Compute the inverse of the matrix using the solve() function
        i <- solve(data) %*% data
        
        x$set_Inverse(i)
        
        #Return the matrix
        i
        
}
