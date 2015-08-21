

##The functions create a global variable that is used to place a matrix inverse in a cache
## The cached inverse can be recalled later--this is more efficient than recalculating the inverse

## makeCacheMatrix establishes and assigns the matrix to a global variable

makeCacheMatrix <- function(x = matrix()) {
  
        invm <- NULL  ##global variable where inv = matrix inverse
        
        set <- function(y) {
                
                x <<- y   ##sets x (matrix) as global variable equal to y
                invm <<- NULL
                
        }
        
        get <- function() x
        setinvm <- function(inverse) invm <<- inverse
        getinvm <- function() invm
        list(set = set, get= get, setinvm=setinvm, getinvm=getinvm)
  
            

}


## cacheSolve calculates the inverse of a matrix after 1st checking to see if a cached version
## of the same matrix is available. 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        
        invm <- x$getinvm()
        
        if(!is.null(invm)) {
                
                message("getting cached data")
                return(invm)
        }
        
        matrix.data <- x$get()
        invm <- solve(matrix.data, ...)
        
        x$setinvm(invm)
        
        return(invm)
 

}
