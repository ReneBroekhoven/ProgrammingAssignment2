## A warm welcome to my fellow peers from Rene (<- that's me') !
 


## Clearing the (possibly) previously used variables, 
## creating a cache in the parent environment and creating four functions 
## set, get , setinverse and getinverse)
## Input is a matrix (which is supposed to be 'inversable' in principle)
## Output is a list with the four functions
makeCachematrix <- function(x = matrix()) {
    # setting m in the global environment 
    m <- NULL
    # setting x and m in the parent environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # getting x from the global environment or if not there in the parent
    get <- function() {
        x
    } 
    # setting the m in the parent enviromnment to the inverse variable
    setinverse <- function(inverse){
        m <<- inverse
    }
    # getting m (which was the inverse variable) form the global environment or if not ther in the parent
    getinverse <- function(){
        m
    } 
    # make a list of the four just created functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Returns a matrix which is the inverse of the original 'x' matrix 
cacheSolve <- function(x, ...) {
    # looking for the inverse variable and assigning to m
    m <- x$getinverse()
    # if the inverse variable (m) is already there use the cached variable
    if(!is.null(m)) {
        message("returning cached inversed matrix")
        return(m)
    }
    # if no inverse variable(m) create the inverse of the matrix variable and return the inverse itself
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}