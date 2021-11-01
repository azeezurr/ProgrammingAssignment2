## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##initialize the inverse matrix
        xMatrix <- NULL
        
        #define the set function for the matrix
        set <- function(y) {
                x <<- y
                xMatrix <<- NULL
        }
        
        ##define the return matrix object by defining it as get
        get <- function() {
                x ##return special matrix
        }
        
        ##setting the inverse matrix 
        setInvMatrix <- function(invM){
                xMatrix <<- invM ##assign the inverse matrix to the environment var
        } 
        
        ##define the function for returning the inverse matrix
        getInvMatrix <- function(){
                xMatrix ##the environment var inverse matrix is return
        }
        
        ##put all defined function into a list for future call within the environment
        list(set = set,
             get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        invM <- x$getInvMatrix()
        
        ## 2. Verified if the inverse has already been calculated
        if (!is.null(invM)){
                ## check if the return xMatrix is identical
                if ( identical( x$get() %*% invM, invM %*% x$get() ) ){
                        ## get it from the cache and skips the computation. 
                        print("getting cached data")
                        return(invM)
                }
        }
        
        ## 3. Else inverse matrix null or matrix change, calculates the inverse 
        data <- x$get()
        invM <- solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setInvMatrix(invM)
        
        ## Return a matrix that is the inverse of 'x'
        print("getting new computated data")
        return(invM)
}
