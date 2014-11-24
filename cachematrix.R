##  There are 2 functions, makeCacheMatrix, and cacheSolve
##  This is to demonstrate the use of lexical scoping 
##  Lexical scoping is method of using free variables that are 
##  defined in the enviroment they are created. 
##  Below is a more detailed description of each function

## makeCacheMatrix is a function that first defines a matrix.  The  
## function has a holding variable "inv" that will be the inverse and is initially
## defined as NULL.  There are 3 other functions defined but not run
## until cacheSolve calls them for setting the "inv".
## The function has a mechanism to screen for for a new object to 
## be used to recalculate the inverse.

makeCacheMatrix <- function(x = matrix()) {    # input x will be a matrix
        
        inv <- NULL                                #  inv will be our 'inverse' and it's reset to NULL every 
                                                   #  time the function makeCacheMatrix is called


	set <- function(y) {                    #  Set will reset the inverse matrix to NULL so that if the matrix 
                x <<- y                         #  is re-assigned it invalidates the previously computed inverse
                inv <<- NULL
        } 
        
                                                #   The following 3 functions are defined but not run when makeCacheMatrix is called.
                                                #   instead, they will be used by cachesolve() to get values for x or for
                                                #   inv (inverse) and for setting the inverse.  
                                                #   These are usually called object 'methods' in other OOP
        
        get <- function() { x }                 # this function returns the value of the original matrix
        
        setinverse <- function(inverse)  { inv <<- inverse }
                                                # this is called by cachesolve() during the first cachesolve()access and it will store the value
        
        getinverse <- function() { inv }        # this will return the cached value to cachsolve() on
                                                #  subsequent accesses
        
        list(set = set, get = get,     #   This portion is used each time makeCacheMatrix() is called,       
             setinverse = setinverse,  #   that is, each time we make a new object.  This is a list of 
             getinverse = getinverse)  #   the internal functions ('methods') so a calling function
                                       #   knows how to access those methods.                            
}





## cacheSolve is a function that uses the input x from makeCacheMatrix.
## It uses "x" and uses the solve() function to determine the inverse
## of the matrix if it is already cached.  A message is printed if the 
## cached value is returned, along with the cached value.  
## There is a mechanism that checks if "inv" is NULL.  If this is the case
## then the inverse is calculated using solve(), stores that value and 
## returns that value to the called function

cacheSolve <- function(x, ...) {     # the input x is an object created by makeCacheMatrix
        inv <- x$getinverse()             # accesses the object 'x' and gets the inverse using the solve function in R
        if(!is.null(inv)) {          # if inverse was already cached (not NULL) ...
                
                message("getting cached data")    # ... send this message to the console
                return(inv)                       # ... and return the cached inverse matrix ... "return"
                                                  #   the function makeCacheMatrix() then ends here
        }   #else "do below"
        data <- x$get()           # we reach this code only if x$getinverse() returned NULL
        inv <- solve(data, ...)   # if inv was NULL then we have to calculate the inverse
        x$setinverse(inv)         # store the calculated inverse value in x (from setinverse() in makeCacheMatrix)
        inv                       # return the inverse to the code that called this function
}





