## Programming Assignment 2: Lexical Scoping

## The following two functions are used to create a special object that stores a 
## matrix and then saves (caches) the inverse of that matrix for subsequent retrieval.  
##
## Please note that these two functions (makeCacheMatrix and cacheSolve) were 
## constructed by using the two example functions (makeVector and cachemean) as 
## templates.  Those two functions are described in the discussion section for 
## Programming Assignment 2: Lexical Scoping (please see link below).
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
## Also, the examples used to demonstrate these functions have been influenced by 
## some of the course discussion forum threads - specifically those of mentors 
## Leonard Greski and Allen Berger.  Links to their posts appear below. 
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/jyJBRWmREeaeEw4INb6PhQ
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg


## FUNCTION makeCacheMatrix
## This function creates a special matrix object and initializes the value of its inverse
## to NULL.  It returns a list comprised of four functions ("getters" and "setters") which do 
## the following:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Double-arrow operator assigns to variables in the parent environment.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvrs <- function(solve) m <<- solve
        getinvrs <- function() m
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## FUNCTION cacheSolve
## When cacheSolve is called for the very first time, for a given matrix, it will compute and  
## store the inverse of that matrix.  Subsequent calls to cacheSolve will check to see if the inverse
## of that matrix already exists in memory.  If so, then it will return the saved (cached)
## value of the inverse.  If cacheSolve is called using a new matrix as input, then 
## it will compute the inverse for the new matrix and then store (cache) the value of the 
## inverse in memory, for subsequent retrieval.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvrs()
        ## If the inverse already exists in memory, return the cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If the inverse does NOT exist in memory, compute the inverse and 
        ## save (cache) it for potential retrieval in the future.
        data <- x$get()
        m <- solve(data, ...)
        x$setinvrs(m)
        m
}



## Let's illustrate the use of the above two functions with some examples.
## First, create a matrix called M1.
M1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## Display M1
M1

## Call the makeCacheMatrix function, using M1 as our input matrix. 
myMatrix_Object <- makeCacheMatrix(M1)
## Display myMatrix_Object. Note that it returns a list of functions. 
myMatrix_Object

## Note: Calling myMatrix_Object$get() will return our input matrix, M1.
myMatrix_Object$get()

## Compute the inverse of M1 using the cacheSolve function. Since this is the 
## first time we are solving for the inverse of M1, it will NOT return the 
## "getting cached data" message.
M1_Inv <- cacheSolve(myMatrix_Object)
## Display the inverse of M1.
M1_Inv

## If we call cacheSolve again, using the same myMatrix_Object, it should
## return the cached (saved) version of the inverse, along with the "getting 
## cached data" message. 
cacheSolve(myMatrix_Object)

## Check to see that the Identity matrix is returned when we perform 
## matrix multiplication of our matrix with its respective inverse.
M1 %*% M1_Inv


####################################################################


## New Example
## Let's create a new matrix, M2.
M2 <- matrix(c(2, -4, 3, 1, -1, 1, 0, -3, 2), nrow = 3, ncol = 3)
## Display M2
M2

## While we could reset the value of our input matrix by once again calling 
## the makeCacheMatrix function, i.e. myMatrixObject <- makeCacheMatrix(M2), 
## let's instead, reset it using the set function.
myMatrix_Object$set(M2)

## If we run myMatrix_Object$get(), we should now see our new M2 matrix.
myMatrix_Object$get()

## Compute the inverse of M2 using the cacheSolve function. Since this is the 
## first time we are solving for the inverse of M2, it will NOT return the 
## "getting cached data" message.
M2_Inv <- cacheSolve(myMatrix_Object)
## Display the inverse of M2.
M2_Inv

## If we repeat the call to cacheSolve, it will return the cached (saved) 
## inverse, along with the "getting cached data" message.
cacheSolve(myMatrix_Object)

## Finally, let's confirm that when we multiply our input matrix by its inverse,
## that we do indeed obtain the Identity matrix.
M2 %*% M2_Inv

