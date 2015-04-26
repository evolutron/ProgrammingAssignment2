## Instruction to test this code
#  1) source("cachematrix.R")
#  2) utest()
#  3) review the outputs on the console.
#  4) create your own matrices and imitate utest to test them

## makeCacheMatrix
#  1) creates an object that maintains a matrix and its inverse pair.
#  2) The inverse of the matrix is cached. 

#  Every time the matrix of the object is reSet, an EXTERNAL program (e.g. cacheSolve) has
#  has to explicitly recompute the inverse and reset the inverse in the makeCacheMatrix  
#  object.  

#  Ideally, both matrix and its inverse should be maintained internally in the 
#  makeCacheMatrix "class".  But I think the point of this exercise may be to show 
#  how NOT to do OOP, setting up for the intro do OOP syntax later. 


makeCacheMatrix <- function(x = matrix()) {

        #"constructor"
        m <<- matrix()
        inverse <<- matrix()
		compinv <<- TRUE
		
        set <- function(y) {
            if (!identical(m, y)) {  #Reset the matrix only it it is different.
                m <<- y
                inverse <<- NULL
				compinv <<- TRUE
		    } else {
			   if (!is.null(inverse))
			      compinv <<- FALSE
		    }
        }
		set(x)  #run constructor
		
        get <- function() m
		
		getcompinv <- function() compinv
		setcompinvfalse <- function() FALSE
		
        setinverse <- function(somevalue) inverse <<- somevalue
		
        getinverse <- function() inverse
		
        list(set = set, get = get, 
		     getcompinv = getcompinv,
			 setcompinvfalse = setcompinvfalse,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
#  Computes the inverse of a matrix where possible or return appropriate error messages.
#  
#  Note: cacheSolve does not do know anything about caching.  The caching is done by 
#  the setinverse method of the makeCacheMatrix class.

cacheSolve <- function(cmo, ...) {  # cmo = cached matrix object

        inverse <- matrix()
		
	    #Check if the inverse needs to be computed
        if(!cmo$getcompinv()) {
            message("No need to compute matrix inverse.")
            return(cmo$getinverse())
        }
		
		# invert this matrix
        cmo_mat <- cmo$get()
		result <- try(solve(cmo_mat), TRUE)
        if ( class(result) == "matrix" ) {
		  inverse <- result
		  # Double check by trying to recover identity matrix.
		  if (  !identical( (result %*% cmo_mat), diag(nrow(cmo_mat)) )  ) {
		    message("Warning: Matrix inverse may be inaccurate.")
            message("Resulting in Identity matrix :")
			print(result %*% cmo_mat)
		  }

		} else {
		  message("Matrix cannot be inverted. Inverse set to Null.")
		  inverse <- matrix()
		  cmo$setcompinvfalse()
		}
				
        inverse
}

# This code test the following cases:
# 0) Empty matrix
# 1) matrix known to be invertible
# 2) solving the same invertible matrix 
# 3) matrix known to be NOT invertible

utest <- function( ) {

  # Empty matrix 
  message ("--Testing Null matrix")
  mymatclass <- makeCacheMatrix(matrix())  # initial constructor
  print(mymatclass$get())
  inv <- cacheSolve(mymatclass)
  if (!is.null(inv)) mymatclass$setinverse(inv)
  print(mymatclass$getinverse())
  
  # Invertible matrix 
  mat_square <- matrix(1:4, 2,2)
  
  message ("--Testing square Matrix")
  mymatclass$set(mat_square)  # reset to new matrix
  print(mymatclass$get())
  inv <- cacheSolve(mymatclass)
  if (!is.null(inv)) mymatclass$setinverse(inv)
  print(mymatclass$getinverse())
  
  ## Set to square matrix again to check caching behaviour
  message ("--Testing square Matrix again right after first call.")
  mymatclass$set(mat_square)  # reset to new matrix
  print(mymatclass$get())
  inv <- cacheSolve(mymatclass)
  if (!is.null(inv)) mymatclass$setinverse(inv)
  print(mymatclass$getinverse())

  # Non-Square matrix 
  mat_rect <- 1:8
  dim(mat_rect) <- c(2,4)
 
  message ("--Testing non-invertible rectangular Matrix")
  mymatclass$set(mat_rect)  # reset to new matrix
  print(mymatclass$get())
  inv <- cacheSolve(mymatclass)
  if (!is.null(inv)) mymatclass$setinverse(inv)
  print(mymatclass$getinverse())
  
}
