## Put comments here that give an overall description of what your
## function does:

##This function returns a list which contains parameters that work with  a matrix. The fuction creates and initializes  matrix x.  
##Variable n is created and intialized to NULL. Variable n will only show up if a previous matrix calculation has been executed before. 
##If there is no previous calculation, n is NULL. 
## 

## Write a short comment describing this function:

makeCacheMatrix <- function(x = matrix()) {
      n  <- NULL    #current environmnet variable
      set  <- function(y){
            x <<- y ## << for environment
            n <<- NULL 
      }
      
      get  <- function() x #gets value of matrix x
      setinverse  <- function(solve) n  <<- solve
      getinverse  <- function() n  #getinverse is in main environment, while n is not in the same environment
      
      list(set= set, get = get, #return list and executes main function
            setinverse = setinverse, 
            getinverse = getinverse)
}

##______________________________________________________________________________________________________________________________________________________________
## Write a short comment describing this function:

##casheSolve calculates and returns an inversed  matrix. When the fist/current matrix doesn't have an inverse, the funcion shows an error.
##  If the inverse of the first/current matrix is already calculated, then, the function gets the inverse matrix already stored. This way, it saves memory and process time.
##  But, if a new calculation is in order, then the function will calculate and store it in current environment and other environment to use in  the future.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
      n  <- x$getinverse()  #store in variable n in current environment
      if (!is.null(n)){ # check to see if it is NULL. Check to see if already cached.
            message("getting cached data")
            return(n)
      }
      
      data  <- x$get() # in case n is NULL
      n  <- solve(data, ...) #once n is stored, setinverse is called and stored to use in the future.
      x$setinverse(n)
      n #inverse matrix
}
