@@ -0,0 +1,42 @@
  ##Matrix inversion is a costly operation(time consuming),hence we cache the matrix.
  ##We can look up inverse value in the cache than recomputing it repeatedly.(We assume that the matrix supplied is always invertible.)
  ##These two pairs of functions helps us with that.
  
  ## This function creates a special "matrix" object that can cache its inverse.
  
  makeCacheMatrix<-function(x=matrix()){
    invm<-NULL
    set<-function(y){
      x <<- y
      invm<<-NULL
    }
    
    get<-function() x
    
    setinverse<-function(inverse) invm<<-inverse
    
    getinverse<-function() invm
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ##If the inverse has already been calculated (and the matrix has not changed), 
  ##then the cachesolve should retrieve the inverse from the cache.Solve fuction is used to find the inverse of square matrix.
  
  cacheSolve <-function(x,...){
    invm<-x$getinverse()
    if(!is.null(invm))
    {
      message("getting cached data")
      return(invm)
    }
    data<-x$get()
    invm<- solve(data,...)
    x$setinverse(invm)
    invm
  }
  