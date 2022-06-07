## This function returns 4 functions needed to calculate cache

makeCacheMatrix <- function(x = matrix()) {
  A1<-NULL
  
  set<-function(y){
    x<<- y
    A1<<-NULL
  }
  
  get <- function(){x}
  setinversemtr <- function(Solve){A1<<-Solve}
  getinversemtr <- function(){A1}
  
  list(set = set, get = get, setinversemtr= setinversemtr,  
       getinversemtr =  getinversemtr  )
  }



## This function calculates inverse matrix using caching 

cacheSolve <- function(x, ...) {
  A1 <- x$getinversemtr ()
  if(!is.null(A1)) {
    message("getting cached data")
    return(A1)
  }
  data <- x$get()
  A1 <- solve(data, ...)
  x$setinversemtr(A1)
  A1
        ## Return a matrix that is the inverse of 'x'
}

m1 = makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
cacheSolve(m1)
cacheSolve(m1)
