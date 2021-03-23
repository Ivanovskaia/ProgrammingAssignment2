makeCacheMatrix = function(a = matrix()){
  inv = NULL
  set = function(m){
    a <<- m
    inv <<- NULL
  }
  get = function() a
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}

cacheSolve = function(a,...){
  inv = a$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data = a$get()
  inv = solve(data,...)
  a$setinv(inv)
  inv
}
