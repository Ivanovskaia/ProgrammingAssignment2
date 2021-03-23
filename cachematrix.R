# create matrix
makeCacheMatrix = function(a = matrix()){
  inv = NULL
  set = function(m){
    a <<- m
    inv <<- NULL
  }
  get = function() a
  SV = function(inverse) inv <<- inverse
 GV = function() inv
  list(set = set, get = get, 
       SV = SV,GV =GV)
}
# returning inverse
cacheSolve = function(a,...){
  inv = a$GV()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data = a$get()
  inv = solve(data,...)
  a$SV(inv)
  inv
}

