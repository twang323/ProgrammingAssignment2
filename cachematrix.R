makeCacheMatrix <- function(x=matrix()){
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_solve <- function(inv) inverse <<- inv
  get_solve <- function() inverse
  list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
}

cacheSolve <- function(x,...){
  inverse <- x$get_solve()
  if(!is.null(inverse)){
    message("gettting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <-solve(data,...)
  x$set_solve(inverse)
  inverse
}

# Define the first matrix
first_matrix <- matrix(c(-2,4,1,-3),nrow=2,ncol=2)

# Apply makeCacheMatrix function over first matrix, out1 as the output
out1 <- makeCacheMatrix(first_matrix)

# Access out1 subfuction get()
print(out1$get())

# Access out1 subfunction get_inverse()
print(out1$get_solve())

# Define the a updated matrix
update_matrix <- matrix(c(4,2,7,6),nrow=2,ncol=2)

# Set the matrix in out1 with update_matrix
out1$set(update_matrix)

# Cache the inverse matrix of update_matrix
print(cacheSolve(out1))