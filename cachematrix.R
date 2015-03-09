## xcc 20150309 for R Programming assignment 2

## This program can be used to calculate the inverse of a matrix.
## The result of the solve() function will be cached.
## So we won't bother calculating the inverse anymore when we need the inverse,
## unless the stored target matrix is changed.

## This function can be used to initialize the target matrix.
## It returns a list of functions.
## The "set" function is used to make a new target matrix.
## The "get" function is used to fetch the target matrix.
## The "set_inversed" function is used to save the result of "solve()".
## The "get_inversed" function is used fetch the result of "solve()".
## The last two functions are only for internal use, on priciple.

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    
    set <- function(new_matrix){
        # set new target
        x <<- new_matrix
        
        # reset result
        inversed <<- NULL
    }
    
    # for getting target
    get <- function() x
    
    # for saving the result
    set_inversed <- function(new_inversed) inversed <<- new_inversed
    
    # for getting the result
    get_inversed <- function() inversed
    
    # return functions
    list(set = set,
         get = get,
         set_inversed = set_inversed,
         get_inversed = get_inversed)
}


## If the cached result is NULL, calculate the inverse of the target matrix.
## Otherwise just take the cached result as the final result.

cacheSolve <- function(x, ...) {
    # fetch the cached result
    inversed = x$get_inversed()
    
    if(!is.null(inversed)){
        #print("cached")
        
        # if not NULL, just return it.
        return(inversed)
    }
    
    # get the target
    m <- x$get()
    
    # solve it
    inversed = solve(m, ...)
    
    # cache it
    x$set_inversed(inversed)
    
    # return it
    return(inversed)
}
