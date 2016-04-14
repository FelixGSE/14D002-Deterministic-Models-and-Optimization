###################################################################################################
### Project - Group 7
###################################################################################################

# Author:   Felix Gutmann
#           Max van Esso
#           Marco Fayet
# Course:   Deterministic Models and Optimization
# Due:      11.12.2015
# Type:     Project - Topic 1 
# Content:  Naive and Divide & Conquer Algorithms to count inversions in an input array

###################################################################################################
#### START CODE # START CODE # START CODE # START CODE # START CODE # START CODE # START CODE #####
###################################################################################################

###################################################################################################
### 1. Naive algorithm
###################################################################################################

Naive.count.inversion <- function(A){
  
  # Define legnth of input Array
  N <- length(A)
  # Initialize the inversion counter 
  inv.count <- 0
  for( i in 1:(N-1) ){
    # Define temporary element of A
    temp <- A[i]
    for( j in  (i+1):N ){
      # Compare elements to the right of temp
      if( temp > A[j] ) {inv.count <- inv.count + 1}
    }
  }
  # Return the total number of inversions
  return(inv.count)
  
}

###################################################################################################
### 2. Divide and conquer algorithm
###################################################################################################

DC.count.inversion <- function(A){

  # Initialize sort and count function
  sort.and.count <- function(A){
    
    # Get length of the input array
    N <- length(A)
    # Base case
    if( length(A) == 1 ){
      
      # Return vector and zero
      return( list( array = A , inversion = 0 ) )
    
    } else { 
      
      # Define left and right array
      mid <- ceiling(N/2) 
      # Compute count 
      sc1 <- sort.and.count( A[ 1:mid ] )
      sc2 <- sort.and.count( A[ (mid+1):N ] )
      mc  <- count.and.merge( sc1$array , sc2$array )
      
      # Count inversions
      inv.count <- sc1$inversion + sc2$inversion + mc$inversion
      
      # Return array and inversion count
      return(list( array = mc$array , inversion = inv.count ))
    
    }
  }

  # Initialize count and merge function
  count.and.merge <- function(res.a,res.b){
    # Initialize output list
    result  <- c()
    # Initialize count variable
    cross.count <- 0
    # Reset iterators
    a <- 1
    b <- 1
    # Find inversions
    while( a < length(res.a) + 1  && b < length(res.b) + 1 ) {
        # Check array a versus array b
        app     <- min(res.a[a],res.b[b])
        # Append to output list
        result  <- c(result,app)
        if( res.b[b] == app ){
          # Update cross inversion count
          temp  <- length(res.a) - a + 1 
          cross.count   <- cross.count + temp 
          # Update iterator b
          b = b + 1
        } else {
          # Update iterator a
          a = a + 1
      }
    }
    # Design final output - Append non empty list 
    if( a > b ) { 
      result <- c(result, res.b[b:length(res.b)]) 
      } else {
      result <- c(result, res.a[a:length(res.a)])
    }
    # Return inversions and sorted vector
    return(list( array=result, inversion=cross.count) )
  }

# Execute function and store results
res <- sort.and.count(A)

# Return final output
return(list( array = res$array, inversion = res$inversion))

}

###################################################################################################
###### END CODE # END CODE # END CODE # END CODE # END CODE # END CODE # END CODE # END CODE ######
###################################################################################################