eventsBufferFutures <- function( bufferSize, FUN ) {
  library(future)
  
  buffer <- vector( mode='double', length=bufferSize )
  bufferCount <- 0
  f <- NULL
  
  add <- function( event ) {
    bufferCount <<- bufferCount + 1
    buffer[bufferCount] <<- event
    if ( bufferCount == bufferSize ) {
      f <<- future( FUN( buffer ) )
      bufferCount <<- 0
    }
    getValue()
  }
  
  getValue <- function() {
    if ( !is.null(f) ) {
      print(value(f))
      f <<- NULL
    }
  }

  obj <- list(add=add,getValue=getValue)
  class(obj) <- c('eventsBufferFutures' )
  return( obj )
  
}
