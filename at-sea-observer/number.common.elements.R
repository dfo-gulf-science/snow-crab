number.common.elements <- function(x, y, index = FALSE){
   
   # Convert data frame to vector form:
   if (is.data.frame(x)) x <- concatenate(x)
   if (is.data.frame(y)) y <- concatenate(y)
   
   # Number of common elements between two vectors:
   n <- 0
   ix <- order(x)
   iy <- order(y)
   x <- x[ix]
   y <- y[iy]
   iix <- NULL
   iiy <- NULL
   s <- NULL
   for (i in 1:length(x)){
      ii <- grep(x[i], y)
      if (length(ii) > 0){
         ii  <- ii[1]
         iix <- c(iix, ix[i])
         iiy <- c(iiy, iy[ii])
         s <- c(s, x[i])
         y   <- y[-ii]
         iy  <- iy[-ii]
         n <- n + 1
      } 
   }
      
   if (!index) return(n) else return(cbind(iix, iiy))
}

