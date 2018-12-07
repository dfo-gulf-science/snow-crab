longest.common.sequence <- function(x, y){
   # Returns the length of the longest common sequence for two vectors:
   
   # Convert data frame to vector form:
   if (is.data.frame(x)) x <- concatenate(x)
   if (is.data.frame(y)) y <- concatenate(y)

   # Expand comparison matrix:
   m <- repvec(x, nrow = length(y)) == repvec(y, ncol = length(x))
   
   d <- row(m) - col(m)
   
   s <- split(m, d)

   fun <- function(t){
      if (length(t) == 0) return(0)
      if (length(t) == 1) return(1)
      kmax <- 1
      k <- 1
      for (i in 1:(length(t)-1)){
         if ((t[i+1]-t[i]) == 1) k <- k + 1 else k <- 1
         if (k > kmax) kmax <- k
      }
      return(kmax)
   }
   
   v <- max(unlist(lapply(lapply(s, which), fun)))
   
   return(v)   
}
