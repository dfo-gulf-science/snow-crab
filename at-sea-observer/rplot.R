rplot <- function(x, round = 1, reference.limit, ...){
   # RPLOT - Diagnostic plot to evaluate the degree of rounding for unit rounded data.
   
   # Round data to specified precision.
   x <- round(x / round) * round

   # Define layout matrix:
   m <- rbind(0, cbind(0, kronecker(matrix(1:2, ncol = 1), matrix(1, ncol = 6, nrow = 6)), 0), 0)
   
   windows()
   layout(m)
   
   # Data frequency plot:
   t <- table(x)
   par(mar = c(2, 1, 3, 2)) # c(bottom, left, top, right)
   dbarplot(t, as.numeric(names(t)), xaxs = "i", yaxs = "i", xlab = "", ylab = "", cex.axis = 1.25, ...)
   mtext("Value", 1, 2.75, cex = 1.25)
   mtext("Frequency", 2, 2.75, cex = 1.25)   
   if (!missing(reference.limit)){
       if (length(reference.limit) != 1) stop("'reference.limit' must be a numeric scalar.")
       lines(rep(reference.limit, 2), par("usr")[3:4], col = "red", lty = "dashed", lwd = 2)
   }
   
   # Last digit frequency plot:
   t <- table(round(x / round) %% 10)
   dbarplot(t, as.numeric(names(t)), xaxs = "i", yaxs = "i", xlab = "", ylab = "", cex.axis = 1.25)
   mtext("Last digit of observation", 1, 2.75, cex = 1.25)  
   mtext("Frequency", 2, 2.75, cex = 1.25) 
   mu <- sum(t) / 10
   sigma <- mu * 0.1 * 0.9
   abline(mu, 0, col = "red", lwd = 2)
   abline(mu - 1.96 * sigma, 0, col = "red", lwd = 1.5, lty = "dotted")  
   abline(mu + 1.96 * sigma, 0, col = "red", lwd = 1.5, lty = "dotted") 
}
