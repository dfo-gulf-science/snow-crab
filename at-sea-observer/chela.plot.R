chela.plot <- function(carapace.width, chela.height, xlim = c(80, 145), ylim = c(10, 42), scale = 1, ...){
   # CHELA.PLOT - Chela height versus carapace width diagnostic plot.

   # Create empty plot:
   plot(xlim, ylim, type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", ...)
   grid()  
   mtext("Carapace width (mm)", 1, 2.75, cex = 1.25)
   mtext("Chela hieght (mm)", 2, 2.75, cex = 1.25)   
   
   # Round off data:
   carapace.width <- round(carapace.width)
   chela.height <- round(chela.height)
   
   f <- aggregate(list(n = carapace.width), by = data.frame(x = carapace.width, y = chela.height), length)
   points(f$x, f$y, cex = scale * 1000 * (1/sum(f$n)) * sqrt(f$n), pch = 21, bg = "grey70")
  
   # Calculate immature and mature reference lines (estimated from survey data):
   xx <- seq(par("usr")[1], par("usr")[2], len = 1000)
   yi <- exp(-3.121 + 1.321 * log(xx))
   ym <- exp(-3.146 + 1.376 * log(xx))
   
   # Approximate confidence intervals:
   si <- cbind(exp(log(yi) - 1.96 * exp(-3.00)), exp(log(yi) + 1.96 * exp(-3.00)))
   sm <- cbind(exp(log(ym) - 1.96 * exp(-3.00)), exp(log(ym) + 1.96 * exp(-3.00)))
   
   # Draw reference lines:
   lines(xx, yi, col = "green", lwd = 2)
   lines(xx, ym, col = "blue", lwd = 2)
   lines(xx, si[, 1], lwd = 2, lty = "dashed", col = "green")
   lines(xx, si[, 2], lwd = 2, lty = "dashed", col = "green")
   lines(xx, sm[, 1], lwd = 2, lty = "dashed", col = "blue")
   lines(xx, sm[, 2], lwd = 2, lty = "dashed", col = "blue")   
   
   legend("topleft", c("Mature", "Immature"), lwd = 2, col = c("blue", "green"), bg = "white", cex = 1.25)
   box()
}
