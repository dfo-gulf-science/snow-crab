compare.traps <- function(x, trap.id, fun = number.common.elements, threshold = 10){
   # COMAPRE - Function for determining the similarity of trap data:
   
   if (missing(x)) stop("'x' trap observations must be a vector, matrix or data frame of observations.")
   if (missing(trap.id)) stop("'trap.id' must be specified.")
    
   # Convert input to data frame:
   x <- as.data.frame(x)
   trap.id <- as.data.frame(trap.id)
   
   # Check that traps have the same number of observations:
   tmp <- aggregate(trap.id[, 1], by = trap.id, length)
   if (length(unique(tmp[, ncol(tmp)])) > 1) warning("Some traps have different numbers of observations.")
   
   # Subdivide data by traps:
   traps <- by(x, concatenate(trap.id), function(x) x)
   index <- unlist(lapply(traps, is.null))
   traps <- traps[!index]
   
   # Find traps with high numbers of common carapace width and chela height combinations:
   k <- 1       # Counter for number of comparisons.
   res <- NULL  # Variable for storing pairs of traps with highest number of similar observations. 
   v <- matrix(NA, nrow = length(traps), ncol = length(traps))
   for (i in 2:nrow(v)){
      for (j in 1:(i-1)){
         if (!is.null(traps[[i]]) & !is.null(traps[[j]])){
            # Determine the number of common observations for pair of trap observations:
            v[i,j] <- fun(concatenate(traps[[i]]), concatenate(traps[[j]]))
            if (v[i,j] > threshold) res <- rbind(res, c(i, j, v[i,j])) # Retain similarity measure if number is above threshold
            if (k %% 100 == 0) cat(paste("Performing ", k, " of ", (nrow(v) * (nrow(v)-1)) / 2, " comparisons.\n")) # Report progress.
            k <- k + 1
         } 
      }
   }
   
   if (!is.null(res)){
      colnames(res) <- c("trap i", "trap j", "n")
      res <- res[order(res[, 3], decreasing = TRUE), ]
      res <- as.data.frame(res)
      str <- names(trap.id)
      tmp1 <- trap.id[res[,1],]
      names(tmp1) <- paste0(str, "_i")
      tmp2 <- trap.id[res[,2],]
      names(tmp2) <- paste0(str, "_j")
      res <- cbind(tmp1, tmp2, res["n"])
      rownames(res) <- NULL
   }

   # Tabulate results:
   results <- list(similarity.matrix = v, 
                   top.similarities = res)
   
   return(results) 
}
