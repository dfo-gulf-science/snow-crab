concatenate <- function(x, sep = "-"){
   # CONCATENATE - Generates a single concatenated string for each line in a matrix or data frame: 
        
   x <- as.data.frame(x, stringsAsFactors = FALSE)
   if (ncol(x) == 1){
      return(gsub("(^ +)|( +$)", "", as.character(x[, 1])))
   }else{
      for (i in 1:ncol(x)){
         if (i == 1){
            v <- concatenate(x[,i])
         }else{
            v <- paste0(v,  sep, concatenate(x[,i]))
         }
      }
   }
   return(v)
}
   