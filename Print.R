# Print.R
#

Print <- function(...){
 key <- as.list(substitute(list(...)))[-1L]
 val <- list(...)
 mapply(
  function(k, v){
   cat(k, "= ")
   if(!is.matrix(v) && (is.logical(v) || is.numeric(v) || is.complex(v) || is.character(v))){ cat(v, "\n") }
   else{ cat("\n"); print(v); cat("\n") }
  },  
  key, val)
 cat("\n")
}

