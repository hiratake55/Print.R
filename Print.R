Print <- function(..., sep="\n", digits=NULL){
 #
 # Print (2010.08.09, 2011.10.28,29,30)
 #
 # (C) Takekatsu Hiramura and Shin-ichi Mayekawa
 #
 # Usage
 #
 #     Print( var1, var2, ...., varN, sep="character", digits=number )
 #      where var.. can be of any type.
 #
 #   When var is either matrix or list, sep="\n" will be used.
 #
 #   Note that  format( var, digits=NULL ) is equal to format( var )
 #

  key <- as.list(substitute(list(...)))[-1L]
  val <- list(...)

  mapply(

  function(k, v){

   cat(deparse(k), "= ")
   if(!is.matrix(v) && (is.logical(v) || is.numeric(v) || is.complex(v)
                                      || is.character(v))){
    # single numeric, complex, logical, character or vector
    cat(format(v,digits=digits),sep)
   } else{
    # matrix and list
    cat("\n"); print(v, digits=digits); cat("\n")
   }
  },

  key, val) # end of mapply

  cat("\n")

} # end of Print