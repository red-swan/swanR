




#' Annualize Returns with NA safety
rtnAnnualized.default <- function(rtns, scale = NA, geometric = TRUE, na.rm = FALSE){
  rtns <-
    if(na.rm){
      rtns[!is.na(rtns)]
    } else {
      rtns
    }
  n <- length(rtns)
  if(geometric){
    prod1p(rtns)^(scale / n) - 1
  } else {
    mean(rtns)*scale
  }

}

#' Annualize Returns with NA safety
rtnsAnnualized.xts <- function(rtns, scale = NA, geometric = TRUE, na.rm = FALSE){
  out <- apply(rtns,2,rtnAnnualized.default,
               scale = scale, geometric = geometric, na.rm = na.rm)
  matrix(out, nrow = 1) %>% set_colnames(names(out))
}

#' Annualize returns with NA safety
#' @export
rtnAnnualized <- function(rtns, scale = NA, geometric = TRUE, na.rm = FALSE){
  UseMethod("rtnsAnnualized",rtns)
}












