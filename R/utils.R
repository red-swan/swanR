#' @export
openwd <- function(){
  shell.exec(getwd())
}

#' @export
selfName <- function(x){
  output <- x
  names(output) <- x
  output
}

#' @export
mb <- function(...,list = NULL, times = 100L, unit, check = NULL,
               control = list(), setup = NULL){
  microbenchmark::microbenchmark(...,list = list, times = times, unit = unit,
                                 check = check, control = control, setup = setup)
}

#' @export
na.removeCols <- function(x){
  x[ , apply(x, 2, function(x) !any(is.na(x)))]
}

#' @export
sdp <- function(x, na.rm = TRUE){
  n <- x %>% na.omit %>% length
  sd(x)*sqrt(n-1)/sqrt(n)
}

null.Default <- function(a,b){
  if(is.null(a)) b else a
}

#' @export
asd <- function() assign(x = 'asd',value = .Last.value, envir = .GlobalEnv)
