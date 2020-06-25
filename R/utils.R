#' Open the current working directory
#'
#' @examples
#' openwd()
#' @export
openwd <- function(){
  shell.exec(getwd())
}

#'Assign a vector's values to its names
#'
#'This function is mostly used when the start of a chained (e.g. \code{%>%})
#'computation, it is good to have the initial parameters be names of the output
#'@param x A vector
#'@examples
#'selfname(1:10)
#'
#'c(4,6,8) %>%
#'selfName %>%
#'map(~ mtcars[mtcars$cyl == .x, ])
#'
#'  @export
selfName <- function(x){
  output <- x
  names(output) <- x
  output
}

#' Convenience wrapper for \code{microbenchmark::microbenchmark}
#'
#' @inheritParams microbenchmark::microbenchmark
#' @export
mb <- function(...,list = NULL, times = 100L, unit, check = NULL,
               control = list(), setup = NULL){
  microbenchmark::microbenchmark(...,list = list, times = times, unit = unit,
                                 check = check, control = control, setup = setup)
}

#' Remove all columns that have any \code{NA}s
#'
#' @param x A matrix or data.frame
#' @export
na.removeCols <- function(x){
  x[ , apply(x, 2, function(x) !any(is.na(x)))]
}

#' Excel's STDEV.P
#'
#' This is the naive and biased estimator for standard deviation. It is often
#' (mis)used in excel. When collaborating with others it may be necessary to use.
#' @inheritParams stats::sd
#' @export
sdp <- function(x, na.rm = FALSE){
  n <- x %>% na.omit %>% length
  sd(x)*sqrt(n-1)/sqrt(n)
}


#' Provide a default if a value is \code{NULL} or \code{NA}
#'
#' This is useful mostly when piping lists of data through map
#' functions. Often, one member of the pipeline will be NA or NULL where
#' an empty data.frame or xts is needed. This is a great for selectively
#' mapping that to the correct type.
#' @export
noneDefault <- function(value,default){
  if(is.null(value) |
     identical(value,NA) |
     identical(value,NA_integer_) |
     identical(value,NA_real_) |
     identical(value,NA_character_) |
     identical(value,NA_complex_) |
     identical(value,NaN)) default else value
}


#' Assign the most recent return (\code{.Last.value}) to a name
#' @export
assignLast <- function(name){
  assign(x = name, value = .Last.value, envir = .GlobalEnv)
}

#' @describeIn assignLast Assign .Last.value to asd
#' @export
asd <- function() assignLast('asd')

#' @describeIn assignLast Assign .Last.value to asd
#' @export
zxc <- function() assignLast('zxc')

#' Get an object's size in MB
#' @export
object.Mb <- function(x) { object.size(x) %>% format("Mb") }
