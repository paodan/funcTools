
#' Object oriented programming like Javascript
#' 


newObject = function(`_class` = NULL, `_inherit` = NULL, ...){
  e <- new.env(parent = emptyenv())
  
  members <- list(..., self = function(self) {
    return(base:::as.list.environment(self))
  })
  if (length(members) != sum(nzchar(names(members)))) {
    abort("All members of a ggproto object must be named.")
  }
  
  # R <3.1.2 will error when list2env() is given an empty list, so we need to
  # check length. https://github.com/tidyverse/ggplot2/issues/1444
  if (length(members) > 0) {
    list2env(members, envir = e)
  }
  
  # Dynamically capture parent: this is necessary in order to avoid
  # capturing the parent at package build time.
  `_inherit` <- substitute(`_inherit`)
  env <- parent.frame()
  find_super <- function() {
    eval(expr = `_inherit`, envir = env, enclos = NULL)
  }
  
  super <- find_super()
  if (!is.null(super)) {
    e$super <- find_super
    class(e) <- c(`_class`, class(super))
  } else {
    class(e) <- c(`_class`, class(e))
  }
  
  fetch_ggproto <- function(x, name) {
    res <- NULL
    val <- .subset2(x, name)
    # The is.null check is an optimization for a common case; exists() also
    # catches the case where the value exists but has a NULL value.
    if (!is.null(val) || exists(name, envir = x, inherits = FALSE)) {
      res <- val
    } else {
      # If not found here, recurse into super environments
      super <- .subset2(x, "super")
      if (is.null(super)) {
        # no super class
      } else if (is.function(super)) {
        res <- fetch_ggproto(super(), name)
      } else { stop ("unknown super")}
    }
    res
  }
  
  # $ method to access obj
  dollarMethod = function(x, name) {
    res <- fetch_ggproto(x, name)
    ####                              ####
    
    if (!is.function(res)) {
      return(res)
    } 
    
    # add self parameter to the method
    args <- formals(res)
    # print(args)
    args$self = x
    formals(res) = args
    class(res) <- `_class`
    return(res)
  }
  
  # export the dollarMethod to the parent environment
  dollarMethodName = paste0('$.', `_class`)
  assign(dollarMethodName, dollarMethod, envir = parent.env(environment()))
  return(e)
}