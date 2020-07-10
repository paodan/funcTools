#' An object constructor of an S3 class
#' 
#' This is inspired by the object oriented programming in Javascript and ggproto 
#' function/object in ggplot2 package. Here the attributes of the 
#' object are replaced in place, and it is able to access the object it"self" 
#' and its parent object in the method of this object.
#' 
#' @param `_class` of which class an object to create
#' @param `_inherit` the parent class an object to inherit
#' @param pos into which environment the $ method (see value part) is assigned. 
#' The pos argument can specify the environment in which to assign the object 
#' in any of several ways: as a positive integer 
#' (the position in the search list, default is 1); as the character string name of an 
#' element in the search list; or as an environment (including using sys.frame 
#' to access the currently active function calls).
#' 
#' @return an instance of `_class` object. Meanwhile, an S3 method of `$` 
#' function is generated in the same environment of the object that you just
#' constructed. For example, if the "NewClass" is assigned to `_class` 
#' parameter, then a function/method called $.NewClass is constructed.
#' 
#' @examples 
#' \dontrun{
#' Adder <- newObject("Adder", 
#'                    x = 0,
#'                    add = function(n) {
#'                      self$x <- self$x + n
#'                      self$x
#'                    }
#' )
#' Adder$x
#' Adder$add(10)
#' # Adder$x is replaced in place
#' Adder$add(10)
#' 
#' Doubler <- newObject("Doubler", Adder,
#'                      add = function(n) {
#'                        self$x = self$x *(n) * self$super()$x
#'                        self$x
#'                      }
#' )
#' Doubler$x
#' Adder$x
#' Doubler$add(10)
#' Doubler$self()
#' 
#' identical(Adder, Doubler$super())
#' Adder$self() # Adder
#' Doubler$super()$self() # Adder
#' 
#' # update Adder
#' Adder$add(10)
#' Adder$x
#' # the super of Doubler is also update
#' Doubler$super()$x
#' # But Double itself is not changed
#' Doubler$x
#' }
#' @export
newObject = function(`_class` = NULL, `_inherit` = NULL, pos = 1,
                     ...){
  e <- new.env(parent = emptyenv())
  
  members <- list(..., self = function(self) {
    print(base:::as.list.environment(self))
    return(invisible(self))
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
  # assign(dollarMethodName, dollarMethod, envir = parent.env(environment()))
  assign(dollarMethodName, dollarMethod, pos = pos)
  return(e)
}
