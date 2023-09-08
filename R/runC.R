#' Run a C function based on C interface from R
#' 
#' This function is a wrapper function to help users run C functions using `.C` and `.Call`
#' interface without taking care of dynamic links.
#' @param RCall The R script to use either `.C` or `.Call` to call the C 
#' function(s) that is/are defined in `cfile` or `body`. Please note RCall is not a string, 
#' it is a standard R script, and it can only be put here, but cannot be assigned 
#' to a variable outside of the `runC` function. If you have multiple lines of R script here, 
#' then put the curly brackets `{}` arround them.
#' @param cfile a C code file where the C function that is called in `RCall` parameter 
#' is declared and defined.
#' @param body The C code that are stored as a character/string (vector). By default, it's an 
#' empty character. But if this parameter is provided with a non-empty character, 
#' the text in `includes` and `body` will be written into a temporary file (`tempfile()`), which, 
#' instead of `cfile`, will be compiled and loaded in R. Please note that, the `\n` 
#' in the source C code needs to be escaped as `\\n`, and `\t` as `\\t`, `\\` as `\\\\`, etc.
#' @param includes This includes "#include <...>" C codes ahead of `body`. Default is character().
#' @param ignoreBuildMessage logical, whether to ignore the compiling message during build, default is TRUE.
#' @param rebuild logical, whether to compile and rebuild the dynamic link file, default is TRUE.
#' @param unload logical, whether to unload the dynamic link file after finished, default is TRUE.
#' @returns It returns what `.cCall` would return.
#' @importFrom tools file_path_sans_ext file_ext
#' @examples
#' \dontrun{
#' ## Suppose you have a C file called `directory/file.c`, in which you have 
#' ## defined a C function called `void myCfun(int *x)`.
#' ## then you can call this function from R like this:
#' x = 1:5
#' run(RCall = {y = .C("myCfun", xname = x)$xname}, cfile = "directory/file.c")
#' # Here, x is processed by the C function `myCfun` and assigned to y.
#' # Because `.C` is used here, `myCfun` does not return anything to R, but `.C` function 
#' # returns all of the parameters (with/without modifications) that you passed to `myCfun` 
#' # as a list, either named or unnamed depending on whether you assign it a 
#' # name (here xname) in `.C` function.
#' 
#' 
#' ## Alternatively, you can pass C code to `body` and `includes` parameters.
#' ## Suppose you have a piece of C code (not a C file) as follow
#' '
#' #include<stdio.h>
#' void myCfun(int *x){
#'   printf("The first number of Your input is %d, ", x[0]);
#'   int y = 100;
#'   *x = y;
#'   printf("Your first number is changed to %d\n", x[0]);
#' }
#' '
#' ## Then you can assign it to `body` in runC, but remember to escape `\n` in the C code.
#' ccode = '
#' #include<stdio.h>
#' void myCfun(int *x){
#'   printf("The first number of Your input is %d, ", x[0]);
#'   int y = 100;
#'   *x = y;
#'   printf("Your first number is changed to %d\\n", x[0]); // escape the backslash
#' }
#' '
#' ## And then you can call `myCfun` as before.
#' x = 1:5
#' runC(RCall = {y = .C("myCfun", xname = x)$xname}, body = ccode)
#' print(y)
#' 
#' ## You can also put `#include<stdio.h>` in the `includes` parameter and the rest in `body`.
#' ccode2 = '
#' void myCfun(int *x){
#'   printf("The first number of Your input is %d, ", x[0]);
#'   int y = 100;
#'   *x = y;
#'   printf("Your first number is changed to %d\\n", x[0]); // escape the backslash
#' }
#' '
#' inclu2 = '
#' #include<stdio.h>
#' '
#' 
#' ## And then you can call `myCfun` as
#' x = 1:5
#' runC(RCall = {y = .C("myCfun", xname = x)$xname}, body = ccode2, includes = inclu2)
#' print(y)
#' 
#' 
#' # You can also use `.Call` function to call C functions, here is an example
#' ccode3 = '
#' SEXP myCfun(SEXP x){
#'   x = duplicate(x);
#'   int *vx = INTEGER(x);
#'   printf("The first number of Your input is %d, ", vx[0]);
#'   int y = 100;
#'   vx[0] = y;
#'   printf("Your first number is changed to %d\\n", vx[0]); // escape the backslash
#'   return(x);
#' }
#' '
#' 
#' inclu3 = '
#' #include <R.h>
#' #include <Rinternals.h>
#' #include <stdio.h>
#' 
#' '
#' x = 1:5
#' runC(RCall = {y = .Call("myCfun", x)}, body = ccode3, includes = inclu3)
#' print(y)
#' }
#' @export
#' 
runC = function(RCall = {.C("C_dist", x1, x2, nchannel, ncells, nnodes, xdist = 0.0)$xdist},
                cfile = "src/testC.c", 
                body = character(),
                includes = character(),
                ignoreBuildMessage = TRUE,
                rebuild = TRUE,
                unload = TRUE){
  stopifnot(is.character(includes), is.character(body))
  if(length(body) > 0){
    cfile = tempfile(fileext = ".c")
    writeLines(c(includes, "", body), cfile)
    #file.show(cfile)
    
    text_lines <- readLines(cfile)
    cat(text_lines, sep = "\n")
    
  }
  
  {
    stopifnot(file.exists(cfile))
    file = tools::file_path_sans_ext(cfile)
    ext = tools::file_ext(cfile)
    stopifnot(ext %in% c("c", "cpp"))
    libraryExtension = .Platform$dynlib.ext
    file_so = paste0(file, libraryExtension)
    if(file.exists(file_so) && rebuild){
      file.remove(file_so)
    }
    cmd = paste0("R CMD SHLIB ", file, ".", ext)
    suppressMessages(system(cmd, ignore.stdout = ignoreBuildMessage))
    dyn.load(file_so)
    on.exit(if(unload) dyn.unload(file_so))
  }
  res = RCall
  return(res)
}