#' Select rows from a data frame according to reference IDs
#' @param x a data frame to select
#' @param ref reference IDs, by which the data frame x is to be filtered.
#' @param by the column name (or NULL meaning "row names"), by which the
#' selection is operated on.
#' @return a data frame filtered and ordered by \code{ref} on \code{by} column.
#' The result of thi function is similar to \code{\link{subset}} function. The
#' difference is that \code{\link{subset}} function maintains the row orders
#' in the data frame \code{x}, but \code{selectRows} maintains
#' the order or \code{ref}. \code{selectRows} does not work if there are
#' duplicates in both "ref" and "by" pointed column.
#' @export
#' @examples {
#' \dontrun{
#' ### Unduplicated "by"
#' x = data.frame(V1 = letters[1:15], b = rnorm(15))
#' # Unduplicated "ref"
#' selectRows(x, ref = c(19, "a", "d", "c", "b"), by = "V1")
#'
#' # Duplicated "ref"
#' selectRows(x, ref = c(19, "a", "a", "c", "b"), by = "V1")
#'
#' ref1 = sample(1:20, 8)
#' print(ref1)
#' selectRows(x, ref = ref1)
#'
#' ### Duplicated "by"
#' x = data.frame(V1 = rep(letters[1:5], 1:5), b = rnorm(15))
#' # Unduplicated "ref"
#' # debugonce(selectRows)
#' selectRows(x, ref = c(19, "a", "d", "c", "b"), by = "V1")
#'
#' # Duplicated "ref" (ERROR!)
#' selectRows(x, ref = c(19, "a", "a", "c", "b"), by = "V1")
#'}
#'}
selectRows = function(x, ref, by = NULL){
  if (is.null(by)){
    cat("Select by row names of x.\n")
    byID = rownames(x)
  } else {
    if (length(by) != 1){
      stop('"by" must be length of 1.')
    }
    if (!by %in% colnames(x)){
      stop("No '", by, "' was found in column names of x.")
    }
    byID = x[, by]
  }

  # are by or ref duplicated.
  by_is_repetitive = any(duplicated(byID))
  ref_is_repetitive = any(duplicated(ref))

  # filtering and ordering data.frame rows.
  if (!by_is_repetitive){
      id = match(ref, byID)
      res = x[na.omit(id),,drop = F]
  } else {
    if (!ref_is_repetitive){
      id = match(byID, ref)
      ord = order(na.omit(id))
      res = x[!is.na(id),,drop = F]
      res = res[ord, ,drop = F]
    } else {
      stop("selectRows doese not support selection for duplicated",
           " 'ref' given duplicated 'by' pointed column.")
    }
  }

  # The IDs not found
  if (is.null(by)){
    byID2 = rownames(res)
  } else {
    byID2 = res[, by]
  }
  notFoundRef = setdiff(ref, byID2)
  return(list(res = res, notFoundRef = notFoundRef))
}
