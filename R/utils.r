`%||%` <- function(x, y) if (is.null(x)) y else x

#' Merge two lists and overwrite latter entries with former entries
#' if names are the same.
#'
#' For example, \code{list_merge(list(a = 1, b = 2), list(b = 3, c = 4))}
#' will be \code{list(a = 1, b = 3, c = 4)}.
#' @param list1 list
#' @param list2 list
#' @return the merged list.
#' @examples
#' stopifnot(identical(syberiaStages:::list_merge(list(a = 1, b = 2), list(b = 3, c = 4)),
#'                     list(a = 1, b = 3, c = 4)))
#' stopifnot(identical(syberiaStages:::list_merge(NULL, list(a = 1)), list(a = 1)))
# TODO: (RK) This is a duplicate of the function in mungebits -- is there
# any way to pull them out into one place? Maybe Ramd?
list_merge <- function(list1, list2) {
  list1 <- list1 %||% list()
  # Pre-allocate memory to make this slightly faster.
  list1[Filter(function(x) nchar(x) > 0, names(list2) %||% c())] <- NULL
  for (i in seq_along(list2)) {
    name <- names(list2)[i]
    if (!identical(name, NULL) && !identical(name, "")) list1[[name]] <- list2[[i]]
    else list1 <- append(list1, list(list2[[i]]))
  }
  list1
}

#' Parse functions out of a custom resource.
#'
#' @param functions character. The names of functions to parse out.
#' @param provided_env environment. The environment the resource was loaded from.
#' @param type character. The keyword for the resource.
#' @param resource_type character. The type of the resource (e.g., "classifier",
#'   "adapter", etc.). This will be used to generate error messages.
#' @param strict logical. Whether or not to error if the functions are not found.
#' @return a list containing keys same as the \code{functions} argument.
#'    and predict functions.
parse_custom_functions <- function(functions, provided_env, type,
                                   resource_type = 'classifier', strict = TRUE) {
  provided_fns <- setNames(vector('list', length(functions)), functions)
  for (function_type in names(provided_fns)) {
    fn <- Filter(
      function(x) is.function(provided_env[[x]]),
      grep(function_type, ls(provided_env), value = TRUE)
    )
    # TODO: (RK) Refactor this to be more careful about idempotent resources.
    error <- function(snip = 'a') paste0("The custom ", resource_type, " in ",
      "lib/", resource_type, "s/", type, ".R should define ", snip, " '",
      testthat::colourise(function_type, 'green'), "' function.")
    if (length(fn) == 0 && identical(strict, TRUE)) stop(error(), call. = FALSE)
    else if (length(fn) > 1)
      stop(error('only one'), " Instead, you defined ", length(fn), ", namely: ",
           paste0(fn, collapse = ', '), call. = FALSE)
    else if (length(fn) == 1)
      provided_fns[[function_type]] <- provided_env[[fn]]
  }
  provided_fns
}

#' Helper function to serialize a tundraContainer of xgboost model object
#'
serialize_xgb_object <- function(object) {
	file_save <- tempfile()
  on.exit(unlink(file_save))
  xgboost::xgb.save(object$output$model, file_save)
  stopifnot(!is.na(as.integer(file.info(file_save)$size)))
  object$output$model <- NULL
  con <- file(file_save, 'rb')
  on.exit(close(con), add = TRUE)
  object <- structure(
	            class = 'special_serialized_object', 
							list(
							  deserialize = function(x) {
                  file_load <- tempfile()
                  on.exit(unlink(file_load))
                  con <- file(file_load, 'wb')
                  on.exit(close(con), add = TRUE)
                  writeBin(x$xgb.bin, con, useBytes = TRUE)
                  x$container$output$model <- xgboost::xgb.load(file_load)
                  invisible(x$container)
                }, 
								object = list(container = object, 
							                xgb.bin = readBin(con, raw(), 
															                  n = file.info(file_save)$size))
							)
					  )
	invisible(object)
}
