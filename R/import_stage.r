#' Import data stage for Syberia model process.
#'
#' @param import_options list. The available import options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
import_stage <- function(import_options) {
  if (!is.list(import_options)) # Coerce to a list using the default adapter
    import_options <- setNames(list(resource = import_options), default_adapter())

  build_import_stagerunner(import_options)
}

#' Build a stagerunner for importing data with backup sources.
#'
#' @param import_options list. Nested list, one adapter per list entry.
#'   These adapter parametrizations will get converted to legitimate
#'   IO adapters. (See the "adapter" reference class.)
build_import_stagerunner <- function(import_options) {
  stages <- Reduce(append, lapply(seq_along(import_options), function(index) {
    adapter <- names(import_options)[index] %||% default_adapter()
    adapter <- fetch_adapter(adapter)
    opts <- import_options[[index]]

    setNames(list(function(modelenv) {
      # Only run if data isn't already loaded
      if (!'data' %in% ls(modelenv)) {
        attempt <- suppressWarnings(suppressMessages(
          tryCatch(adapter$read(opts), error = function(e) FALSE)))
        if (!identical(attempt, FALSE) && !identical(attempt, NULL)) {
          modelenv$import_stage$adapter <- adapter
          modelenv$data <- attempt
        }
      }
    }), adapter$.keyword)
  }))
  if (length(stages) > 0)
    names(stages) <- vapply(names(stages), function(stage_name)
      paste0("Import from ", gsub('/','.',as.character(stage_name),fixed=TRUE)), character(1))

  # Always verify the data was loaded correctly in a separate stageRunner step.

  stages <- append(stages,
    list("(Internal) Verify data was loaded" = function(modelenv) {
      if (!'data' %in% ls(modelenv))
        stop("Failed to load data from all data sources", call. = FALSE)
      
      # TODO: (RK) Move this somewhere else.
      modelenv$import_stage$variable_summaries <-
        statsUtils::variable_summaries(modelenv$data) 
    }))

  stages
}

