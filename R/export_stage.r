#' Export data stage for Syberia model process.
#'
#' @param export_options list. The available export options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
export_stage <- function(export_options) {
  if (!is.list(export_options)) # Coerce to a list using the default adapter
    export_options <- setNames(list(resource = export_options), default_adapter())

  build_export_stagerunner(export_options)
}

#' Build a stagerunner for exporting data with backup sources.
#'
#' @param export_options list. Nested list, one adapter per list entry.
#'   These adapter parametrizations will get converted to legitimate
#'   IO adapters. (See the "adapter" reference class.)  The object to
#'   be exported is denoted by a prefix: e.g. object.adapter.  If the
#'   prefix is omitted then the tundra container will be exported.
build_export_stagerunner <- function(export_options) {
  stages <- lapply(seq_along(export_options), function(index) {
    
    export_key <- names(export_options)[index]
    
    # two-letter prefix on adapter name indicates object to be exported
    # (no prefix indicates that the tundra container should be exported)
    key <- rev(strsplit(export_key, '.', fixed=TRUE)[[1]])
    adapter_name <- key[1]
    if (length(key)==1) {
      export_object_type <- "tundra_container"
    } else if (key[2]=="ac") {
      export_object_type <- "avant_container"
    } else {
      stop(paste0("No export object associated with adapter prefix \"", key[2], "\""))
    }
    
    # fetch the adapter
    adapter <- adapter_name %||% default_adapter()
    adapter <- fetch_adapter(adapter)
    opts <- export_options[[index]]

    # return a function for the stagerunner
    function(modelenv) {
      
      attempt <- suppressWarnings(suppressMessages( # TODO: (RK) Announce errors
        if (export_object_type=="tundra_container") {
          tryCatch(adapter$write(modelenv$model_stage$model, opts),
                   error = function(e) e)
        } else if (export_object_type=="avant_container") {
          # Fill this in
        }
      ))
      
      if (is(attempt, 'try-error')) {
        warning("Failed to export to ", sQuote(adapter$.keyword), " due to: ",
                attempt, call. = FALSE)
      }
      
    }
  })
  names(stages) <- vapply(stages, function(stage)
    paste0("Export to ", environment(stage)$adapter$.keyword), character(1))

  stages
}

