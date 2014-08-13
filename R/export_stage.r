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
          
          # process options
          if (!is.list(opts)) stop("Options for exporting avantContainer must be passed as list")
          if (! "path" %in% names(opts)) stop("Must provide path with avantContainer export options")
          if (!( "train_rows" %in% names(opts) || "trainpct" %in% names(opts) ))
            stop("Must provide either train_rows or trainpct with avantContainer export options")
          dep_var_name = opts$dep_var_name %||% 'dep_var'
          version <- opts$version %||% ''
          reference_version <- opts$reference_version %||% 'default/en-US/2.1.3'
          
          # retrieve data
          raw_data <- stagerunner:::treeSkeleton(
            active_runner()$stages$data)$first_leaf()$object$cached_env$data
          if (!is.null(opts$train_rows)) {
            train_data <- raw_data[opts$train_rows, ]
            test_data <- raw_data[-opts$train_rows, ]
          } else {
            train_data <- raw_data[1:(opts$trainpct * nrow(raw_data)), ]
            test_data <- raw_data[-c(1:(opts$trainpct * nrow(raw_data))), ]
          }
          munged_train_data <- 
            stagerunner:::treeSkeleton(active_runner()$stages$model)$object$cached_env$data
          
          # create the avant container
          ac <- avant::avantContainer$new(modelenv$model_stage$model, 
                                          version, 
                                          dep_var_name,
                                          train_data,
                                          test_data, 
                                          munged_train_data,
                                          reference_version)
          
          # write it to disk
          tryCatch(adapter$write(ac, opts$path),
                   error = function(e) e)
          
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

