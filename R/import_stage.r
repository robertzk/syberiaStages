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


#' Fetch the default adapter keyword from the active syberia
#' project's configuration file.
#'
#' @return a string representing the default adapter.
default_adapter <- function() {
  # TODO: (RK) Multi-syberia projects root tracking?

  # Grab the default adapter if it is not provided from the Syberia
  # project's configuration file. If no default is specified there,
  # we will assume we're reading from a file.
  default_adapter <- syberia_config()$default_adapter %||% 'file'
}

#' Build a stagerunner for importing data with backup sources.
#'
#' @param import_options list. Nested list, one adapter per list entry.
#'   These adapter parametrizations will get converted to legitimate
#'   IO adapters. (See the "adapter" reference class.)
build_import_stagerunner <- function(import_options) {
  stages <- lapply(seq_along(import_options), function(index) {
    stage <- function(modelenv) {
      # Only run if data isn't already loaded
      if (!'data' %in% ls(modelenv)) {
        attempt <- suppressWarnings(suppressMessages(
          tryCatch(adapter$read(opts), error = function(e) FALSE)))
      }
    }
    
    # Now attach the adapter and options to the above closure.
    adapter <- names(import_options)[index] %||% opts$adapter
    environment(stage)$fn <- import_adapter(adapter)
    environment(stage)$opts <- opts
    environment(stage)$adapter <- adapter
    stage
  })
  names(stages) <-
    vapply(stages, function(stage)
                     paste0("Import from ", environment(stage)$adapter),
           character(1))

  stages[[length(stages) + 1]] <- function(modelenv) {
    if (!'data' %in% ls(modelenv))
      stop("Failed to load data from all data sources")
    
    # TODO: (RK) Move this somewhere else.
    modelenv$import_stage$variable_summaries <-
      statsUtils::variable_summaries(modelenv$data) 
  }
  names(stages)[length(stages)] <- "(Internal) Verify data was loaded" 

  if ('skip' %in% names(meta_options)) {
    stages <- append(list("(Internal) Import model from a global variable" = function(modelenv) {
      stopifnot(is.character(meta_options$skip))
      if (!exists(meta_options$skip, envir = globalenv(), inherits = FALSE)) return()
      modelenv$data <- get(meta_options$skip, envir = globalenv())
      #copy is assigned to the global environment which is a local copy of the trained model
    }), stages)
  }

  stages
}

#' Fetch an import adapter.
#'
#' @param adapter character. Only supported so far are 's3' and 'file'.
#'    The default is 'file'.
#' @param opts list. The options that get passed to the import adapter.
import_adapter <- function(adapter = 'file') {
  stopifnot(is.character(adapter))
  adapter <- tolower(adapter)
  # TODO: Given the similarities, if most future adapters are similarly,
  # maybe generate these using an adapter_template
  if (adapter == 's3') {
    function(modelenv, opts) {
      require(s3mpi)
      if (is.character(opts)) opts <- list(file = opts)
      filename <- opts$file %||% opts$filename %||% opts$name %||% opts$path
      stopifnot(is.character(filename))
      modelenv$data <- s3read(filename)
      modelenv$import_stage$file <- filename
    }
  } else {
    function(modelenv, opts) {
      if (is.character(opts)) opts <- list(file = opts)
      filename <- opts$file %||% opts$filename %||% opts$name %||% opts$path
      stopifnot(is.character(filename))
      modelenv$data <- read.csv(filename, stringsAsFactors = FALSE)
      modelenv$import_stage$file <- filename
    }
  }
}

# A reference class to abstract importing and exporting data.
adapter <- setRefClass('adapter',
  list(.read_function = 'function', .write_function = 'function',
       .format_options = 'function', .default_options = 'list'),
  methods = list(
    initialize = function(read_function, write_function,
                          format_options = identity, default_options = list()) { 
      .read_function <<- read_function
      .write_function <<- write_function
      .format_options <<- format_options
      .default_options <<- default_options
    },

    read = function(options = list()) {
      .read_function(format_options(options))
    },

    write = function(options = list()) {
      .write_function(format_options(options))
    },

    format_options = function(options) {
      if (!is.list(options)) options <- list(resource = options)

      # Merge in default options if they have not been set.
      for (i in seq_along(.default_options))
        if (!is.element(name <- names(.default_options)[i], names(options)))
          options[[name]] <- .default_options[[i]]

      .format_options(options)
    }
  )
)

