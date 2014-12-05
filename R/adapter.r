#' Fetch the default adapter keyword from the active syberia
#' project's configuration file.
#'
#' @return a string representing the default adapter.
default_adapter <- function() {
  # TODO: (RK) Multi-syberia projects root tracking?

  # Grab the default adapter if it is not provided from the Syberia
  # project's configuration file. If no default is specified there,
  # we will assume we're reading from a file.
  default_adapter <-
    (if (!is.null(syberia_root())) syberia_config()$default_adapter) %||% 'file'
}

#' Fetch a syberia IO adapter.
#'
#' IO adapters are (reference class) objects that have a \code{read}
#' and \code{write} method. By wrapping things in an adapter, you do not have to
#' worry about whether to use, e.g., \code{read.csv} versus \code{s3read}
#' or \code{write.csv} versus \code{s3store}. If you are familiar with
#' the tundra package, think of adapters as like tundra containers for
#' importing and exporting data.
#'
#' For example, we can do: \code{fetch_adapter('file')$write(iris, '/tmp/iris.csv')}
#' and the contents of the built-in \code{iris} data set will be stored
#' in the file \code{"/tmp/iris.csv"}.
#'
#' @param keyword character. The keyword for the adapter (e.g., 'file', 's3', etc.)
#' @return an \code{adapter} object (defined in this package, syberiaStages)
fetch_adapter <- function(keyword) {
  adapters <- syberiaStructure:::get_cache('adapters')
  keyword <- tolower(keyword)
  is_built_in <- is.element(keyword, names(built_in_adapters))
  if (!is.element(keyword, names(adapters)) ||
      (!is_built_in && fetch_custom_adapter(keyword, modified_check = TRUE))) {
    # If this adapter is not cached, or is a custom adapter and has been
    # modified since being cached, re-compute it.

    if (is.null(adapters)) adapters <- list()
    new_adapter <-
      if (is.element(keyword, names(built_in_adapters)))
        built_in_adapters[[keyword]]()
      else fetch_custom_adapter(keyword)
    adapters[[keyword]] <- new_adapter
    syberiaStructure:::set_cache(adapters, 'adapters')
  }

  # TODO: (RK) Should we re-compile the adapter if the syberia config
  # changed, or force the user to restart R/syberia?
  adapters[[keyword]]
}

#' Publically exported version of \code{fetch_adapter}.
#'
#' @param keyword character. The keyword for the adapter (e.g., 'file', 's3', etc.)
#' @export
#' @seealso \code{\link{fetch_adapter}}
fetch_syberia_adapter <- fetch_adapter

#' Fetch a custom syberia IO adapter.
#'
#' Custom adapters are defined in \code{lib/adapters} from the root
#' of the syberia project. Placing a file there with, for example, name 'foo.R',
#' will cause \code{fetch_custom_adapter('foo')} to return an appropriate
#' IO adapter. The file 'foo.R' must contain a 'read', 'write', and (optionally)
#' 'format' function, which will be used to construct the adapter. (See
#' the definition of the adapter reference class.)
#'
#' @param keyword character. The keyword for the adapter (e.g., 'file', 's3', etc.)
#' @param modified_check logical. If \code{TRUE}, will return a logical indicating
#'    whether or not the customer adapter has been modified. By default, \code{FALSE}.
#' @return an \code{adapter} object (defined in this package, syberiaStages)
fetch_custom_adapter <- function(keyword, modified_check = FALSE) {
  # TODO: (RK) Better multi-project support
  adapters_path <- file.path(syberia_root(), 'lib', 'adapters')
  valid_adapters <- vapply(syberia_objects('', adapters_path), function(x)
    tolower(gsub("\\.[rR]$", "", x)), character(1))

  if (!is.element(keyword, valid_adapters))
    stop("There is no adapter ", sQuote(keyword), " for reading and ",
         "writing data. The available adapters are: ",
         paste0(c(names(built_in_adapters), valid_adapters), collapse = ', '),
         call. = FALSE)

  provided_env <- new.env()
  adapter_index <- which(valid_adapters == keyword)[1]
  adapter_file <- names(valid_adapters)[adapter_index]
  filename <- file.path(adapters_path, adapter_file)
  resource <- syberiaStructure:::syberia_resource_with_modification_tracking(
    filename, root = syberia_root(filename), provides = provided_env, body = FALSE)

  if (identical(modified_check, FALSE)) {
    resource$value()
    parse_custom_adapter(provided_env, valid_adapters[adapter_index])
  } else resource$modified
}

#' Ensures a custom adapter resource is valid and returns the corresponding
#' adapter reference class object.
#'
#' There can only be one function defined that contains the string "read".
#' Similarly there can only be one such function containing "write".
#' If this condition is not met, this function will throw an error.
#' Finally, there is also an optional "format" function that can be defined.
#'
#' @param provided_env environment. The environment the adapter was loaded from.
#' @param type character. The keyword for the adapter.
#' @return the \code{adapter} reference class object constructed from the parsed
#'    adapter resource.
parse_custom_adapter <- function(provided_env, type) {
  args <- parse_custom_functions(c('read', 'write'), provided_env, type, 'adapter')
  names(args) <- c('read_function', 'write_function')
  format_fn <- parse_custom_functions(c('format'), provided_env,
                                      type, 'adapter', strict = FALSE)
  if (!is.null(format_fn$format)) args$format_function <- format_fn$format
  args$keyword <- type

  # TODO: (RK) Read defaults for adapter from syberia project config file.
  do.call(adapter$new, args)
}

#' A helper function for formatting parameters for adapters to
#' correctly include an argument "file", with aliases
#' "resource", "filename", "name", and "path".
#'
#' @param opts list. The options that will get passed to the adapter
#'   constructor function.
#' @return the fixed and sanitized formatted options.
common_file_formatter <- function(opts) {
  if (!is.element('resource', names(opts))) {
    filename <- opts$file %||% opts$filename %||% opts$name %||% opts$path
    if (is.null(filename))
      stop("You are trying to read from ", sQuote(.keyword), ", but you did ",
           "not provide a file name.", call. = FALSE)
    opts$resource <- filename
  }
  if (!is.character(opts$resource))
    stop("You are trying to read from ", sQuote(.keyword), ", but you provided ",
         "a filename of type ", sQuote(class(opts$resource)[1]), " instead of ",
         "a string. Make sure you are passing a file name ",
         "(for example, 'example/file.csv')", call. = FALSE)
  opts
}

#' Construct a file adapter.
#'
#' @return an \code{adapter} object which reads and writes to a file.
construct_file_adapter <- function() {
  read_function <- function(opts) {
    # If the user provided any of the options below in their syberia model,
    # pass them along to read.csv
    if ('.rds' == substring(opts$resource, nchar(opts$resource) - 3, nchar(opts$resource)))
      readRDS(opts$resource)
    else {
      read_csv_params <- c('header', 'sep', 'quote', 'dec', 'fill', 'comment.char',
                           'stringsAsFactors')
      args <- list_merge(list(file = opts$resource, stringsAsFactors = FALSE),
                         opts[read_csv_params])
      do.call(read.csv, args)
    }
  }

  write_function <- function(object, opts) {
    # If the user provided any of the options below in their syberia model,
    # pass them along to write.csv
    if (is.data.frame(object)) {
      write_csv_params <- setdiff(names(formals(write.table)), c('x', 'file'))
      args <- list_merge(
        list(x = object, file = opts$resource, row.names = FALSE),
        opts[write_csv_params])
      do.call(write.csv, args)
    } else {
      save_rds_params <- setdiff(names(formals(saveRDS)), c('object', 'file'))
      args <- list_merge(list(object = object, file = opts$resource),
                         opts[save_rds_params])
      do.call(saveRDS, args)
    }
  }

  # TODO: (RK) Read default_options in from config, so a user can
  # specify default options for various adapters.
  adapter(read_function, write_function, format_function = common_file_formatter,
          default_options = list(), keyword = 'file')
}

#' Construct an Amazon Web Services S3 adapter.
#'
#' This requires that the user has set up the s3mpi package to
#' work correctly (for example, the s3mpi.path option should be set).
#' (Note that this adapter is not related to R's S3 classes).
#'
#' @return an \code{adapter} object which reads and writes to Amazon's S3.
construct_s3_adapter <- function() {
  load_s3mpi_package <- function() {
    if (!'s3mpi' %in% installed.packages())
      stop("You must install and set up the s3mpi package from ",
           "https://github.com/robertzk/s3mpi", call. = FALSE)
    require(s3mpi)
  }

  read_function <- function(opts) {
    load_s3mpi_package()

    # If the user provided an s3 path, like "s3://somebucket/some/path/", 
    # pass it along to the s3read function.
    args <- list(name = opts$resource)
    if (is.element('s3path', names(opts))) args$.path <- opts$s3path
    do.call(s3mpi::s3read, args)
  }

  write_function <- function(object, opts) {
    load_s3mpi_package()

    # Hack for model object requiring customized
    # serializer, e.g., xgb.Booster
    if (is(object, 'tundraContainer') &&
        is(object$output$model, 'xgb.Booster')) {
			object <- serialize_xgb_object(object)
    }

    # If the user provided an s3 path, like "s3://somebucket/some/path/", 
    # pass it along to the s3read function.
    args <- list(obj = object, name = opts$resource)
    if (is.element('s3path', names(opts))) args$.path <- opts$s3path
    do.call(s3mpi::s3store, args)
  }

  format_function <- function(opts) {
    environment(common_file_formatter) <- environment()
    opts <- common_file_formatter(opts)
    if (is.element('bucket', names(opts)))
      opts$s3path <- paste0("s3://", opts$bucket, "/")
    opts
  }

  # TODO: (RK) Read default_options in from config, so a user can
  # specify default options for various adapters.
  adapter(read_function, write_function, format_function = format_function,
          default_options = list(), keyword = 's3')
}

#' Construct an adapter for reading to and from an R environment,
#' by default the global environment.
#'
#' @return an \code{adapter} object which reads and writes to Amazon's S3.
construct_R_adapter <- function() {
  read_function <- function(opts) {
    get(opts$resource, envir = opts$env) # TODO: (RK) Support "inherits"?
  }

  write_function <- function(object, opts) {
    assign(opts$resource, object, envir = opts$env)
  }

  adapter(read_function, write_function, format_function = common_file_formatter,
          default_options = list(env = globalenv()), keyword = 'R')
}

# A reference class to abstract importing and exporting data.
adapter <- setRefClass('adapter',
  list(.read_function = 'function', .write_function = 'function',
       .format_function = 'function', .default_options = 'list', .keyword = 'character'),
  methods = list(
    initialize = function(read_function, write_function,
                          format_function = identity, default_options = list(),
                          keyword = character(0)) { 
      .read_function <<- read_function
      .write_function <<- write_function
      .format_function <<- format_function
      .default_options <<- default_options
      .keyword <<- keyword
    },

    read = function(options = list()) {
      .read_function(format(options))
    },

    write = function(value, options = list()) {
      .write_function(value, format(options))
    },

    store = function(...) { write(...) },

    format = function(options) {
      if (!is.list(options)) options <- list(resource = options)

      # Merge in default options if they have not been set.
      for (i in seq_along(.default_options))
        if (!is.element(name <- names(.default_options)[i], names(options)))
          options[[name]] <- .default_options[[i]]

      environment(.format_function) <<- environment()
      .format_function(options)
    },

    show = function() {
      has_default_options <- length(.default_options) > 0
      cat("A syberia IO adapter of type ", sQuote(.keyword), ' with',
          if (has_default_options) '' else ' no', ' default options',
          if (has_default_options) ': ' else '.', "\n", sep = '')
      if (has_default_options) print(.default_options)
    }
  )
)

built_in_adapters <- list(file = construct_file_adapter,
                          s3 = construct_s3_adapter,
                          r = construct_R_adapter)

