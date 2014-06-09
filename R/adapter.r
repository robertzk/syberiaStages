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

#' Fetch a syberia IO adapters.
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
  if (!is.element(keyword, names(adapters))) {
    if (is.null(adapters)) adapters <- list()
    # TODO: (RK) Compile just-in-time adapters
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

fetch_customer_adapter <- function() {
      stop("There is no adapter ", sQuote(keyword), " for reading and ",
           "writing data. The available adapters are: ",
          paste0(names(built_in_adapters), collapse = ', '), call. = FALSE)
}

#' A helper function for formatting parameters for adapters to
#' correctly include an argument "file", with aliases
#' "resource", "filename", "name", and "path".
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
    read_csv_params <- c('header', 'sep', 'quote', 'dec', 'fill', 'comment.char',
                         'stringsAsFactors')
    args <- list_merge(list(file = opts$resource, stringsAsFactors = FALSE),
                       opts[read_csv_params])
    do.call(read.csv, args)
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

    # If the user provided an s3 path, like "s3://somebucket/some/path/", 
    # pass it along to the s3read function.
    args <- list(obj = object, name = opts$resource)
    if (is.element('s3path', names(opts))) args$.path <- opts$s3path
    do.call(s3mpi::s3read, args)
  }

  # TODO: (RK) Read default_options in from config, so a user can
  # specify default options for various adapters.
  adapter(read_function, write_function, format_function = common_file_formatter,
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
      .read_function(format_function(options))
    },

    write = function(value, options = list()) {
      .write_function(value, format_function(options))
    },

    store = function(...) { write(...) },

    format_function = function(options) {
      if (!is.list(options)) options <- list(resource = options)

      # Merge in default options if they have not been set.
      for (i in seq_along(.default_options))
        if (!is.element(name <- names(.default_options)[i], names(options)))
          options[[name]] <- .default_options[[i]]

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

