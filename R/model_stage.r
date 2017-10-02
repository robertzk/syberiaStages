#' Model stage for syberia models
#'
#' TODO: Document this more
#' 
#' @param model_parameters a list. Model-specific parameters, with the first
#'    parameter always being the model keyword for the tundra container
#'    (e.g., glm, gbm, etc.)
#' @export
model_stage <- function(model_parameters) {
  stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
  model_fn <- fetch_model_container(model_parameters[[1]])

  # Remove the model keyword (e.g., "gbm", "glm", etc.)
  model_parameters[[1]] <- NULL

  webbank_variables <- readLines(file.path(syberia_root(), 'etc',
    'webbank', 'disallowed_variables'), warn = FALSE)
  
  webbank_variables_TU <- readLines(file.path(syberia_root(), 'etc',
    'webbank', 'disallowed_TU_variables'), warn = FALSE)

  webbank_variables <- union(webbank_variables, webbank_variables_TU)

  function(modelenv) {
    # TODO: (RK) Move this out of syberia package!!!
    if (any(vapply(badv <- tolower(webbank_variables), is.element,
                   logical(1), set = allv <- tolower(colnames(modelenv$data))))) {
      stop("You are using disallowed webbank variables: \n",
           paste(intersect(badv, allv), collapse = "\n"),
           call. = FALSE)
    }


    # Track variable summaries
    summaries <- modelenv$import_stage$variable_summaries
    summaries <- lapply(summaries,
      function(vars) vars[setdiff(intersect(names(vars), colnames(modelenv$data)), 'dep_var')]
    )
    # TODO: Remove unimportant variables so they do not trigger
    # velocity check. For this, we need a model-agnostic variable
    # importance measure. Maybe add a hack for GBM first.

    # Instantiate tundra container for model
    modelenv$model_stage$model <-
      model_fn(list(), model_parameters, list(variable_summaries = summaries))

    # Train the model
    modelenv$model_stage$model$train(modelenv$data, verbose = TRUE)
    
    # Manually skip munge procedure since it was already done
    modelenv$model_stage$model$munge_procedure <-
      attr(modelenv$data, 'mungepieces') %||% list()
    # Since munge was called with train_only, the mungebits are incapable of
    # getting predicted. The line below remedies this.
    for (ix in seq_along(modelenv$model_stage$model$munge_procedure))
      modelenv$model_stage$model$munge_procedure[[ix]]$bit$trained <- TRUE
  }
}


#' Fetch a tundra model container.
#'
#' The container is fetched either from the tundra package or from
#' the \code{lib/classifiers} directory in the syberia project.
#'
#' If there is no tundra model associated to the keyword (like 'gbm'
#' or 'regularization'), you must place a function called \code{train}
#' and a function called \code{predict} in the an R file with the same
#' name as the keyword for your classification. For example,
#' if you are implementing least-angle regression, you could define
#' \code{lib/classifiers/lar.R} and from your syberia model use
#' \code{'lar'} for your model keyword.
#'
#' @param type character. The model keyword. This function will attempt
#'   to fetch the associated container construction function (with
#'   parameters \code{munge_procedure}, \code{default_args}, and
#'   \code{internal} from either (1) the tundra package, or (2)
#'   your syberia project's \code{lib/classifiers} directory (see
#'   description).
#' @return A container construction function (with
#'   parameters \code{munge_procedure}, \code{default_args}, and
#'   \code{internal} which takes these and return a \code{tundra_container}
#'   object with a \code{train} and \code{predict} method.
#' @export
fetch_model_container <- function(type) {
  # TODO: (RK) Should we be using syberia_objects for this?
  base <- file.path(syberia_root(), 'lib', 'classifiers')
  potential_object <-
    syberiaStructure:::syberia_objects(type, base = base, fixed = TRUE)
  if (length(potential_object) == 0) {
    if (exists(model_fn <- pp('tundra_#{type}'))) return(get(model_fn))
    stop("Missing tundra container for keyword '", type, "'. ",
         "It must exist in the tundra package or be present in ",
         pp("lib/classifiers/#{type}.R"), call. = FALSE)
  } else if (length(potential_object) > 1) {
    stop("Found multiple classifiers with keyword ", sQuote(type), ", namely: ",
         paste0(potential_object, collapse = ', '), call. = FALSE)
  } else {
    filename <- file.path(base, potential_object)
  }
  
  provided_env <- new.env()
  syberiaStructure:::syberia_resource_with_modification_tracking(
    filename, root = syberia_root(filename), provides = provided_env)$value()
  provided_functions <- parse_custom_classifier(provided_env, type)

  function(munge_procedure = list(), default_args = list(), internal = list()) {
    tundra:::tundra_container$new(type, provided_functions$train, provided_functions$predict,
                                  munge_procedure, default_args, internal)
  }
}

#' Ensures a custom classifier is valid and returns its train and predict methods.
#'
#' There can only be one function defined that contains the string "train".
#' Similarly there can only be one such function containing "predict".
#' If this condition is not met, this function will throw an error.
#'
#' @param provided_env environment. The environment the classifier was loaded from.
#' @param type character. The keyword for the classifier.
#' @return a list containing keys "train" and "predict" indicating the train
#'    and predict functions.
parse_custom_classifier <- function(provided_env, type) {
  parse_custom_functions(c('train', 'predict'), provided_env, type, 'classifier')
}

