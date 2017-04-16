#' Data stage for syberia models
#'
#' TODO: Document this more
#' 
#' @param modelenv an environment. The persistent modeling environment.
#' @param munge_procedure a list. A list of mungepiece arguments,
#'    first preprocessed then passed to munge.
#' @export
data_stage <- function(modelenv, munge_procedure) {
  # preprocess_munge_procedure(munge_procedure)
  removed_steps <- vapply(munge_procedure, function(x) is(x, 'trigger'), logical(1))
  # TODO: sameAs/importFrom and butWith/except triggers
  # TODO: save trigger
  if ('monitor' %in% names(munge_procedure)) {
    if (is.function(munge_procedure$monitor) || munge_procedure$monitor==TRUE) { # only monitor = TRUE will enable monitoring

      # define the munge monitor
      if (is.function(munge_procedure$monitor)) {
        tmp_fn <- munge_procedure$monitor
        monitor <- function(df) {
          cat('\033[0;33m')
          tmp_fn(df) 
          cat('\033[0m')
          TRUE
        }
      } else {
        monitor <- function(df) {
          cat("\033[0;33m\t\tDimensions: ", dim(df)[1], ' x ', dim(df)[2], '\033[0m\n', sep='')
          TRUE
        }  
      }
      munge_procedure$monitor <- NULL
  
      # interleave mungesteps with monitor function
      n <- length(munge_procedure)
      i2 <- 2*(1:n)
      i1 <- i2 - 1 
      tmp <- list() 
      tmp[i1] <- munge_procedure
      names(tmp)[i1] <- names(munge_procedure)
      for (i in i2) {
        tmp[[i]] <- monitor 
        names(tmp)[i] <- "Monitor"
      }
      munge_procedure <- tmp
    }
  }

  stagerunner <- munge(modelenv, munge_procedure,
    stagerunner = list(remember = TRUE),
    train_only = TRUE # This refers to us not wanting to set the mungebits
                      # as "trained" when running this stageRunner, since we
                      # would like to be able to run them multiple times -- 
                      # the stageRunner used in the main syberia model run is
                      # only used for training.
  ) 
    
  stagerunner
}

