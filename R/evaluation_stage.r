#' Evaluation stage for syberia models.
#'
#' A helper stage for evaluating a binary classification
#' model according to the metrics: AUC, a confusion matrix, and a validation plot.
#'
#' The evaluation stage parameters in the syberia model file are described in the
#' evaluation_parameters argument.
#'
#' @param modelenv an environment. The persistent modeling environment.
#' @param evaluation_parameters list. These come from the syberia model and
#'    must be in the following format:
#'
#'    \itemize{
#'      \item output. The prefix of the CSV and PNG to which to output the results
#'        of the validation. For example, if you put "output/foo", then "output/foo.csv"
#'        will be a CSV file containing a data.frame with the columns "dep_var",
#'        "loan_id", and "score", where the latter refers to predicted score. On the other
#'        hand, "output/foo.png" will contain a decile validation plot of the results.
#'      \item percent. The percent of the data that was used for training. Currently,
#'        only sequential splits of training and validation are supported until
#'        syberia introduces a better mechanism for data partitions. The default is 0.8.
#'      \item dep_var. (Optional) The name of the dependent variable in the evaluated data.
#'        The default is "dep_var".
#'      \item cutoff. (Optional) A cutoff for binary classification predictions: above the
#'        cutoff means a prediction 1, and otherwise a 0. The default is 0.5.
#'     }
#' @export
#' @return a stageRunner that performs AUC, confusing matrix, and validation
#'   plotting.
evaluation_stage <- function(evaluation_parameters) {
  stopifnot('output' %in% names(evaluation_parameters))

  cutoff <- evaluation_parameters$cutoff %||% 0.5
  train_percent <- evaluation_parameters$percent %||% 0.8
  dep_var <- evaluation_parameters$dep_var %||% 'dep_var'

  generate_options <- function(modelenv) {
    # TODO: (RK) Remove this to use the IO adapter once that has been written.
    # In order to grab the data as what it looked like prior to any data preparation,
    # we are going to extract it from the cached environment of the first step in the
    # data stage. This way, it will be import-method-agnostic, and we will not
    # have to worry whether our data came from CSV, S3, etc. We also assume the
    # stageRunner we are attached to is in `active_runner()`.
    raw_data <- stagerunner:::treeSkeleton(
      active_runner()$stages$data)$first_leaf()$object$cached_env$data

    validation_rows <- seq(train_percent * nrow(raw_data) + 1, nrow(raw_data))
    validation_data <- raw_data[validation_rows, ]
    score <- modelenv$model_stage$model$predict(validation_data)

    modelenv$evaluation_stage$prediction_data <-
      data.frame(dep_var = validation_data[[dep_var]],
                 score = score, loan_id = validation_data$loan_id)
   # TODO: (RK) WTF is loan_id doing in a general evaluation stage??

    if ('output' %in% names(evaluation_parameters)) {
      write.csv(paste0(prediction, '.csv'), evaluation_parameters$output, row.names = FALSE)
    }
  }
  
  auc <- function(modelenv) {
    require(AUC)
    modelenv$evaluation_stage$auc <- with(modelenv$evaluation_stage$prediction_data, {
      list(
        roc = AUC::auc(roc(score, factor(dep_var))),
        accuracy = AUC::auc(accuracy(score, factor(dep_var))),
        sensitivity = AUC::auc(sensitivity(score, factor(dep_var))),
        specificity = AUC::auc(specificity(score, factor(dep_var)))
    )})
    print(modelenv$evaluation_stage$auc)
  }
  
  confusion_matrix <- function(modelenv) {  
    confusion_matrix_arguments <- 
      list(modelenv$evaluation_stage$prediction_data, cutoff,
           plot.it = TRUE, xlab = c("dep_var = 0", "dep_var = 1"),
           ylab = c("score = 0", "score = 1"), title = NULL)
    do.call(confusion_matrix, confusion_matrix_arguments)
  }
  
  validation_plot <- function(modelenv){
    with(modelenv$evaluation_stage$options$prediction, {
      ordered_scores <- modelenv$evaluation_stage$prediction_data[
        order(modelenv$evaluation_stage$prediction_data$score), ]
      xs <- (10*(0:9) + 9) / 100 # TODO: (RK) Make a parameter for this
      ys <- sapply(xs, function(x) {
        xrows <- seq((nrow(ordered_scores) * max(x - (xs[2] - xs[1]), 0) + 1),
                     (nrow(ordered_scores) * x))
        sum(ordered_scores[xrows, 'dep_var']) / length(xrows)
      })
      
      png(filename = paste0(prediction, '.png'))
      plot(xs, ys, type = 'l', col = 'darkgreen',
           main = 'Dependent variable capture v.s. score deciles',
           xlab = '% of samples ordered by model score',
           ylab = '% of dependent variable = 1',
           frame.plot = TRUE, lwd = 3, cex = 2)
      dev.off()
      NULL
    })
  }
  
  # This list of functions will be incorporated into the full model stageRunner
  list('(Internal) Generate evaluation options' = generate_options,
       auc = auc,
       'confusion matrix' = confusion_matrix,
       'validation plot' = validation_plot)
}

#' Plot a confusion matrix for a given prediction set, and return the table.
#' 
#' @param dataframe data.frame. Must contain \code{score} and \code{dep_var}
#'    columns. The confusion matrix will be calculated for these values.
#'    The mentioned columns must both be numeric.
#' @param cutoff numeric. The cutoff at which to assign numbers greater a 1
#'    for prediction purposes, and 0 otherwise. The default is 0.5.
#' @param plot.it logical. Whether or not to plot the confusion matrix as a
#'    four fold diagram. The default is \code{TRUE}.
#' @param xlab character. The labels for the rows (\code{dep_var}). The default
#'    is \code{c("dep_var = 0", "dep_var = 1")}.
#' @param ylab character. The labels for the rows (\code{score}). The default
#'    is \code{c("score = 0", "score = 1")}.
#' @param title character. The title for the fourfoldplot, if it is graphed.
#' @return a table. The confusion matrix table.
confusion_matrix <- function(dataframe, cutoff = 0.5, plot.it = TRUE,
                             xlab = c("dep_var = 0", "dep_var = 1"),
                             ylab = c("score = 0", "score = 1"), title = NULL) {
  stopifnot(is.data.frame(dataframe) &&
            all(c('score', 'dep_var') %in% colnames(dataframe)))
  stopifnot(is.numeric(dataframe$score) && is.numeric(dataframe$dep_var))

  dataframe$score <- ifelse(dataframe$score <= cutoff, 0, 1)
  categories <- dataframe$score * 2 + dataframe$dep_var
  confusion <- matrix(tabulate(1 + categories, 4), nrow = 2)
  colnames(confusion) <- ylab
  rownames(confusion) <- xlab
  if (plot.it) fourfoldplot(confusion, color = c("#CC6666", "#99CC99"),
                            conf.level = 0, margin = 1, main = title)
  confusion 
}

