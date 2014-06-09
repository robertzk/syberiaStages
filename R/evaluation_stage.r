#' Evaluation stage for binary classification syberia models.
#'
#' A helper stage for evaluating a binary classification model
#' according to the metrics: AUC, a confusion matrix, and a validation plot.
#'
#' The evaluation stage parameters that can be controlled through the
#' syberia model file are described in the evaluation_parameters argument.
#' For example, just like how you can set \code{import = list(...)} to
#' control what data gets imported in your syberia model, you can
#' write \code{evaluation = list(output = 'foo', percent = 0.6, ...)} to
#' control what happens during this evaluation_stage.
#'
#' @param evaluation_parameters list. These come from the syberia model and
#'    must be in the following format:
#'
#'    \itemize{
#'      \item output. The prefix of the CSV and PNG to which to output the results
#'        of the validation. For example, if you put "output/foo", then "output/foo.csv"
#'        will be a CSV file containing a data.frame with the columns "dep_var",
#'        "score", and \code{id_column}, where the score refers to predicted score,
#"        and \code{id_column} is given by the \code{id_column} option below. On the other
#'        hand, "output/foo.png" will contain a decile validation plot of the results.
#'      \item percent. The percent of the data that was used for training. Currently,
#'        only sequential splits of training and validation are supported until
#'        syberia introduces a better mechanism for data partitions. The default is 0.8.
#'      \item dep_var. (Optional) The name of the dependent variable in the evaluated data.
#'        The default is "dep_var".
#'      \item id_column. (Optional) The name of an identifying column in your
#'        pre-data-munged data.frame. This will be included in the validation
#'        output CSV. If not given, no ID column will be included.
#'      \item cutoff. (Optional) A cutoff for binary classification predictions: above the
#'        cutoff means a prediction 1, and otherwise a 0. The default is 0.5.
#'     }
#' @export
#' @return a stageRunner that performs AUC, confusing matrix, and validation
#'   plotting.
evaluation_stage <- function(evaluation_parameters) {
  stopifnot('output' %in% names(evaluation_parameters))

  params <- list(
    output = evaluation_parameters$output,
    cutoff = evaluation_parameters$cutoff %||% 0.5,
    train_percent = evaluation_parameters$percent %||% 0.8,
    dep_var = evaluation_parameters$dep_var %||% 'dep_var',
    id_column = evaluation_parameters$id_column
  )

  # This list of functions will be incorporated into the full model stageRunner
  list(
     '(Internal) Generate evaluation options' = evaluation_stage_generate_options(params),
     'confusion matrix' = evaluation_stage_confusion_matrix,
     'validation plot' = evaluation_stage_validation_plot,
     auc = evaluation_stage_auc
  )
}

#' Store necessary information for evaluation stage.
#'
#' The \code{evaluation_stage} function needs a data.frame containing
#' some information (namely, \code{score} and \code{dep_var}). This function
#' extracts that information from the active stageRunner. Note this
#' means evaluation will not work if not called from syberia's \code{run_model}.
#' This will later be alleviated by the introduction of data partitions
#' into syberia.
#' 
#' If the evaluation stage prints a CSV copy of the prediction dataframe
#' (see the \code{output} parameter in the \code{evaluation_stage} options,
#' it is also performed in this helper function.
#'
#' @param params list. A list containing \code{output}, \code{cutoff},
#'   \code{train_percent}, \code{dep_var}, and \code{id_column} as in
#'   the evaluation_stage parameters.
#' @return a function suitable for use in a stageRunner.
evaluation_stage_generate_options <- function(params) {
  function(modelenv) {
    modelenv$evaluation_stage <- params
    # TODO: (RK) Remove this to use the IO adapter once that has been written.
    # In order to grab the data as what it looked like prior to any data preparation,
    # we are going to extract it from the cached environment of the first step in the
    # data stage. This way, it will be import-method-agnostic, and we will not
    # have to worry whether our data came from CSV, S3, etc. We also assume the
    # stageRunner we are attached to is in `active_runner()`.
    raw_data <- stagerunner:::treeSkeleton(
      active_runner()$stages$data)$first_leaf()$object$cached_env$data

    if(modelenv$evaluation_stage$random_sample){
      set.seed(modelenv$evaluation_stage$seed) 
      training_rows <- createDataPartition(factor(raw_data[, modelenv$evaluation_stage$dep_var]), 
                                           p = modelenv$evaluation_stage$train_percent, list = FALSE, times = modelenv$evaluation_stage$times)[,1]  
      
      validation_rows <- setdiff(seq(1, nrow(raw_data)), training_rows)
    } else validation_rows <- seq(round(modelenv$evaluation_stage$train_percent * nrow(raw_data) + 1), nrow(raw_data))
    
    # The validation data is the last (1 - train_percent) of the dataframe.
    validation_data <- raw_data[validation_rows, ]
    score <- modelenv$model_stage$model$predict(validation_data)
    # TODO: (RK) Replace this with data partitions after they've been 
    # incorporated into syberia.

    modelenv$evaluation_stage$prediction_data <-
      data.frame(dep_var = validation_data[[modelenv$evaluation_stage$dep_var]],
                 score = score)

    if (!is.null(id_column <- modelenv$evaluation_stage$id_column))
      modelenv$evaluation_stage$prediction_data[[id_column]] <-
        validation_data[[id_column]]

    write.csv(modelenv$evaluation_stage$prediction_data,
              paste0(modelenv$evaluation_stage$output, '.csv'), row.names = FALSE)
  }
}
  
#' Compute the AUC in conjunction with \code{evaluation_stage}.
#'
#' The RoC, accuracy, sensitivity, and specificy information will be
#' returned as computed by the AUC package.
#'
#' The modeling environment will contain a key \code{auc} inside
#' of the \code{evaluation_stage} list. This will contain:
#' 
#' \itemize{
#'    \item roc. The RoC of the validation data.
#'    \item accuracy. The accuracy of the validation data.
#'    \item sensitivity. The sensitivity of the validation data.
#'    \item specificity. The specificity of the validation data.
#' }
#'
#' @param modelenv environment. The current modeling environment.
# TODO: (RK) Make the above items more descriptive.
evaluation_stage_auc <- function(modelenv) {
  Ramd::packages('AUC') 
  modelenv$evaluation_stage$auc <- with(modelenv$evaluation_stage$prediction_data, {
    list(
      roc = AUC::auc(roc(score, factor(dep_var))),
      accuracy = AUC::auc(accuracy(score, factor(dep_var))),
      sensitivity = AUC::auc(sensitivity(score, factor(dep_var))),
      specificity = AUC::auc(specificity(score, factor(dep_var)))
  )})
  print(modelenv$evaluation_stage$auc)
}

#' Compute a confusion matrix and plot it.
#'
#' The results will be stored in the modeling environment's 
#' \code{confusion_matrix} key within the \code{evaluation_stage} list.
#'
#' @param modelenv environment. The current modeling environment.
evaluation_stage_confusion_matrix <- function(modelenv) {  
  confusion_matrix_arguments <- 
    list(modelenv$evaluation_stage$prediction_data,
         modelenv$evaluation_stage$cutoff,
         plot.it = TRUE, xlab = c("dep_var = 0", "dep_var = 1"),
         ylab = c("score = 0", "score = 1"), title = NULL)
  modelenv$evaluation_stage$confusion_matrix <-
    do.call(confusion_matrix, confusion_matrix_arguments)
}
  
#' Draw a decile validation plot for a classification problem.
#'
#' This evaluation stage will produce a plot showing performance
#' of the model according to a certain metric described below.
#' The plot will be stored in a PNG file, which the user can control.
#' (See the \code{output} option in \code{evaluation_stage}).
#'
#' The plot will contain the following information:
#'
#' \itemize{
#'  \item Percent of validation samples with \code{dep_var} = 1 on the y-axis.
#'  \item Decile by model score on the x-axis. For example, if 0.1 is displayed
#'    on the x-axis, this should be interpreted as "the best 10% of samples
#'    by decile score" (meaning, most likely to have \code{dep_var = 0} as
#'    predicted by the model). If 0.2 is displayed, it means "the next best 10%
#'    of samples by decile score", that is, those falling within the 10 to 20th
#'    percentile.
#' } 
#'
#' In other words, the plot describes how "good" the model is at capturing
#' the actual percent of samples with \code{dep_var = 1}. The samples whose
#' predicted model score is within the top 10% should have low percent of
#' empirical \code{dep_var = 1}, whereas the worst 10% should have high
#' such percentage.
#'
#' @param modelenv environment. The current modeling environment.
evaluation_stage_validation_plot <- function(modelenv){
  ordered_scores <- modelenv$evaluation_stage$prediction_data[
    order(modelenv$evaluation_stage$prediction_data$score), ]
  xs <- (10*(0:9) + 9) / 100 # TODO: (RK) Make a parameter for this
  ys <- sapply(xs, function(x) {
    xrows <- seq((nrow(ordered_scores) * max(x - (xs[2] - xs[1]), 0) + 1),
                 (nrow(ordered_scores) * x))
    sum(ordered_scores[xrows, 'dep_var']) / length(xrows)
  }) # TODO: (RK) Figure out if this can be done more cleanly with tapply.
  
  png(filename = paste0(modelenv$evaluation_stage$output, '.png'))
  plot(xs, ys, type = 'l', col = 'darkgreen',
       main = 'Dependent variable capture v.s. score deciles',
       xlab = '% of samples ordered by model score',
       ylab = '% of dependent variable = 1',
       frame.plot = TRUE, lwd = 3, cex = 2)
  dev.off()
  NULL
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


