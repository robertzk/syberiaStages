#' Evaluation stage for syberia models.
#'
#' A helper stage for evaluating a binary classification or a regression 
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
#'      \item type. The first element must be a string "regression" or "classification".
#'      \item percent. The percent of the data that was used for training. Currently,
#'        only sequential splits of training and validation are supported until
#'        syberia introduces a better mechanism for data partitions. The default is 0.8.
#'      \item dep_var. The name of the dependent variable in the evaluated data.
#'        The default is "dep_var".
#'      \item prediction_output. The name of the CSV to which to output the results
#'        of the validation. This will be a data.frame with the columns "dep_var",
#'       "loan_id", and "score", where the latter refers to predicted score.
#'      \item cutoff. A cutoff for binary classification predictions: above the
#'        cutoff means a prediction 1, and otherwise a 0. The default is 0.5.
#'     }
#' @export
#' @return a stageRunner that performs AUC, confusing matrix, and validation
#'   plotting.
evaluation_stage <- function(evaluation_parameters) {
  # Only binary classification and continuous regression are supported
  # evaluation types for now.
  stopifnot((is.character(type <- evaluation_parameters[[1]]) ||
             is.character(type <- evaluation_parameters$type)) &&
            type %in% c("regression", "classification"))

  cutoff <- evaluation_parameters$cutoff %||% 0.5
  list(
       plot.it = TRUE, xlab = c("dep_var = 0", "dep_var = 1"),
       ylab = c("score = 0", "score = 1"), title = NULL, last = FALSE, nxt = NULL)

  generate_options <- function(modelenv) {
    # TODO: (RK) Remove this to use the IO adapter once that has been written.
    # In order to grab the data as what it looked like prior to any data preparation,
    # we are going to extract it from the cached environment of the first step in the
    # data stage. This way, it will be import-method-agnostic, and we will not
    # have to worry whether our data came from CSV, S3, etc. We also assume the
    # stageRunner we are attached to is in `active_runner()`.
    raw_data <- stagerunner:::treeSkeleton(
      active_runner()$stages$data)$first_leaf()$object$cached_env$data

    train_percent <- evaluation_parameters$percent %||% 0.8
    validation_rows <- seq(train_percent * nrow(raw_data) + 1, nrow(raw_data))
    validation_data <- raw_data[validation_rows, ]
    score <- modelenv$model_stage$model$predict(validation_data)

    modelenv$evaluation_stage$prediction_data <-
      data.frame(dep_var = test_data[[evaluation_parameters$dep_var %||% 'dep_var']],
                 score = score, loan_id = validation_data$loan_id)

    if ('prediction_output' %in% names(evaluation_parameters)) {
      write.csv(prediction, evaluation_parameters$prediction_output, row.names = FALSE)
    }
  }
  
  auc <- function(modelenv) {
    require(AUC)
    auc_result <- with(modelenv$evaluation_stage$options$prediction, {
      list(
        roc = AUC::auc(roc(score, factor(dep_var))),
        accuracy = AUC::auc(accuracy(score, factor(dep_var))),
        sensitivity = AUC::auc(sensitivity(score, factor(dep_var))),
        specificity = AUC::auc(specificity(score, factor(dep_var)))
      ) })
    print(auc_result)
  }
  
  confusion_matrix <- function(modelenv) {  
    with(modelenv$evaluation_stage$options, {
      stopifnot(is.data.frame(prediction) &&
                  all(c('score', 'dep_var') %in% colnames(prediction)))
      stopifnot(is.numeric(prediction$score) && is.numeric(prediction$dep_var))
      
      prediction$score <- ifelse(prediction$score <= cutoff, 0, 1) # TODO: K-S statistic?
      categories <- prediction$score * 2 + prediction$dep_var
      confusion <- matrix(tabulate(1 + categories, 4), nrow = 2)
      colnames(confusion) <- ylab
      rownames(confusion) <- xlab
      if (plot.it) fourfoldplot(confusion, color = c("#CC6666", "#99CC99"),
                                conf.level = 0, margin = 1, main = title)
      confusion
    })
  }
  
  validation_plot <- function(modelenv){
    with(modelenv$evaluation_stage$options$prediction, {
      ordered_postdata <- prediction[order(score), ]
      xs <- (10*(0:9) + 9) / 100
      # ys <- tapply(dep_var, rep(1:10, rep(round(nrow(postdata) / 10), 10))[1:nrow(postdata)], mean)
      ys <- sapply(xs, function(x) {
        xrows <- (nrow(ordered_postdata) * max(x - (xs[2] - xs[1]), 0) + 1):(nrow(ordered_postdata) * x)
        sum(ordered_postdata[xrows, 'dep_var']) / length(xrows)
      })
      
      png(filename = gsub(".csv", "_validation.png", global_modeling_environment$import_stage$file))
      plot(xs, ys, type = 'l', col = 'darkgreen',
           main = pp('#{output_name} (on new validation data)'),
           xlab = '% of customers, ordered by model score',
           ylab = '% of defaults captured',
           frame.plot = TRUE, lwd = 3, cex = 2)
      dev.off()
      NULL
    })
  }
  
  stageRunner$new(modelenv, list('auc' = auc, 'confusion matrix' = confusion_matrix, 'validation_plot' = ), remember = TRUE)
  # return stagerunner(modelenv, functions) to test specified evaluation methods
}

# hack for now
# tryCatch(run('~/dev/avant-models/models/dev/conditional_default1.0/conditional_default1.0.r', 'evaluation'),error = force); active_runner()$stages[[5]]$run(remember = FALSE)
