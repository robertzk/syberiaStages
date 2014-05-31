#' Evaluation stage for syberia models.
#'
#' A helper stage for evaluating a binary classification or a regression 
#' model according to the metrics: AUC, a confusion matrix, and a validation plot.
#'
#' The evaluation stage parameters in the syberia model file are described in the
#' `evaluation_parameters` argument below.
#'
#' @param modelenv an environment. The persistent modeling environment.
#' @param evaluation_parameters list. These come from the syberia model and
#'    must be in the following format:
#'
#'    \itemize{
#'      \item type. The first element must be a string `"regression"` or "`classification"`.
#'      \item percent. The percent of the data that was used for training. Currently,
#'        only sequential splits of training and validation are supported until
#'        syberia introduces a better mechanism for data partitions.
#'     }
#' @export
#' @return a stageRunner that performs AUC, confusing matrix, and validation
#'   plotting.
evaluation_stage <- function(modelenv, evaluation_parameters) {
  # TODO: (RK) Remove this to use the IO adapter once that has been written.
  # In order to grab the data as what it looked like prior to any data preparation,
  # we are going to extract it from the cached environment of the first step in the
  # data stage. This way, it will be import-method-agnostic, and we will not
  # have to worry whether our data came from CSV, S3, etc. We also assume the
  # stageRunner we are attached to is in `active_runner()`.
  raw_data <- stagerunner:::treeSkeleton(
    active_runner()$stages$data)$first_leaf()$object$cached_env$data

  stopifnot(is.character(evaluation_parameters[[1]]) &&
            evaluation_parameters[[1]] %in% c("regression", "classification"))
  # if (!exists(evalutaion_fn <- pp('eval_models_#{evaluation_parameters[[1]]}')))
  #  stop("Missing evaluation container for keyword '", evaluation_parameters[[1]], "'")
  trainpct <- evaluation_parameters[[grep("percent", names(evaluation_parameters))]]
  test_rows <- (trainpct*nrow(modelenv$data)+1) : nrow(modelenv$data)
  test_data <- modelenv$data[test_rows, ]
  score <- modelenv$model_stage$model$predict(test_data)
  
  #pred_scores <- global_modeling_environment$model_stage$model$predict(global_modeling_environment$data
  prediction <- data.frame(dep_var = test_data[[evaluation_parameters$dep_var]], score = score)
  write.csv(cbind(test_data$loan_id, prediction), gsub(".csv", "_prediction.csv", global_modeling_environment$import_stage$file), row.names = F)
  modelenv$evaluation_stage$options <-
    list(prediction = prediction, cutoff = evaluation_parameters$cutoff %||% 0.5,
         plot.it = TRUE, xlab = c("dep_var = 0", "dep_var = 1"),
         ylab = c("score = 0", "score = 1"), title = NULL, last = FALSE, nxt = NULL)
  #everything each function needs should reside in the modelenv$evalutaion_stage
  
  auc <- function(modelenv) { #each of the function is a stagerunner and it should just take modelenv as input
    library(AUC)
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
