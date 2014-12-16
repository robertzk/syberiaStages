#' Evaluation stage for survival analysis of syberia models.
#'
#' A helper stage for evaluating a survival model
#' based on a comparison chart of IRR on different buckets between 
#' primary feature and full survival model
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
#'        will be a CSV file containing a data.frame with the columns "yld_actual",
#'        "yld_expected", and \code{id_column}, where the yield assumes flat forward curve,
#'        and \code{id_column} is given by the \code{id_column} option below. On the other
#'        hand, "output/foo.png" will contain a decile validation plot of the results.
#'      \item percent. The percent of the data that was used for training. Currently,
#'        only sequential splits of training and validation are supported until
#'        syberia introduces a better mechanism for data partitions. The default is 0.8.
#'      \item dep_var. (Optional) The name of the dependent variable in the evaluated data.
#'        The default is "dep_var".
#'      \item dep_val. (Optional) The name of the dependent numeric variable in the evaluated data.
#'        The default is "dep_val".
#'      \item id_column. (Optional) The name of an identifying column in your
#'        pre-data-munged data.frame. This will be included in the validation
#'        output CSV. If not given, no ID column will be included.
#'      \item random_sample (Optional) An indicator for specifying whether the one has used 
#'      random sample to setup validation data.
#'      \item seed (Optional) the seed used to generate the random validation set.
#'      \item times (Optional) number of times one wants to draw random sample, right now only supports 1.
#'     }
#' @export
#' @return a stageRunner that performs lift chart plotting.
evaluation_stage <- function(evaluation_parameters) {
  stopifnot('output' %in% names(evaluation_parameters))
  params <- list(
    output = evaluation_parameters$output,
    train_percent = evaluation_parameters$percent %||% 0.8,
    validation_rows = evaluation_parameters$validation_rows %||% NULL,
    dep_var = evaluation_parameters$dep_var %||% 'dep_var',
    dep_val = evaluation_parameters$dep_val %||% "dep_val",
    id_column_orig = evaluation_parameters$id_column_orig %||% "id",
    id_column = evaluation_parameters$id_column %||% "loan_id",
    id_benchmark = evaluation_parameters$id_benchmark %||% "sub_grade", 
    id_installment = evaluation_parameters$id_installment %||% "installment",
    id_funded_amnt = evaluation_parameters$id_funded_amnt %||% "funded_amnt",
    id_term = evaluation_parameters$id_term %||% "term",
    id_split = evaluation_parameters$id_split %||% "oracle_split",
    random_sample = evaluation_parameters$random_sample %||% FALSE,
    seed =  evaluation_parameters$seed, 
    times =  evaluation_parameters$times %||% 1
  )

  # This list of functions will be incorporated into the full model stageRunner
  list(
     '(Internal) Generate evaluation options' = evaluation_stage_generate_options(params),
     'validation plot' = evaluation_stage_validation_plot
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
#' @param params list. A list containing \code{output}, \code{dep_val}, 
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

    # TODO: (TL) need to manually run munge procedure to filter out bad loans/loans with too many missing values
    if(!is.null(modelenv$data_stage$validation_primary_key)){

      validation_rows <- raw_data[[params$id_column]] %in% modelenv$data_stage$validation_primary_key
    } else if (!is.null(modelenv$evaluation_stage$validation_rows)) {
      validation_rows <- modelenv$evaluation_stage$validation_rows
    } else if (modelenv$evaluation_stage$random_sample) {
      stopifnot('seed' %in% names(modelenv$evaluation_stage) &&
                  is.numeric(modelenv$evaluation_stage$seed))
      Ramd::packages('caret') # Make sure caret is installed and loaded
      set.seed(modelenv$evaluation_stage$seed) 
      training_rows <- createDataPartition(factor(raw_data[, modelenv$evaluation_stage$id_split]), 
                                           p = modelenv$evaluation_stage$train_percent, list = FALSE, times = modelenv$evaluation_stage$times)[,1]  
      
      validation_rows <- setdiff(seq(1, nrow(raw_data)), training_rows)
    } else validation_rows <- seq(modelenv$evaluation_stage$train_percent * nrow(raw_data) + 1, nrow(raw_data))
    
    # The validation data is the last (1 - train_percent) of the dataframe.
    # validation_data <- raw_data[sample(validation_rows, 500), ]
    validation_data <- raw_data[validation_rows, ]
    munged_data <- modelenv$model_stage$model$munge(validation_data)
    score <- modelenv$model_stage$model$predict(validation_data)
    # TODO: (RK) Replace this with data partitions after they've been 
    # incorporated into syberia.

    validation_data <- validation_data[
      validation_data[[modelenv$evaluation_stage$id_column_orig]] %in% munged_data[[modelenv$evaluation_stage$id_column]], , drop = FALSE]
    modelenv$evaluation_stage$prediction_data <-
      data.frame(dep_var = munged_data[[modelenv$evaluation_stage$dep_var]],
                 dep_val = munged_data[[modelenv$evaluation_stage$dep_val]],
                 benchmark = validation_data[[modelenv$evaluation_stage$id_benchmark]],
                 installment = validation_data[[modelenv$evaluation_stage$id_installment]],
                 funded_amnt = validation_data[[modelenv$evaluation_stage$id_funded_amnt]],
                 term = munged_data[[modelenv$evaluation_stage$id_term]],
                 score = score)
    modelenv$evaluation_stage$baseline_fcn <- 
      modelenv$model_stage$model$output$baseline_fcn

    if (!is.null(id_column <- modelenv$evaluation_stage$id_column))
      modelenv$evaluation_stage$prediction_data[[id_column]] <-
        munged_data[[id_column]]
  }
}

#' Draw a validation plot for a survival problem comparing benchmarck 
#' identifier and model survival curve.
#'
#' This evaluation stage will produce a plot showing performance
#' of the model according to a certain metric described below.
#' The plot will be stored in a PNG file, which the user can control.
#' (See the \code{output} option in \code{evaluation_stage}).
#'
#' @param modelenv environment. The current modeling environment.
evaluation_stage_validation_plot <- function(modelenv) {
  ret <- data.frame(matrix(NA, nrow = nrow(modelenv$evaluation_stage$prediction_data), ncol = 3))
  for (i in 1:nrow(modelenv$evaluation_stage$prediction_data)) {
    row <- modelenv$evaluation_stage$prediction_data[i, , drop = FALSE]
    survival_probs <- modelenv$evaluation_stage$baseline_fcn(seq_len(row$term) / row$term)^exp(row$score)
    irrs <- c(calc_irr(TRUE, row, survival_probs), calc_irr(FALSE, row))
    if (length(irrs) == 2) {
      print(paste(rep("=",80), collapse=""))
      print(row)
      print(irrs)
      ret[i, ] <- list(row$benchmark, irrs[2], irrs[1])
  ret <- ret[!is.na(ret[[1]]), ]
  colnames(ret) <- c("Benchmark", "IRR_realized", "IRR_projected")

  dir.create(dirname(modelenv$evaluation_stage$output), FALSE, TRUE)
  write.csv(ret, paste0(modelenv$evaluation_stage$output, '.csv'), row.names = FALSE)

  #assign("ret", ret, envir = .GlobalEnv)

  ret$Benchmark <- unname(sapply(ret$Benchmark, function(x) strsplit(x, "[0-9]*$")[[1]]))
  buckets <- sort(unique(ret$Benchmark))
  v_buckets <- sapply(buckets, function(x) sum(ret$Benchmark == x) / nrow(ret))
  r_buckets <- sapply(buckets, function(x) median(ret$IRR_realized[ret$Benchmark == x]))
  c_buckets <- unname(1 - quantile(1 - ret$IRR_projected, Reduce(sum, v_buckets, accumulate = TRUE)))
  p_buckets <- sapply(seq_along(c_buckets), function(x) {
    if (x == 1) median(ret$IRR_realized[ret$IRR_projected > c_buckets[x]])
    else if (x == length(c_buckets)) median(ret$IRR_realized[ret$IRR_projected <= c_buckets[x-1]])
    else median(ret$IRR_realized[ret$IRR_projected <= c_buckets[x-1] & 
                                 ret$IRR_projected > c_buckets[x]]) 
    })

  png(filename = paste0(modelenv$evaluation_stage$output, '.png'))
  plot(r_buckets, 
       ylim = c(min(c(r_buckets, p_buckets)), max(c(r_buckets, p_buckets))), 
       type = 'l', col = 'red',
       xlab = 'Buckets',
       ylab = 'Internal Rate of Return',
       lwd = 3, cex = 2)
  lines(p_buckets, type = 'l', col = 'green',
        lwd = 3, cex = 2)
  legend(4, 0, # places a legend at the appropriate place 
         c("Lending Club Grade", "Survival Model Tier"), # puts text in the legend 
         lty=c(1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5), 
         col=c("red","green") # gives the legend lines the correct color and width
        )
  dev.off()
  invisible(NULL)
}
