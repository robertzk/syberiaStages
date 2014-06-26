#' @export
explore_stage <- function(modelenv,pars) {
  stagerunner_input <- list(
    "Visualization" = visualize(),
    "DQ Check"      = dqcheck(pars)
  ) 
}

# Do some basic data visualizations
visualize <- function() {
  function(modelenv) {
    source("~/dev/avant-models/scripts/visualizeData.R")
    plotHistograms(modelenv$data)
  }
}

# We want to see how well the data are segmented by each variable in turn.
# If the p-value is unrealistically low, it could be a sign that there is
# some issue with the data.
dqcheck <- function(pars) {
  force(pars)
  function(modelenv) {
    whichones <- modelenv$data[[pars$depvarname]]==1
    ones <- subset(modelenv$data,subset=whichones)
    zeros <- subset(modelenv$data,subset=!whichones)
    for (col in colnames(modelenv$data)) {
      cat(col,": ",sep='')
      if (is.factor(modelenv$data[[col]])) {
        cat("is factor\n")
      } else { 
        mean1 <- formatC(mean(ones[[col]],na.rm=T),digits=3)
        mean0 <- formatC(mean(zeros[[col]],na.rm=T),digits=3)
        missing1 <- sum(is.na(ones[[col]]))
        missing0 <- sum(is.na(zeros[[col]]))
        cat("Avg[",col," | ",pars$depvarname,"=0] = ",mean0," (",missing0," NA), ",sep='')
        cat("Avg[",col," | ",pars$depvarname,"=1] = ",mean1," (",missing1," NA)\n",sep='')
      }
    }
  }
}
