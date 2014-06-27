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
    #plotHistograms(modelenv$data)
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
      } else if (is.character(modelenv$data[[col]])) {
        cat("is character\n")
      } else { 

        x <- ones[[col]]
        y <- zeros[[col]]

        flag.problem = FALSE
 
        # Check for unrealistically good separation of 0s and 1s via two-sample t-test
        spaces <- paste0(rep(' ',nchar(col)+2),collapse='')
        print.spaces <- function() ifelse(flag.problem, spaces, '')
        if (all(is.na(x))) { cat(print.spaces(),'\033[0;31mAll 1s are missing\033[0m\n',sep=''); flag.problem=TRUE }
        if (all(is.na(y))) { cat(print.spaces(),'\033[0;31mAll 0s are missing\033[0m\n',sep=''); flag.problem=TRUE } 
        if (flag.problem) next
        if (sum(is.na(x))==1) { cat(print.spaces(),'\033[0;31mOnly one non-missing 1\033[0m\n',sep=''); flag.problem=TRUE }
        if (sum(is.na(y))==1) { cat(print.spaces(),'\033[0;31mOnly one non-missing 0\033[0m\n',sep=''); flag.problem=TRUE }
        if (flag.problem) next
        if (sd(x,na.rm=T)==0) { cat(print.spaces(),'\033[0;31m1s are single-valued\033[0m\n',sep=''); flag.problem=TRUE }
        if (sd(y,na.rm=T)==0) { cat(print.spaces(),'\033[0;31m0s are single-valued\033[0m\n',sep=''); flag.problem=TRUE }
        if (flag.problem) next

        # Remove NAs
        x <- x[!is.na(x)]
        y <- y[!is.na(y)]
 
        # Do the t-test and print results
        tmp <- t.test(x,y)
        p <- tmp$p.value
        if (p<0.05) {
          cat('\033[0;31mp-value = ',p,'\033[0m\n',sep='')
        } else {
          cat("p-value = ",p,'\n',sep='')
        }
      }
    }
  }
}
