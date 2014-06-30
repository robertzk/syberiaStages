# pars
#  do_visualize, do_check_sig, do_check_excessive_levels = whether or not to perform the stages
#  depvarname = name of dependent variable
#  maxlevels = warn if a factor variable has more than maxlevels levels  

#' @export
explore_stage <- function(modelenv,pars) {
  
  pars <- list(
    # which functions to execute
    do_visualize = pars$do_visualize %||% FALSE,
    do_check_sig = pars$do_check_sig %||% FALSE,
    do_check_excessive_levels = pars$do_check_excessive_levels %||% FALSE,

    # parameters for functions
    depvarname = pars$depvarname %||% 'dep_var',
    maxlevels = pars$maxlevels  %||% 10
  )

  stagerunner_input <- list()

  if (pars$do_visualize) stagerunner_input[["Visualization"]] <- visualize()
  if (pars$do_check_sig) stagerunner_input[["Check for significant differences in 1s and 0s"]] <- check_sig(pars)
  if (pars$do_check_excessive_levels) stagerunner_input[["Check for excessive number of levels"]] <- check_excessive_levels(pars)

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
check_sig <- function(pars) {
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

# How many levels does each factor level have?
check_excessive_levels <- function(pars) {
  force(pars)
  function(modelenv) {
    for (col in names(modelenv$data)) {
      column <- modelenv$data[[col]]
      if (is.factor(column)) {
        n <- nlevels(column)
        if (n > pars$maxlevels) {
          cat('\033[0;31m',col,': ',n,' levels\033[0m\n',sep='')
        } else {
          cat(col,': ',n,' levels\n',sep='')
        }
      } else { 
        # in case column is truly continuous, this would take forever
        # so we do a preliminary check on the head of the column
        if (length(table(head(column,n=1000)))<=10) {
          n <- length(table(column))
          if (n<=10 && n>0) cat('\033[0;33m',col,' is not a factor, but has only ',n,' levels\033[0m\n',sep='')
        }
      }
    }
  }
}
