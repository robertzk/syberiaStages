# pars
#  do_visualize, do_check_sig, do_check_excessive_levels = whether or not to perform the stages
#  depvarname = name of dependent variable
#  maxlevels = warn if a factor variable has more than maxlevels levels  

#' @export
explore_stage <- function(modelenv,pars) {
  
  pars <- list(
    # which functions to execute
    do_var_quality_checks = pars$do_var_quality_check %||% TRUE,
    do_visualize = pars$do_visualize %||% FALSE,
    do_check_sig = pars$do_check_sig %||% FALSE,
    do_check_excessive_levels = pars$do_check_excessive_levels %||% FALSE,
    do_check_unpopulated_levels = pars$do_check_unpopulated_levels %||% FALSE,

    # parameters for functions
    depvarname = pars$depvarname %||% 'dep_var',
    maxlevels = pars$maxlevels  %||% 10,
    min_cts_in_level = pars$min_cts_in_level %||% 100
  )

  stagerunner_input <- list()

  if (pars$do_var_quality_checks) stagerunner_input[["Variable quality checks"]] <- check_var_quality(pars)
  if (pars$do_visualize) stagerunner_input[["Visualization"]] <- visualize()
  if (pars$do_check_sig) stagerunner_input[["Check for significant differences in 1s and 0s"]] <- check_sig(pars)
  if (pars$do_check_excessive_levels) stagerunner_input[["Check for excessive number of levels"]] <- check_excessive_levels(pars)
  if (pars$do_check_unpopulated_levels) stagerunner_input[["Checking for unpopulated factor levels"]] <- check_unpopulated_levels(pars)

  stagerunner_input
}

# Do basic variable quality checks
check_var_quality <- function(pars) {
  force(pars)
  function(modelenv) {
    
    for (col in colnames(modelenv$data)) {
      cat(col,": ",sep='')

      # check for all NA
      if (all(is.na(modelenv$data[[col]]))) {
        cat('\033[0;31m is all NA\033[0m\n',sep='')
        next
      }

      # check for only one level
      if (length(table(head(modelenv$data[[col]],n=1000))) == 1) {
        if (length(table(modelenv$data[[col]]))==1) {
          cat('\033[0;31m has only one level\033[0m\n',sep='')
          next
        }
      }
    }
  }
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

        if (length(table(head(x,n=1000)))<10 & length(table(head(y,n=1000)))<10) { # the variable is factor-like
        
          # convert x and y to factors with common levels
          all_levels <- as.character(unique(c(x,y)))
          x <- factor(x,levels=all_levels)
          y <- factor(y,levels=all_levels)
          
          # tabulate x and y
          t1 <- table(x)
          t2 <- table(y)
          
          # consider only bins with at least 5 counts
          select <- t1>=5 & t2>=5
          t1 <- t1[select]
          t2 <- t2[select]
          
          # compute the chi-squared
          f1 <- sqrt(sum(t2)/sum(t1))
          f2 <- sqrt(sum(t1)/sum(t2))
          X2 <- sum( (f2*t2-f1*t1)^2 / (t1+t2) )
          
          # get the p-value
          p <- 1-pchisq(X2,4)

          if (p<0.05) {
            cat('\033[0;31mp-value = ',p,'\033[0m',sep='')
          } else {
            cat("p-value = ",p,sep='')
          }
          cat(' (X2-test)\n')

	} else { # the variable is more or less continuous 

          # Do the t-test and print results
          tmp <- t.test(x,y)
          p <- tmp$p.value
          if (p<0.05) {
            cat('\033[0;31mp-value = ',p,'\033[0m',sep='')
          } else {
            cat("p-value = ",p,sep='')
          }
          cat(' (t-test)\n')

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

check_unpopulated_levels <- function(pars) {
  force(pars)
  function(modelenv) {
    for (col in names(modelenv$data)) {
      column <- modelenv$data[[col]]
      if (is.factor(column)) {
        unpopulated_levels <- levels(x)[table(x) < pars$min_cts_in_level]
        if (length(unpopulated_levels)>0) cat(col," has the following sparse levels: ",paste0(unpopulated_levels,collapse=', '),'\n',sep='')
      }
    }
  }
}
