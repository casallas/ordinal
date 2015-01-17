# clm2.predict modification
# based on the documentation of predict.clm2
# 2014, Juan Sebastian Casallas

# Gets the name of the response column given a clm2 object
getY.clm2 <- function(object){
  terms <- attr(object$location, "terms") # Get location terms
  Y.indx <- attr(terms, "response") # Get response from terms
  colnames(object$location)[Y.indx]
}

# Expands each row of df by all the levels of Y
expand_levels <- function(df.in, Y){
  # Get response levels
  lvls <- levels(df.in[[Y]])
  # Make response df out of replicated rows
  df.out <- df.in[rep(row.names(df.in), length(lvls)),]
  # Order rows
  df.out <- df.out[order(as.numeric(row.names(df.out))),]
  # Replicate levels
  df.out[[Y]] <- rep(lvls, nrow(df.in))
  # Readjust factors
  df.out[[Y]] <- factor(df.out[[Y]], levels = lvls, ordered = is.ordered(df.in[[Y]]))
  df.out
}

# type = c("prob", "probs", "class")
# "prob" is the type of predictions given by the original predict.clm2 function
# "probs" and "class" mimic the functionality of predict.polr
predict.clm2 <- function(object, newdata, type="prob", ...){
  # The original prediction of the ordinal package
  if(type == "prob"){
	  return(.predict.clm2(object, newdata, ...))
  }
  # Get data from model if not specified
  if(missing(newdata)){
    newdata <- object$location
    # Append nominal predictors, if any
    if(!is.null(object$nominal))
      newdata <- cbind(newdata, object$nominal)
    # Append random effects, if any
    if(!is.null(object$grFac)){
      newdata <- cbind(newdata, factor(object$grFac))
      colnames(newdata)[ncol(newdata)] <- as.character(object$call$random)
    }
  }

  # Get the response name from the model
  Y <- getY.clm2(object)

  # Get number of response levels
  nlevels <- length(levels(newdata[[Y]]))
  
  # Expand the levels of the data set
  newdata <- expand_levels(newdata, Y)
  
  # Get the original predictions for the expanded dataset
  p.clm <- .predict.clm2(object, newdata, ...)
  
  # Now generate the prob matrix as specified in the predict.clm2 documentation
  pmat.clm <- matrix(p.clm, ncol=nlevels, byrow = TRUE)
  if(type=="probs"){
    pmat.clm
  }
  else if(type=="class"){
    factor(apply(pmat.clm, 1, which.max),levels=1:nlevels)
  }
}