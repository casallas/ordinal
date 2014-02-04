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
  df.out <- df.in[0,] # Aux df of newdata
  lvls <- levels(df.in[[Y]])
  # Barbaric method to duplicate rows for each level
  for(r in 1:nrow(df.in)){
    for(l in lvls){
      new_row <- df.in[r,]
      new_row[Y] <- l
      df.out <- rbind(df.out,new_row)
    }
  }
  # Readjust factors
  df.out[[Y]] <- factor(df.out[[Y]], levels = lvls, ordered = is.ordered(df.in[[Y]]))
  df.out
}

# type = c("prob", "probs", "class")
# "prob" is the type of predictions given by the original predict.clm2 function
# "probs" and "class" mimic the functionality of predict.polr
predict.clm2 <- function(object, newdata, type="prob", ...){
  # Hacky solution, get the response name from the call
  Y <- getY.clm2(object)
  nlevels <- length(levels(newdata[[Y]]))
  
  # Expand the levels of the data set
  newdata <- expand_levels(newdata, Y)
  
  # The original prediction of the ordinal package
  p.clm <- .predict.clm2(object, newdata = newdata, ...)
  if(type == "prob"){
	  return(p.clm)
  }
  
  # Now generate the prob matrix as specified in the predict.clm2 documentation
  pmat.clm <- matrix(p.clm, ncol=nlevels, byrow = TRUE)
  if(type=="probs"){
    pmat.clm
  }
  else if(type=="class"){
    factor(apply(pmat.clm, 1, which.max),levels=1:nlevels)
  }
}