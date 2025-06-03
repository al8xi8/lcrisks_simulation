
# function to check the input data frame or matrix
checkInputData <- function(x, minNC=10, maxNC=23) {

  if (!length(x)) stop("ERROR: input data has length 0")
  if (!is.data.frame(x) && !is.matrix(x)) stop("ERROR: input data must be a matrix or data frame")
  nc <- ncol(x) 
  if (nc < minNC) stop(paste0("ERROR: input data must have at least ", minNC, " columns"))
  if (is.matrix(x) && !is.numeric(x)) stop("ERROR: input data must be a numeric matrix or data frame with numeric columns")

  if (is.data.frame(x)) {
    err <- 0
    for (i in 1:min(nc, maxNC)) {
      vec <- x[, i, drop=TRUE]
      tmp <- is.character(vec) || (!is.numeric(vec) && !all(!is.finite(vec)))
      if (tmp) {
        msg <- paste0("ERROR: column ", i, " of the input data is not numeric\n")
        cat(msg) 
        err <- 1
      }
    }
    if (err) stop("ERROR: input data must be a numeric matrix or data frame with numeric columns")
  }

  # Convert non-finite values (NA, NaN, Inf, etc) to NA
  for (i in 1:min(nc, maxNC)) {
    tmp <- !is.finite(x[, i, drop=TRUE])
    if (any(tmp)) x[tmp, i] <- NA
  }

  # Check for missing age and gender
  tmp <- !is.finite(x[, 1, drop=TRUE]) | !is.finite(x[, 2, drop=TRUE]) 
  if (any(tmp)) {
    stop("ERROR: missing values are not allowed for columns 1 and 2 (age and gender)")
  }

  err <- checkAllCols(x, maxNC=maxNC)
  if (err) stop("ERROR with input data, see ERROR message(s) above")

  # Only return cols that are needed
  x <- as.matrix(x[, 1:min(nc, maxNC), drop=FALSE])

  x
}

# Function to check the columns of the input data frame or matrix
checkAllCols <- function(x, maxNC=23) {

  nc  <- ncol(x)
  err <- 0
  err <- err + checkNumCol(x[, 1, drop=TRUE], 1, minVal=1) 
  err <- err + checkCatCol(x[, 2, drop=TRUE], 2, 0:1, c("Male", "Female"))
  err <- err + checkNumCol(x[, 3, drop=TRUE], 3, minVal=0)
  err <- err + checkNumCol(x[, 4, drop=TRUE], 4, minVal=0)
  err <- err + checkNumCol(x[, 5, drop=TRUE], 5, minVal=0)
  err <- err + checkCatCol(x[, 6, drop=TRUE], 6, 0:3, c("Non-hispanic white", 
                                           "Non-hispanic Black/African American",
                                           "Hispanic", "Other Ethnicity"))
  err <- err + checkCatCol(x[, 7, drop=TRUE], 7, 0:1, c("No COPD or Emphysema", "COPD or Emphysema"))
  err <- err + checkCatCol(x[, 8, drop=TRUE], 8, 0:2, NULL)
  err <- err + checkNumCol(x[, 9, drop=TRUE], 9, minVal=0)
  err <- err + checkCatCol(x[, 10, drop=TRUE], 10, 1:6, c("< 12th grade", "high school graduate", 
                                           "post high school, no college",
                                           "associate degree/some college",
                                           "bachelors degree", "graduate school"))

  yncols <- 11:22
  tmp    <- yncols <= nc
  yncols <- yncols[tmp]
  if (length(yncols)) {
    for (i in yncols) err <- err + checkCatCol(x[, i, drop=TRUE], i, 0:1, c("No", "Yes"))
  }
  if (nc > 22) err <- err + checkNumCol(x[, 23, drop=TRUE], 23, minVal=0)

  err
}

# Function to check for valid levels of a categorical vector
checkCatCol <- function(vec, col, levels, values) {

  err <- 0
  tmp <- (is.finite(vec) & !(vec %in% levels))
  if (any(tmp)) {
    if (length(values)) { 
      str <- paste(levels, values, sep="=")
      str <- paste(str, collapse=", ", sep="") 
    } else {
      str <- paste(levels, collapse=", ", sep="")
    }  
    msg <- paste0("ERROR: column ", col, " must be coded as ", str, "\n")
    cat(msg)  
    err <- 1
  }
  err
}

# Function to check for valid levels of a categorical vector
checkNumCol <- function(vec, col, minVal=0) {

  err <- 0
  tmp <- is.finite(vec) & (vec < minVal) 
  tmp[is.na(tmp)] <- FALSE
  if (any(tmp)) {
    msg <- paste0("ERROR: column ", col, " must have values >= ", minVal, "\n")
    cat(msg)  
    err <- 1    
  }
  err

}

checkLogical <- function(obj, name="impute.missing") {

  len <- length(obj)
  if (len != 1) stop(paste0("ERROR: ", name, " must be TRUE or FALSE"))
  NULL

} 

checkNumeric <- function(obj, name="nyears", minval=0, maxval=10) {

  len <- length(obj)
  if (len != 1) stop(paste0("ERROR: ", name, " must be a single numeric value"))
  if ((obj < minval) || (obj > maxval)) {
    stop(paste0("ERROR: ", name, " must be between ", minval, " and ", maxval))
  }
  NULL

}

getMinLogVecLE <- function(vec, value) {

  n   <- length(vec)
  tmp <- vec <= value
  tmp[is.na(tmp)] <- FALSE
  ret <- which(tmp)
  len <- length(ret)
  if (len > 1) ret <- max(ret, na.rm=TRUE)
  if (!length(ret)) ret <- NA
  ret
}

checkImputed <- function(lp, var, warn=1) {

  tmp <- !is.finite(lp)
  if (any(tmp)) {
    lp[tmp] <- NA
    if (warn) warning(paste0("Imputing variable '", var, "' results in missing values"))
  }
  lp
}

checkCounterfactual <- function(x) {

  if (length(x) != 1) stop("ERROR: counterfactual.race must be an integer 0-3")
  if (!is.numeric(x)) stop("ERROR: counterfactual.race must be an integer 0-3")
  if (!(x %in% 0:3))  stop("ERROR: counterfactual.race must be an integer 0-3")
  NULL
}

mergeObj.counterfactual <- function(x1, x2) {

  ret <- x1
  if (!length(x1)) return(ret)
  if (!length(x2)) return(ret)
  if (!is.matrix(x1) && !is.data.frame(x1)) return(ret)
  if (!is.matrix(x2) && !is.data.frame(x2)) return(ret)
  nr1 <- nrow(x1)
  nc1 <- ncol(x1)
  nr2 <- nrow(x2)
  nc2 <- ncol(x2)
  if ((nr1 != nr2) || (nc1 != nc2)) return(ret)
  if (nc2 == 13) {
    colnames(x2) <- paste0("Counterfactual: ", colnames(x2))
    x2           <- x2[, 8:13, drop=FALSE]
    ret          <- cbind(x1, x2)
  }
  ret

}
