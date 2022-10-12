#' A modified version of dismo::evaluate() that will work with glmnet model 
#' objects (which require inputs as matrices).
#'
#' @param p a matrix with values of predictors at presence locations
#' @param a a matrix with values to predictors at background locations
#' @param model a model fit using glmnet::glmnet() or glmnet::cv.glmnet()
#' @param ... additional arguments for the predict function
#' 
#' @return an object of ModelEvaluation-class 

evaluate_lasso <- function(p, a, model, ...) {
  p <- predict(model, p, ...)
  a <- predict(model, a, ...)
  p <- stats::na.omit(p)
  a <- stats::na.omit(a)
  np <- length(p)
  na <- length(a)
  if (na == 0 | np == 0) {
    stop("cannot evaluate a model without absence and presence data that are not NA")
  }
  if (length(p) > 1000) {
    tr <- as.vector(quantile(p, 0:1000/1000))
  }
  else {
    tr <- p
  }
  if (length(a) > 1000) {
    tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
  }
  else {
    tr <- c(tr, a)
  }
  tr <- sort(unique(round(tr, 8)))
  tr <- c(tr - 1e-04, tr[length(tr)] + c(0, 1e-04))
  N <- na + np
  xc <- new("ModelEvaluation")
  xc@presence = p
  xc@absence = a
  R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)
  xc@auc <- R/(as.numeric(na) * as.numeric(np))
  cr <- try(cor.test(c(p, a), c(rep(1, length(p)), rep(0, length(a)))), 
            silent = TRUE)
  if (!inherits(cr, "try-error")) {
    xc@cor <- cr$estimate
    xc@pcor <- cr$p.value
  }
  res <- matrix(ncol = 4, nrow = length(tr))
  colnames(res) <- c("tp", "fp", "fn", "tn")
  xc@t <- tr
  for (i in 1:length(tr)) {
    res[i, 1] <- length(p[p >= tr[i]])
    res[i, 2] <- length(a[a >= tr[i]])
    res[i, 3] <- length(p[p < tr[i]])
    res[i, 4] <- length(a[a < tr[i]])
  }
  xc@confusion = res
  a = res[, 1]
  b = res[, 2]
  c = res[, 3]
  d = res[, 4]
  xc@np <- as.integer(np)
  xc@na <- as.integer(na)
  xc@prevalence = (a + c)/N
  xc@ODP = (b + d)/N
  xc@CCR = (a + d)/N
  xc@TPR = a/(a + c)
  xc@TNR = d/(b + d)
  xc@FPR = b/(b + d)
  xc@FNR = c/(a + c)
  xc@PPP = a/(a + b)
  xc@NPP = d/(c + d)
  xc@MCR = (b + c)/N
  xc@OR = (a * d)/(c * b)
  prA = (a + d)/N
  prY = (a + b)/N * (a + c)/N
  prN = (c + d)/N * (b + d)/N
  prE = prY + prN
  xc@kappa = (prA - prE)/(1 - prE)
  return(xc)
}
