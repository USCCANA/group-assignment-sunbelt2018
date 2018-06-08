#' Function to evaluate an expression and with a timelimit
#' @param expr Expression to evaluate
#' @param timeout Timelimit in seconds.
eval_with_timeout <- function(
  expr,
  timeout = 5
) {
  
  p   <- parallel::mcparallel(expr)
  on.exit(tools::pskill(p$pid))
  ans <- parallel::mccollect(p, timeout = timeout, wait=FALSE)
  
  if (!length(ans)) {
    warning("The expression ", substitute(expr), " failed to complete (exceded timeout).")
    return(NA)
  }
  
  ans[[1]]
  
}

# f <- function() {
#   Sys.sleep(1)
#   lm(Murder ~ Rape, data = USArrests)
# }
# 
# eval_with_timeout(f(), timeout = .01)
