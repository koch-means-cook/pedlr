GetNloptrStatusCodes <- function() {
  result <- data.frame(
    code = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -4, -5),
    message = c(
      "Generic success return value.",
      "Optimization stopped because stopval was reached.",
      "Optimization stopped because ftol_rel or ftol_abs was reached.",
      "Optimization stopped because xtol_rel or xtol_abs was reached.",
      "Optimization stopped because maxeval was reached.",
      "Optimization stopped because maxtime was reached.",
      "Generic failure code.",
      paste(
        "Invalid arguments (e.g. lower bounds are bigger than upper",
        "bounds, an unknown algorithm was specified, etc.)."
      ),
      "Ran out of memory.",
      paste(
        "Halted because roundoff errors limited progress. (In this case,",
        "the optimization still typically returns a useful result.)"
      ),
      paste(
        "Halted because of a forced termination: the user called",
        "nlopt_force_stop(opt) on the optimization's nlopt_opt object opt",
        "from the user's objective function or constraints."
      )
    )
  )
  return(result)
}