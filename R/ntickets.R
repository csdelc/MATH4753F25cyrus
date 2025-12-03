#' Title
#'
#' @param N Seats available on the flight
#' @param gamma Probability of overbooking
#' @param p Likelihood a passenger will show
#'
#' @importFrom stats pbinom
#' @importFrom graphics abline barplot points
#'
#'
#' @returns Plots for both discrete and continuous methods along with a list of variable values
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=400, gamma = .02, p = .95)}
ntickets = function(N, gamma, p){

  #range for tickets
  tixrange <- seq(N, N+50, by = 1)

## discrete objective ticket number
  #Formula for discrete objective where we know that:
  # 1 - gamma - P(X <= N), considering X~Bin(n, p)
  obj <- 1 - gamma - pbinom(N, size=tixrange, p)

  #indexing the closest term to 0 (minimum) for out objective
  min <- which.min(abs(obj))

  #find minimum n-value for the discrete method:
  nd <- tixrange[min]

  #plotting discrete objective line
  plot(tixrange, obj, type = "l", lty = "dotdash", col = "black",
       main = paste("Objective v. n to find optimal tickets sold\n(", tixrange[min],") gamma=", gamma, ", N =", N, "discrete", sep = ""),
       xlab = "n", ylab = "Objective"
  )

  #plot points along the line
  points(tixrange, obj, pch = 21, col = "black", bg = "blue", cex = .75)

  #plot point to show optimal # of tickets sold
  points(tixrange[min], obj[min], pch = 21, col = "yellow", cex = 1)

  #plot vertical/horizontal lines that intersect with optimized point
  abline(h = 0, col = "red", lty = 1, lwd = 2.25)
  abline(v = tixrange[min], col = "red", lty = 1, lwd = 2.25)

## continuous approximation objectives
  # approximation through P(X <= N) using z-score to find outlier
  z <- (N+.5 - tixrange*p)/sqrt(tixrange*p*(1-p))
  approxp <- pnorm(z)
  contapprox <- 1-gamma-approxp

  # find min-index for continuous approx (closest to 0)
  mincont <- which.min(abs(contapprox))

  #minimum n-value using ticket range
  nc <- tixrange[mincont]

  #plots the line for continuous objective
  plot(tixrange, contapprox, type = "l", col = "black", lwd = 1,
       main = paste("Objective vs n to find optimal tickets sold\n(", tixrange[mincont],") gamma=", gamma, ", N =", N, "continuous", sep = ""),
       xlab = "n", ylab = "Objective"
  )

  #plot point to show the optimal # of tickets
  points(tixrange[mincont], obj[mincont], pch = 21, col = "yellow", cex = 1)

  #plot vertical and horizontal lines that intersect with optimized point
  abline(h = 0, col = "blue", lty = 1)
  abline(v = tixrange[mincont], col = "blue", lty = 1)

  #returning the list of values for: nd, nc, N, p, and gamma
  return(list(nd=nd, nc=nc, N=N, p=p, gamma=gamma))
}
