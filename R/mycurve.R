#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a value to compute P(x <= a)
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#' @autoglobal
#'
#' @returns a list with the mean, standard deviation, a, and area components
#' @export
#'
#' @examples
#' \dontrun{mycurve(mu=5, sigma=3, a=6)}
myncurve = function(mu, sigma, a) {
  #plot a normal curve
  curve((dnorm(x, mean = mu, sd = sigma)),
        xlim = c((mu - 3*sigma), (mu + 3*sigma)),
                 main = paste("P(X <=", a, ")"),
                 ylab = "Density",
                 xlab = "x")
  #shading polygon area
  xval = seq((mu - 3*sigma), a, length = 1000)
  yval = dnorm(xval, mean = mu, sd = sigma)
  polygon(c(xval, a), c(yval, 0), col = "#9482FF")

  #calculate probability/area under curve
  area = round(pnorm(a, mean = mu, sd = sigma),4)

  #return list
  return(list(mu = mu, sigma = sigma,  a = a, area = area))
}
