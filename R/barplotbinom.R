#' Title
#'
#' @param iter number of times the experiment is run
#' @param n number of options
#' @param p probability of a 1 or 0 occurring
#' @importFrom grDevices rainbow
#'
#' @returns a barplot with the binomial values through the table
#' @export
#'
#' @examples
#' mybin(iter=1000,n=10,p=.5)
mybin <- function(iter=100,n=10,p=.5){
  #creating a matrix
  sample.mx <- matrix(NA,nrow=n,ncol=iter,byrow=TRUE)

  #vector holding successes
  success <- c()

  #creating a table of successes
  for(i in 1:iter) {
    sample.mx[,i] <- sample(c(1,0),n,replace=TRUE,prob=c(p,1-p))

    #calculate statistics from the sample population (taking the sum)
    success[i] <- sum(sample.mx[,i])
  }

  #actually creates the table after parsing through the for loop
  success.tab <- table(factor(success,levels <- 0:n))

  # create the barplot
  barplot(success.tab/(iter), col=rainbow(n+1), main="Binomial Simulation", xlab = "Number of Successes")
  success.tab/iter
}
