#' MyBin
#'
#' @title Interation Function
#'
#' @param iter Number of Iterations
#' @param n Number of population
#' @param p Chance of success
#'
#' @return A barplot of successes
#' @export
#'
#' @examples
#' \dontrun{(iter=100, n=10, p=0.8)}
#'
#'
#'
mybin=function(iter=100,n=10, p=0.7){

  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)

  succ=c()
  for( i in 1:iter){

    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    succ[i]=sum(sam.mat[,i])
  }

  succ.tab=table(factor(succ,levels=0:n))

  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
