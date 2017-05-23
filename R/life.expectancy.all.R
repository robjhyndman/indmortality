#' Calculate life expectancies for both sexes and all cuts of the deaths
#' matrix.
#' 
#' \code{life.expectancy.mean} calculates life expectancies and
#' \code{life.expectancy.ci} computes confidence intervals for life
#' expectancies. The functions take the output from
#' \code{\link{make.demogdata}} and returns life expectancy results for 
#' both sexes and all years for the specified ages.
#' 
#' @aliases life.expectancy.mean life.expectancy.ci
#' @param object Output from \code{\link{make.demogdata}}.
#' @param ages Ages for which life expectancy is required.
#' @param nsim Number of simulations to use in the calculation.
#' @param level Confidence level for the intervals.
#' @param smooth If TRUE, smoothing will be used before computing life
#' expectancies.
#' @param sigma Value of sigma used in PES estimate for simulations.
#' @return An array containing life expectancies for each year, age, and sex 
#' contained in \code{object}.
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @seealso \link[demography]{life.expectancy}
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @examples
#' 
#' life.expectancy.mean(nsw)
#' life.expectancy.ci(nsw)
#' 
#' @export

life.expectancy.mean <- function(object,ages=c(0,20,45,65))
{

  if(!is.element("demogdata",class(object)))
    stop("First argument must be a demogdata object")

  out <- array(NA,c(length(ages),length(object$year),length(object$rate)))
  for(i in 1:length(ages))
  {
    for(j in 1:length(object$rate))
      out[i,,j] <- life.expectancy(object,series=names(object$rate)[j],age=ages[i])
  }
  dimnames(out) <- list(as.character(ages), as.character(object$year), names(object$rate))

  # Remove dimensions of length 1
  dl <- dim(out)
  out <- apply(out,which(dl>1),function(x){x})

  return(out)
}

life.expectancy.ci <- function(object, ages=c(0,20,45,65), nsim=250, level=95, smooth=FALSE,
  sigma=0.0295)
{
  if(!is.element("demogdata",class(object)))
    stop("First argument must be a demogdata object")

  if(length(level)>1)
    stop("I can only compute one confidence interval at a time")
  if(level > 1 & level < 100)
    level <- level/100
  if(level < 0 | level > 1)
    stop("Inappropriate value for level")

  #if(is.element("female", names(object$rate)))
  #  object$rate$total <- object$pop$total <- NULL

  out <- array(NA,c(length(ages),length(object$year),length(object$rate),nsim))
  newobj <- list()
  for(i in 1:nsim)
  {
    newobj <- simulate(object, sigma=sigma)
    if(smooth)
      newobj <- smooth.demogdata(newobj)
    out[,,,i] <- life.expectancy.mean(newobj,ages=ages)
  }
  out <- aperm(apply(out,1:3,quantile,prob=0.5 + c(-.5,.5)*level),c(2:4,1))
  dimnames(out) <- list(as.character(ages),as.character(object$year), names(object$rate),
    paste(level*100,c("% Lower","% Upper"),sep=""))

  # Now remove bias by re-centering confidence intervals
  lifeexp <- life.expectancy.mean(object,ages=ages)
  mid <- 0.5*(out[,,,2]+out[,,,1])
  adjust <- mid - lifeexp
  out <- sweep(out, 1:3, adjust)

  # Remove dimensions of length 1
  dl <- dim(out)
  out <- apply(out,which(dl>1),function(x){x})

  return(out)
}
