#' Calculate life expectancies for both sexes and all cuts of the deaths
#' matrix.
#' 
#' \code{life.expectancy.mean} calculates life expectancies and
#' \code{life.expectancy.ci} computes confidence intervals for life
#' expectancies. The functions take the output from
#' \code{\link{make.demogdata}} and returns life expectancy results for all
#' populations for the specified ages.
#' 
#' The life expectancy is calculated using a life table approach based on the
#' mortality rates in \code{object}.
#' 
#' @aliases life.expectancy.mean life.expectancy.ci
#' @param object Output from \code{\link{make.demogdata}}.
#' @param ages Ages for which life expectancy is required.
#' @param nsim Number of simulations to use in the calculation.
#' @param level Confidence level for the intervals.
#' @param smooth If TRUE, smoothing will be used before computing life
#' expectancies.
#' @param sigma Value of sigma used in PES estimate for simulations.
#' @return An array containing life expectancies for each year, age, sex and
#' cut contained in \code{object}.
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @seealso \link[demography]{life.expectancy}
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @examples
#' 
#' nsw.nmd <- make.demogdata(state="NSW", cut="M", aveyear=TRUE)
#' life.expectancy.mean(nsw.nmd)
#' life.expectancy.ci(nsw.nmd)
#' 
#' @export

life.expectancy.mean <- function(object,ages=c(0,20,45,65))
{
  if(class(object)=="gdemogdata")
  {
    out <- array(NA,c(length(ages),length(object[[1]]$year), length(object[[1]]$rate),length(object)))
    for(i in 1:length(object))
      out[,,,i] <- life.expectancy.mean(object[[i]],ages)
    dimnames(out) <- list(as.character(ages), as.character(object[[1]]$year), names(object[[1]]$rate),names(object))
  }
  else if(class(object)=="demogdata")
  {
    out <- array(NA,c(length(ages),length(object$year),length(object$rate)))
    for(i in 1:length(ages))
    {
      for(j in 1:length(object$rate))
        out[i,,j] <- life.expectancy(object,series=names(object$rate)[j],age=ages[i])
    }
    dimnames(out) <- list(as.character(ages), as.character(object$year), names(object$rate))
  }

  # Remove dimensions of length 1
  dl <- dim(out)
  out <- apply(out,which(dl>1),function(x){x})

  return(out)
}

#' @export

life.expectancy.ci <- function(object, ages=c(0,20,45,65), nsim=250, level=95, smooth=FALSE,
  sigma=0.0295)
{
  if(length(level)>1)
    stop("I can only compute one confidence interval at a time")
  if(level > 1 & level < 100)
    level <- level/100
  if(level < 0 | level > 1)
    stop("Inappropriate value for level")
  if(class(object) != "demogdata" & class(object) != "gdemogdata")
    stop("object is not of demogdata or gdemogdata class")
  if(class(object)=="demogdata")
    object <- structure(list(object),class="gdemogdata")
  # Remove totals to save time
  for(i in 1:length(object))
    object[[i]]$rate$total <- object[[i]]$pop$total <- NULL

  out <- array(NA,c(length(ages),length(object[[1]]$year),length(object[[1]]$rate),length(object),nsim))
  newobj <- list()
  for(i in 1:nsim)
  {
    newobj <- simulate(object, sigma=sigma)
    if(smooth)
    {
      for(j in 1:length(object))
        newobj[[j]] <- smooth.demogdata(newobj[[j]])
    }
    out[,,,,i] <- life.expectancy.mean(newobj,ages=ages)
  }
  out <- aperm(apply(out,1:4,quantile,prob=0.5 + c(-.5,.5)*level),c(2:5,1))
  dimnames(out) <- list(as.character(ages),as.character(object[[1]]$year), names(object[[1]]$rate), names(object), paste(level*100,c("% Lower","% Upper"),sep=""))

  # Now remove bias by re-centering confidence intervals
  lifeexp <- life.expectancy.mean(object,ages=ages)
  mid <- 0.5*(out[,,,,2]+out[,,,,1])
  adjust <- mid - lifeexp
  out <- sweep(out, 1:4, adjust)

  # Remove dimensions of length 1
  dl <- dim(out)
  out <- apply(out,which(dl>1),function(x){x})

  return(out)
}
