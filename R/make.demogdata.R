#' Make demogdata objects for particular states and years.
#'
#' The function \code{make.demogdata} will return a demogdata object
#' for the given state and years.
#'
#' @param state Character code indicating state or territory of Australia, or
#' "AUS" indicating the whole of Australia.
#' @param year Years to include in the demogdata object. The default is to
#' include all available years from the \link{ideaths} database.
#' @param aveyear Logical value indicating whether deaths and population numbers
#' should be averaged across years. Default is FALSE.
#' @param upper.age Upper age group, by default set to 100+.
#' @param smooth Logical value indicating whether mortality rates should be
#' smoothed using penalized regression splines. Default is TRUE.
#' @param population Character code indicating which estimated residential
#' indigenous population should be used. \code{"cohort"} means use the ERP
#' obtained by linearly interpolating the 2001, 2006 and 2011 census figures
#' along cohorts. \code{"interpolated"} means use the ERP obtained by linearly
#' interpolating the 2001, 2006 and 2011 census figures along ages.
#' \code{"backcast"} means use the ABS ERP based on 2011 census values.
#' @return A demogdata object containing mortality rates for males, females and total.
#' See \link[demography]{demogdata} for more information about demogdata objects.
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @examples
#'
#' nsw <- make.demogdata(state="NSW")
#' plot(nsw, "female")
#'
#' # Show smoothing
#' test <- make.demogdata(state="NSW", smooth=FALSE)
#' test.sm <- demography::smooth.demogdata(test)
#' plot(test, series='female', year=2006, type="p", pch=1)
#' lines(test.sm, series='female', year=2006, col="red")
#'
#' @export


make.demogdata <- function(state=c("AUS","ACT","NSW","NT","QLD","SA","TAS","VIC","WA"),
year=2001:2010, aveyear=FALSE, upper.age=100, smooth=TRUE, population=c("cohort","interpolated","backcast"))
{
  state <- match.arg(state)
  population <- match.arg(population)

  if(min(year) < 2001 | max(year) > 2010)
    stop("year values must be between 2001 and 2010")
  else
    year <- sort(unique(year))

  # Get deaths
  if(aveyear)
  {
    nyears <- length(year)
    fdeaths <- as.matrix(deathstable(state, sex="female", year=year, upper.age=upper.age))/nyears
    mdeaths <- as.matrix(deathstable(state, sex="male", year=year, upper.age=upper.age))/nyears
  }
  else
  {
    fdeaths <- mdeaths <- matrix(0, nrow=upper.age+1, ncol=length(year))
    for(i in 1:length(year))
    {
      fdeaths[,i] <- as.matrix(deathstable(state, sex="female", year=year[i], upper.age=upper.age))
      mdeaths[,i] <- as.matrix(deathstable(state, sex="male", year=year[i], upper.age=upper.age))
    }
  }
  # Make sure deaths are non-negative
  mdeaths <- pmax(mdeaths,0)
  fdeaths <- pmax(fdeaths,0)

  # Get population
  if(population=="backcast")
    pop <- indmortality2::ipop$Backcast
  else if(population=="interpolated")
    pop <- indmortality2::ipop$Interpolated
  else
    pop <- indmortality2::ipop$Cohort
  pop <- subset(pop, pop$State==state)
  mpop <- subset(pop, pop$Sex=="Male")
  fpop <- subset(pop, pop$Sex=="Female")
  if(upper.age < 115)
  {
    mpop[upper.age+1,4:14] <- colSums(mpop[(upper.age+1):NROW(mpop),4:14])
    mpop <- mpop[1:(upper.age+1),]
    fpop[upper.age+1,4:14] <- colSums(fpop[(upper.age+1):NROW(fpop),4:14])
    fpop <- fpop[1:(upper.age+1),]
  }
  mpop <- as.matrix(mpop[,year-2001+4,drop=FALSE])
  fpop <- as.matrix(fpop[,year-2001+4,drop=FALSE])

  if(aveyear)
  {
    fpop <- as.matrix(rowSums(fpop))/nyears
    mpop <- as.matrix(rowSums(mpop))/nyears
    year <- mean(year)
  }

  # female rates
  frate <- as.matrix(fdeaths/fpop)
  frate[is.na(frate)] <- 0
  frate[abs(frate)>1e10] <- 0
  rownames(frate) <- rownames(fpop) <- paste(0:upper.age,c(rep("",upper.age),"+"),sep="")

  # male rates
  mrate <- as.matrix(mdeaths/mpop)
  mrate[is.na(mrate)] <- 0
  mrate[abs(mrate)>1e10] <- 0
  rownames(mrate) <- rownames(mpop) <- paste(0:upper.age,c(rep("",upper.age),"+"),sep="")

  # Total rates
  tpop <- fpop+mpop
  trate <- as.matrix((fdeaths+mdeaths)/tpop)
  trate[is.na(trate)] <- 0
  trate[abs(trate)>1e10] <- 0
  rownames(trate) <- rownames(tpop) <- paste(0:upper.age,c(rep("",upper.age),"+"),sep="")

  # Construct demogdata object
  out <- demogdata(trate,tpop,ages=0:upper.age,years=year,
      type="mortality",label=paste("Indigenous",state),name="total",lambda=0)

  out$rate$female <- frate
  out$rate$male <- mrate
  out$pop$female <- fpop
  out$pop$male <- mpop

  # Smooth rates
  if(smooth)
  {
    smoothout <- try(demography::smooth.demogdata(out,b=25,k=30), silent=TRUE)
    if(class(smoothout)=="try-error")
    {
      smoothout <- try(demography::smooth.demogdata(out,b=25,k=20), silent=TRUE)
      if(class(smoothout)=="try-error")
      {
        smoothout <- try(demography::smooth.demogdata(out,b=25,k=10), silent=TRUE)
        if(class(smoothout)=="try-error") # Give up
        {
          warning("Not enough available data to do any smoothing")
          smoothout <- out
        }
      }
    }
    # Cap smoothed rates at 1.5
    nl <- length(smoothout$rate)
    for(i in seq(nl))
      smoothout$rate[[i]] <- pmin(smoothout$rate[[i]], 1.50)
    return(smoothout)
  }
  else
    return(out)

}

