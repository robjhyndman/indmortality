#' Make demogdata objects for particular states and years.
#' 
#' The function \code{make.demogdata} will return a demogdata object if
#' \code{cut!="ALL"}. Otherwise it returns a list of demogdata objects for all
#' cuts for a particular state.
#' 
#' If there are more years of deaths than years of population, then the last
#' year of available population data is used to compute the rates for
#' subsequent years.
#' 
#' @param state Character code indicating state or territory of Australia, or
#' "AUS" indicating the whole of Australia.
#' @param sex If "ALL", the function will return all sexes as part of each
#' demogdata object. Otherwise the function will return only the sex indicated.
#' @param cut Which cut of data to use. If "ALL", then a list of demogdata
#' objects is returned.
#' @param year Years to include in the demogdata object. The default is to
#' include all available years from the deaths matrix.
#' @param aveyear Logical value indicating whether results should be averaged
#' across years. Default is FALSE.
#' @param upper.age Upper age group, by default set to 100+.
#' @param smooth Logical value indicating whether mortality rates should be
#' smoothed using penalized regression splines. Default is TRUE.
#' @param deaths A matrix containing all unit death records in the form of
#' \code{\link{ideaths}}.
#' @param pop A list of list of matrices containing population numbers. This
#' should be in the form of \code{\link{ipop}}.
#' @return A demogdata object (if \code{cut!="ALL"}) or a list of demogdata
#' objects (if \code{cut=="ALL"}). See \link[demography]{demogdata} for more
#' information about demogdata objects.
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @examples
#' 
#' nsw.M <- make.demogdata(state="NSW",cut="M")
#' plot(nsw.M, "female")
#' 
#' # Show smoothing
#' test <- make.demogdata(state="NSW", cut="M", sex="female", smooth=FALSE)
#' test.sm <- smooth.demogdata(test)
#' plot(test, year=2006, type="p", pch=1)
#' lines(test.sm, year=2006, col="red")
#' 
#' @export
make.demogdata <- function(state=c("AUS","ACT","NSW","NT","QLD","SA","TAS","VIC","WA"),
sex=c("ALL","female","male","total"), cut=c("ALL","M","H","A","N"),
    year=NULL, aveyear=FALSE, upper.age=100, smooth=TRUE, deaths=indmortality::ideaths, pop=indmortality::ipop)
{
  cut <- match.arg(cut)
  state <- match.arg(state)
  sex <- match.arg(sex)
  if(cut!="ALL")
    return(make.demogdata.cut(deaths,pop,state,sex,cut,year,upper.age,smooth,aveyear))
  # Otherwise continue with all cuts
  out <- list()
  cut <- c("M", "H", "A", "N")
  for(i in 1:length(cut))
    out[[i]] <- make.demogdata.cut(deaths,pop,state,sex,cut[i],year,upper.age,smooth,aveyear)
  if(smooth) # Check rates increase with more deaths data. Should happen anyway for non-smoothed data
  {
    for(j in 1:length(out[[1]]$rate))
      for(i in 2:length(cut))
        out[[i]]$rate[[j]] <- pmax(out[[i-1]]$rate[[j]],out[[i]]$rate[[j]])
  }
  names(out) <- cut
  return(structure(out,class="gdemogdata"))
}

#' @export
make.demogdata.cut <- function(deaths, pop, state=c("NSW","VIC","QLD","WA","SA","TAS","ACT","NT","AUS"),
    sex=c("ALL","female","male","total"), cut=c("M", "H", "A", "N"), year=NULL, upper.age=100, smooth=TRUE, aveyear=FALSE)
{
  state <- match.arg(state)
  cut <- match.arg(cut)
  sex <- match.arg(sex)

  # Check years required and available
  availableyears <- sort(unique(as.numeric(as.character(deaths$Year))))
  if(is.null(year))
    year <- availableyears
  else
    year <- year[is.element(year,availableyears)]

  # Get male deaths and pop
  if(sex!="female")
  {
    mdeaths <- as.matrix(deathstable(state, "male",   cut, year, aveyear, upper.age, deaths=deaths))
    upper.age <- nrow(mdeaths)-1
    mpop <- pop[[state]][["male"]]
    if(upper.age < nrow(mpop)-1)
      mpop <- rbind(mpop[1:upper.age,,drop=FALSE],colSums(mpop[(upper.age+1):nrow(mpop),,drop=FALSE]))
    else
    {
      upper.age <- nrow(mpop)-1
      if(upper.age < nrow(mdeaths)-1)
        mdeaths <- rbind(mdeaths[1:upper.age,,drop=FALSE],colSums(mdeaths[(upper.age+1):nrow(mdeaths),,drop=FALSE]))
    }
    ppop <- mpop
  }
  # Get female deaths and pop
  if(sex!="male")
  {
    fdeaths <- as.matrix(deathstable(state, "female", cut, year, aveyear, upper.age, deaths=deaths))
    upper.age <- nrow(fdeaths)-1
    fpop <- pop[[state]][["female"]]
    if(upper.age < nrow(fpop)-1)
      fpop <- rbind(fpop[1:upper.age,,drop=FALSE],colSums(fpop[(upper.age+1):nrow(fpop),,drop=FALSE]))
    else
    {
      upper.age <- nrow(fpop)-1
      if(upper.age < nrow(fdeaths)-1)
        fdeaths <- rbind(fdeaths[1:upper.age,,drop=FALSE],colSums(fdeaths[(upper.age+1):nrow(fdeaths),,drop=FALSE]))
    }
    ppop <- fpop
  }
  # Get total deaths
  if(sex=="ALL" | sex=="total")
    tdeaths <- mdeaths + fdeaths

  # Assume last year population for all subsequent years
  pyear <- as.numeric(colnames(ppop))
  pyear <- c(pyear,max(pyear)+(1:50))
  j <- pmin(ncol(ppop),which(is.element(pyear,year)))
  if(aveyear)
    year <- mean(year)

  # Get female rates
  if(sex!="male")
  {
    fpop <- fpop[,j,drop=FALSE]
    if(aveyear)
      fpop <- matrix(rowMeans(fpop),ncol=1)
    frate <- as.matrix(fdeaths/fpop)
    rownames(frate) <- rownames(fpop) <- paste(0:upper.age,c(rep("",upper.age),"+"),sep="")
  }
  # Get male rates
  if(sex!="female")
  {
    mpop <- mpop[,j,drop=FALSE]
    if(aveyear)
      mpop <- matrix(rowMeans(mpop),ncol=1)
    mrate <- as.matrix(mdeaths/mpop)
    rownames(mrate) <- rownames(mpop) <- paste(0:upper.age,c(rep("",upper.age),"+"),sep="")
  }
  # Get total pop and rates
  if(sex=="ALL" | sex=="total")
  {
    tpop <- fpop+mpop
    trate <- as.matrix(tdeaths/tpop)
    rownames(trate) <- rownames(tpop) <- paste(0:upper.age,c(rep("",upper.age),"+"),sep="")
  }

  # Construct demogdata object
  if(sex=="female")
    cut <- demogdata(frate,fpop,ages=0:upper.age,years=year,
      type="mortality",label=paste("Indigenous",state,cut),name="female",lambda=0)
  else if(sex=="male")
    cut <- demogdata(mrate,mpop,ages=0:upper.age,years=year,
      type="mortality",label=paste("Indigenous",state,cut),name="male",lambda=0)
  else
    cut <- demogdata(trate,tpop,ages=0:upper.age,years=year,
      type="mortality",label=paste("Indigenous",state,cut),name="total",lambda=0)

  if(sex=="ALL")
  {
    cut$rate$female <- frate
    cut$rate$male <- mrate
    cut$rate$total <- trate
    cut$pop$female <- fpop
    cut$pop$male <- mpop
    cut$pop$total <- tpop
  }

  # Smooth rates
  if(smooth)
  {
    smoothcut <- try(smooth.demogdata(cut,b=30,k=30), silent=TRUE)
    if(class(smoothcut)=="try-error")
    {
      smoothcut <- try(smooth.demogdata(cut,b=30,k=20), silent=TRUE)
      if(class(smoothcut)=="try-error")
      {
        smoothcut <- try(smooth.demogdata(cut,b=30,k=10), silent=TRUE)
        if(class(smoothcut)=="try-error") # Give up
        {
          warning("Not enough available data to do any smoothing")
          smoothcut <- cut
        }
      }
    }
    return(smoothcut)
  }
  else
    return(cut)

}

