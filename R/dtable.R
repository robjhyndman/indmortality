# Create table of total deaths for a specific state/sex combination



#' Function to make deaths tables for subsets of data.
#' 
#' Returns array with ages and years and (depending on the arguments) states,
#' sex and cut. Default arguments will return the complete array. Use the
#' argument to specify subsets of the array. Uses all available years unless
#' aveyear=TRUE, in which case it will return average deaths across years.
#' 
#' 
#' @param state If "ALL", the function will return all deaths with state as a
#' dimension of the array. Otherwise the function will return all deaths from
#' the state indicated but with state not a dimension of the array.
#' @param sex If "ALL", the function will return all deaths with sex as a
#' dimension of the array. Otherwise the function will return all deaths from
#' the sex indicated but with sex not a dimension of the array.
#' @param cut If "ALL", the function will return all deaths with state as a
#' dimension of the array. Otherwise the function will return all deaths from
#' the state indicated but with state not a dimension of the array.
#' @param year Which years to be returned. Default is all available years.
#' years is always a dimension of the returned array.
#' @param upper.age Deaths are cumulated for higher ages into this upper age
#' group.
#' @param aveyear If TRUE, the deaths are averaged across years.
#' @param deaths Matrix containing all deaths with indigenous classification in
#' Australia from 2001-2007 in the same format as \code{\link{deaths}}.
#' @param cumulative Indicates if the deaths are to be cumulated up to the
#' specified "cut", or whether the marginal deaths associated with that cut are
#' to be returned.
#' @return A numerical array with death counts in each cell. Dimensions of the
#' array are age, year and then (depending on the arguments) state,sex,cut.
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @examples
#' 
#' tab1 <- deathstable(sex="female")
#' tab2 <- deathstable(state="VIC", cut="H", aveyear=TRUE)
#' tab3 <- deathstable()
#' 
#' @export
deathstable <- function(
  state=c("AUS","ACT","NSW","NT","QLD","SA","TAS","VIC","WA"),
  sex=c("female","male","total"),
  linked=c(NA,TRUE,FALSE),
  classified=c(NA,"N","Y","U"),
  year=NULL,
  aveyear=FALSE,
  upper.age=100,
  cumulative=TRUE,
  deaths=indmortality::ideaths)
{
  state <- match.arg(state)
  sex <- match.arg(sex)
  maxage <- max(deaths$Age,na.rm=TRUE)

  # Subset on year
  nyear <- deaths$RegYear
  dyear <- sort(unique(nyear))
  if(is.null(year))
    year <- dyear
  else if(sum(!is.element(year,dyear)) > 0)
    warning("Some years unavailable")
  deaths <- subset(deaths,is.element(nyear,year))
  deaths$Year <- as.factor(deaths$RegYear)

  # Subset on state
  if(state != "AUS")
    deaths <- subset(deaths,deaths$State==state)

  # Subset on sex
  if(sex=="male")
    deaths <- subset(deaths,deaths$Sex=="M")
  else if(sex=="female")
    deaths <- subset(deaths,deaths$Sex=="F")

  # Subset on linked
  if(!is.na(linked))
  {
    if(linked)
      deaths <- subset(deaths, deaths$LinkStatus=="Linked")
    else
      deaths <- subset(deaths, deaths$LinkStatus=="Unlinked")
  }

  # Subset on indigenous status
  if(!is.na(classified))
    deaths <- subset(deaths, deaths$Indigenous==classified)

  if(nrow(deaths)==0)
    stop("No deaths selected")

  # Make age a factor
  deaths$age1 <- factor(deaths$Age, exclude=NULL, levels=c(paste(0:maxage),"missing"))
  deaths$age1[is.na(deaths$age1)] <- "missing"


  # Sum up data where age is not missing
  tab <- xtabs(~ age1 + Year + Sex + State, data = deaths)

  if(state!="AUS")
    tab <- tab[,,,state,drop=FALSE]
  if(sex=="male")
    tab <- tab[,,"M",,drop=FALSE]
  else if(sex=="female")
    tab <- tab[,,"F",,drop=FALSE]

  tab <- apply(tab,2:length(dim(tab)),distribute.deaths.vec)

  # Aggregate upper ages
  if(upper.age < maxage)
  {
    tab[upper.age+1,,,] <- apply(tab[(upper.age:maxage)+1,,,,drop=FALSE],2:4,sum)
    tab <- tab[1:(upper.age+1),,,,drop=FALSE]
    maxage <- upper.age
  }
  dimnames(tab)[[1]][maxage+1] <- paste(dimnames(tab)[[1]][maxage+1],"+",sep="")

  # Collapse results if required
  dnames <- names(dimnames(tab)) <- c("Age","Year","Sex","State")
  if(sex=="total")
  {
    dnames <- dnames[dnames!="Sex"]
    tab <- apply(tab,dnames,sum)
  }
  if(state=="AUS")
  {
    dnames <- dnames[dnames!="State"]
    tab <- apply(tab,dnames,sum)
  }
  if(aveyear)
  {
    dnames <- dnames[dnames!="Year"]
    tab <- apply(tab,dnames,mean)
  }

  # Add pro-rata deaths due to missing ages


  # Remove dimensions of length 1
  dl <- dim(tab)
  tab <- apply(tab,which(dl>1),function(x){x})

  # Return resulting table
  return(tab)
}

# Find all deaths associated with missing ages and distribute across ages
# according to existing age distribution
# This assumes age missing at random
distribute.deaths.vec <- function(x)
{
  miss <- names(x)=="missing"
  sumx <- sum(x[!miss])
  if(sumx > 0)
    x[!miss] <- x[!miss] * sum(x)/sumx
  return(x[!miss])
}
