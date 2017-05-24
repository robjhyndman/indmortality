# Create table of total deaths for a specific state/sex combination



#' Function to make deaths tables for subsets of data.
#'
#' Returns vector of total deaths by single-year of age.
#' Deaths are counted using the method adopted for the
#' ``Enhanced Mortality Database'', adjusting for the
#' under-identification of Indigenous deaths on the
#' National Death Index.
#'
#' @param state The function will return all deaths from the state indicated.
#' @param sex The function will return all deaths from the sex indicated.
#' @param year Which years to be counted. Default is all available years.
#' @param upper.age Deaths are cumulated for higher ages into this upper age
#' group.
#' @return A numerical vector with death counts in each cell.
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @examples
#'
#' vicfdeaths2010 <- deathstable(state="VIC", sex="female", year=2010)
#' plot(0:115, vicfdeaths2010, xlab="Age", ylab="Number of deaths",
#'   main="Indigenous deaths in Victoria: females 2010")
#'
#' @export
deathstable <- function(
  state=c("AUS","ACT","NSW","NT","QLD","SA","TAS","VIC","WA"),
  sex=c("female","male","total"),
  year=2001:2010,
  upper.age=115)
{
  # Check arguments
  state <- match.arg(state)
  sex <- match.arg(sex)

  # Maximum age at death
  maxage <- max(ideaths$Age,na.rm=TRUE)

  # Make a copy of ideaths to subset
  deaths <- ideaths

  # Subset data on state
  if(state != "AUS")
    deaths <- subset(deaths,State==toupper(state))

  # Subset data on sex
  if(sex=="male")
    deaths <- subset(deaths,Sex=="Male")
  else if(sex=="female")
    deaths <- subset(deaths,Sex=="Female")

  # Create year as a factor
  deaths$Year <- as.factor(deaths$RegYear)

  # Make age a factor and change NA to "missing"
  deaths$age1 <- factor(deaths$Age, exclude=NULL,
                        levels=c(paste(0:maxage),"missing"))
  deaths$age1[is.na(deaths$age1)] <- "missing"

  # Check available years
  dyear <- sort(unique(deaths$RegYear))
  if(is.null(year))
    year <- dyear
  else if(sum(!is.element(year,dyear)) > 0)
    warning("Some years unavailable")

  # Save gains and losses for later
  maxyear <- max(year)
  minyear <- min(year)
  gains <- subset(deaths, deaths$DeathYear < minyear - 0.01 &
                          deaths$RegYear > minyear - 0.01 &
                          deaths$RegYear < maxyear + 0.01)
  losses <- subset(deaths, deaths$RegYear > maxyear + 0.01 &
                     deaths$DeathYear > minyear - 0.01 &
                     deaths$DeathYear < maxyear + 0.01)

  # Subset on years requested
  deaths <- subset(deaths, deaths$RegYear > minyear - 0.01 &
                           deaths$RegYear < maxyear + 0.01)

  # Check that we still have some deaths left after subsetting
  if(nrow(deaths)==0)
    stop("No deaths selected")

  # Sum up data by age, indigenous, linked (i.e., total over sex, state and year)
  tab <- stats::xtabs(~ age1 + Indigenous + Linked, data = deaths)

  ##################################################################
  ###### Now follow spreadsheet logic
  ###### as in "Enhancing_Indigenous Deaths_Phase4_2001_2005.xlsx"
  ##################################################################

  ###### Linked worksheet
  linked <- tab[,,"TRUE"]

  # Distribute missing ages across other ages proportionally
  linked <- apply(linked,2,distribute.deaths.vec)

  # Add in mis-classified deaths
  misclass <- subset(deaths, Misclassified)
  tabmiss <- stats::xtabs(~ age1 + Indigenous, data = misclass)
  # Distribute missing ages across other ages proportionally
  tabmiss <- apply(tabmiss,2:length(dim(tabmiss)),distribute.deaths.vec)
  misclassN <- tabmiss[,"N"]
  misclassU <- tabmiss[,"U"]
  pmisclassN <- misclassN / linked[,"N"]
  pmisclassN[is.na(pmisclassN)] <- 0
  pmisclassU <- misclassU / linked[,"U"]
  pmisclassU[is.na(pmisclassU)] <- 0
  linked.enhanced <- misclassN + misclassU + linked[,"Y"]

  ###### Unlinked worksheet
  unlinked <- tab[,,"FALSE"]

  # Distribute missing ages across other ages proportionally
  unlinked <- apply(unlinked,2,distribute.deaths.vec)

  # Add in mis-classified deaths
  misclassN <- pmisclassN * unlinked[,"N"]
  misclassU <- pmisclassU * unlinked[,"U"]
  unlinked.enhanced <- misclassN + misclassU + unlinked[,"Y"]

  ###### Calculating gains/losses

  # Sum up data by age, indigenous (i.e., total over sex, state and year)
  tabg <- stats::xtabs(~ age1 + Indigenous, data = gains)
  tabl <- stats::xtabs(~ age1 + Indigenous, data = losses)
  netloss <- tabl-tabg

  ###### Adjusting for net gains/losses

  # Distribute missing ages across other ages proportionally
  netloss <- apply(netloss,2,distribute.deaths.vec)

  # Add in mis-classified deaths
  misclassN <- pmisclassN * netloss[,"N"]
  misclassU <- pmisclassU * netloss[,"U"]
  netloss.enhanced <- misclassN + misclassU + netloss[,"Y"]

  # Final enhanced number of deaths
  enhanced <- linked.enhanced + unlinked.enhanced + netloss.enhanced

  # Aggregate upper ages
  if(upper.age < maxage)
  {
    enhanced[upper.age+1] <- sum(enhanced[(upper.age:maxage)+1])
    enhanced <- enhanced[1:(upper.age+1)]
    maxage <- upper.age
  }
  else if(upper.age > maxage)
  {
    enhanced <- c(enhanced, rep(0, upper.age-maxage))
    maxage <- upper.age
  }
  names(enhanced) <- c(paste(0:(maxage-1)), paste(maxage,"+",sep=""))

  # Return resulting death counts
  return(enhanced)
}

# Find all deaths associated with missing ages and distribute across ages
# according to existing age distribution
# This assumes age missing at random
distribute.deaths.vec <- function(x)
{
  miss <- names(x)=="missing"
  sumx <- sum(x[!miss])
  if(sumx > 0)
    x[!miss] <- x[!miss]/sumx * sum(x)
  return(x[!miss])
}
