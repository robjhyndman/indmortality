#' Australian indigenous mortality rates and life expectancies
#'
#' Package prepared for the Australian Institute of Health and Welfare to
#' compute life expectancy and other mortality-related information for the
#' Australian indigenous population using simulation and nonparametric
#' smoothing.
#'
#' @name indmortality-package
#' @aliases indmortality-package indmortality indmortality2
#' @import demography
#' @docType package
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @seealso \code{\link[demography:demography-package]{demography}}
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @keywords package
#' @examples
#'
#' nsw <- make.demogdata(state="NSW", population="backcast")
#' plot(nsw)
#' life.expectancy.mean(nsw)
#' life.expectancy.ci(nsw)
#'
#' # Complete calculation for Australia, 2001-2005.
#' aus1 <- make.demogdata(state="AUS", year=2001:2005, aveyear=TRUE,
#'    population="cohort")
#' aus2 <- make.demogdata(state="AUS", year=2001:2005, aveyear=TRUE,
#'    population="interpolated")
#' aus3 <- make.demogdata(state="AUS", year=2001:2005, aveyear=TRUE,
#'    population="backcast")
#' life.expectancy.mean(aus1)
#' life.expectancy.ci(aus1)
#' life.expectancy.mean(aus2)
#' life.expectancy.ci(aus2)
#' life.expectancy.mean(aus3)
#' life.expectancy.ci(aus3)
#'
NULL


#' Lists of demogdata objects for all Australian states, 2001-2007
#'
#' Objects: \code{aus}, \code{nsw}, \code{vic}, \code{sa}, \code{wa},
#' \code{nt}, and \code{qld}.
#' Note that there is insufficient data to produce similar objects for Tasmania and the ACT.
#' These are demogdata objects, based on backcast population estimates.
#' The rates are smoothed over age using \code{\link[demography]{smooth.demogdata}}.
#' For information about demogdata objects, see \code{\link[demography]{demogdata}}.
#'
#' Demogdata objects constructed using \code{\link{make.demogdata}} using
#' 2001-2010 data from \code{\link{ideaths}} and \code{\link{ipop}}.
#'
#' @name aus
#' @aliases aus nsw vic qld nt sa wa
#' @docType data
#' @keywords datasets
#' @examples
#'
#' plot(aus,'male')
#'
#' # To create similar objects using interpolated population,
#' # use, for example,
#' nsw2 <- make.demogdata(state="NSW", population="interpolated")
#'
NULL





#' Database of all deaths with indigenous classification in Australia from
#' 2001-2010.
#'
#' Database constructed from NDI with state, age and sex information.
#' Each death is classified as Indigenous/Non-Indigenous/Unknown and Linked/Unlinked.
#' In addition, Indigenous deaths have been flagged if they have been misclassified
#' as Non-Indigenous or Unknown.
#'
#' @name ideaths
#' @docType data
#' @format A data frame with 1,357,538 observations on the following eight variables:
#' \describe{
#'   \item{State}{a factor with levels \code{ACT}, \code{NSW},
#'      \code{NT}, \code{QLD}, \code{SA}, \code{TAS}, \code{VIC}, \code{WA}.}
#'   \item{Sex}{a factor with levels \code{Female} and \code{Male}.}
#'   \item{Age}{a numeric vector indicating age at death.}
#'   \item{RegYear}{an integer vector indicating year of registration.}
#'   \item{DeathYear}{an integer vector indicating year of death.}
#'   \item{Indigenous}{a factor with three levels:
#'     \code{N} (non-indigenous), \code{Y} (indigenous), and \code{U} (unknown).}
#'   \item{Linked}{a logical variable indicating if the death was linked on
#'     data sets in addition to the NDI.}
#'   \item{Misclassified}{A logical variable indicating if the death has
#'     been misclassified as non-Indigenous or ``Unknown'' on the NDI.}
#' }
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @keywords datasets
#' @examples
#'
#' summary(ideaths)
#'
NULL







#' Populations for all Australian states, 2001-2006
#'
#' This is a list of data frames each containing indigenous estimated
#' resident population (ERP) data, by state, sex and single-year of age,
#' from 0 to 115.
#'
#' The first data frame contains cohort-interpolated Indigeneous ERP,
#' estimated from the 2001, 2006 and 2011 Census-based Indigenous ERPs.
#' Interpolation is linear between years for each cohort.
#'
#' The second data frame contains age-interpolated Indigeneous ERP,
#' estimated from the 2001, 2006 and 2011 Census-based Indigenous ERPs.
#' Interpolation is linear between years for each age.
#'
#' The third data frame contains backcasts of 2011 census-based Indigenous ERPs.
#' For each state, the total populations are split into males and females using
#' the sex ratios for each age computed from averaged census populations for
#' 2001, 2006 and 2011.
#'
#' Each data frame contains columns \code{State}, \code{Sex}, \code{Age},
#' \code{2001}, ... \code{2011}.
#'
#' @name ipop
#' @docType data
#' @source ABS Cat 3238.0
#' @keywords datasets
#' @examples
#'
#' plot(0:115,
#'   subset(ipop$Backcast, State=="NT" & Sex=="Male")[,"2003"],
#'   type="l",
#'   xlab="Age",
#'   ylab="Population",
#'   main="Northern Territory Male 2003",
#'   col="blue")
#' lines(0:115,
#'   subset(ipop$Interpolated, State=="NT" & Sex=="Male")[,"2003"],
#'   col="red")
#' lines(0:115,
#'   subset(ipop$Cohort, State=="NT" & Sex=="Male")[,"2003"],
#'   col="green")
#'
#' legend("topright",
#'   lty=1,
#'   col=c("blue","red","green"),
#'   legend=c("Backcast","Interpolated","Cohort"))

NULL



