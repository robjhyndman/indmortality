#' Lists of demogdata objects for all Australian states, 2001-2007
#' 
#' Objects: \code{aus}, \code{nsw}, \code{vic}, \code{qld}, \code{wa},
#' \code{sa} and \code{nt}.
#' 
#' These are lists of demogdata objects. Each state contains four demogdata
#' objects, corresponding to the M, H, A and N cuts of the indigenous deaths
#' data. For information about demogdata objects, see
#' \code{\link[demography]{demogdata}}.
#' 
#' Demogdata objects constructed using \code{\link{make.demogdata}} using
#' 2001-2007 data from \code{\link{ideaths}} and \code{\link{ipop}}.
#' 
#' @name aus
#' @aliases aus nsw vic qld nt sa wa
#' @docType data
#' @keywords datasets
#' @examples
#' 
#' plot(nsw$M,'male')
#' 
NULL





#' Database of all deaths with indigenous classification in Australia from
#' 2001-2007.
#' 
#' Database constructed by linking National Mortality Database, hospital
#' records, residential aged care records and neonatal records. The
#' \code{ideaths} matrix was formed using the \code{\link{read.deaths}} command
#' using the data set available on 22 November 2011.
#' 
#' 
#' @name ideaths
#' @docType data
#' @format A data frame with 16625 observations on the following six variables:
#' \describe{ \item{list("State")}{a factor with levels \code{ACT}, \code{NSW},
#' \code{NT}, \code{QLD}, \code{SA}, \code{TAS}, \code{VIC}, \code{WA}.}
#' \item{list("Year")}{a numeric vector indicating year of registration.}
#' \item{list("Deathyear")}{a numeric vector indicating year of death.}
#' \item{list("Sex")}{a factor with levels \code{F} and \code{M}.}
#' \item{list("Age")}{a numeric vector indicating age at death.}
#' \item{list("Cut")}{a factor with levels \code{M}, \code{H}, \code{A} and
#' \code{N} indicating the database where the death was first recorded
#' (treating the levels in order).} }
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @keywords datasets
#' @examples
#' 
#' summary(ideaths)
#' 
NULL





#' Australian indigenous mortality rates and life expectancies
#' 
#' Package prepared for the Australian Institute of Health and Welfare to
#' compute life expectancy and other mortality-related information for the
#' Australian indigenous population using simulation and nonparametric
#' smoothing.
#' 
#' \tabular{ll}{ Package: \tab indmortality\cr Type: \tab Package\cr Version:
#' \tab 1.0\cr License: \tab GPL 2\cr }
#' 
#' @name indmortality-package
#' @aliases indmortality-package indmortality
#' @docType package
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' 
#' Maintainer: Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @seealso \code{\link[demography:demography-package]{demography}}
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @keywords package
#' @examples
#' 
#' nsw.nmd <- make.demogdata(state="NSW", cut="M", sex="male", aveyear=TRUE)
#' plot(nsw.nmd)
#' life.expectancy.mean(nsw.nmd)
#' life.expectancy.ci(nsw.nmd)
#' 
NULL





#' Populations for all Australian states, 2001-2006
#' 
#' This is a list of lists of matrices. The first level is the Australian
#' states and the second level contains sex groups. Each matrix is of size
#' 101x6 with single ages in each row and calendar years in columns. For
#' example, ipop$NSW$male contains the estimated indigenous male population for
#' NSW in 2001-2006.
#' 
#' Population estimates are based on ABS 3238.0 which gives five year age
#' groups up to 85+. These are distributed according to earlier single-year-age
#' estimates of the state indigenous populations, except for the ACT for which
#' the five-year age groups are distributed according to the NSW
#' single-year-age estimates of the indigenous population. The population for
#' 85+ in each state is distributed according to the general Australian age
#' distribution for that age group.
#' 
#' @name ipop
#' @docType data
#' @source ABS Cat 3238.0
#' @keywords datasets
#' @examples
#' 
#' plot(0:100,ipop$NT$male[,"2003"],type="l",xlab="Age",ylab="Population",
#'   main="Northern Territory 2003",col="blue")
#' lines(0:100,ipop$NT$female[,"2003"],col="red")
#' legend("topright",lty=1,col=c("blue","red"),legend=c("male","female"))
#' 
NULL



