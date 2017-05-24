#' Simulate a new demogdata object based on an existing demogdata object using
#' a parametric bootstrap.
#'
#' Populations are simulated using a normal distribution with standard
#' deviation based on PES estimates. Deaths are estimated using a Poisson
#' distribution. This function assumes that the rates for the demogdata object
#' used as an argument are smoothed. When the simulated deaths are equal to
#' zero (so the simulated rate is then either zero or undefined), the simulated
#' rate is set to be equal to the smoothed rate contained in \code{object}.
#' Otherwise, the simulated rate is set to be the simulated deaths divided by
#' the simulated population.
#'
#'
#' @importFrom stats simulate
#'
#' @param object A demogdata object that will be used as the basis for the
#' simulation.
#' @param nsim Number of simulated populations returned. This argument is
#' ignored.
#' @param seed Either NULL or an integer that will be used in a call to
#' set.seed before simulation. The default, NULL will not change the random
#' generator state.
#' @param sigma PES sigma estimate.
#' @param ... Other arguments are ignored.
#' @return A demogdata object of the same structure as \code{object}.
#' @author Rob J Hyndman <Rob.Hyndman@@monash.edu>
#' @references Choi, C., Hyndman, R.J., Smith, L., and Zhao, K. (2010) \emph{An
#' enhanced mortality database for estimating indigenous life expectancy}.
#' Report for Australian Institute of Health and Welfare.
#' @examples
#'
#' plot(nt$N,year=2003)
#' points(simulate(nt$N),year=2003)
#'
#' # Example of infant.mortality CI for NT males
#' nsim <- 1000
#' infant.mort <- numeric(nsim)
#' ntmale <- make.demogdata(state="NT",cut="N",sex="male",aveyear=TRUE)
#' for(i in 1:nsim)
#' {
#'   sim <- simulate(ntmale)
#'   infant.mort[i] <- sim$rate$male[1,1]
#' }
#' # 95% CI for infant mortality rate per 100,000 boys aged 0
#' # (not the same as per 100,000 births)
#' quantile(infant.mort,c(0.025,0.975))*100000
#'
#' # Example of total mortality rate for WA
#' nsim <- 1000
#' TMR <- matrix(NA,nsim,3)
#' wa <- make.demogdata(state="WA", cut="N", aveyear=TRUE)
#' colnames(TMR) <- c("male","female","total")
#' for(i in 1:nsim)
#' {
#'   sim <- simulate(wa)
#'   TMR[i,1] <- sum(sim$rate$male * sim$pop$male) / sum(sim$pop$male)
#'   TMR[i,2] <- sum(sim$rate$female * sim$pop$female) / sum(sim$pop$female)
#'   TMR[i,3] <- sum(sim$rate$total * sim$pop$total) / sum(sim$pop$total)
#' }
#' # 95% CI for total mortality rate per 100,000 people
#' # (not age-standardized)
#' TMR.ci <- apply(TMR,2,quantile,prob=c(0.025,0.975))*100000
#'
#' @export
simulate.demogdata <- function(object, nsim=1, seed=NULL, sigma=0.0295, ...)
{
  ny <- length(object$year)
  nx <- length(object$age)

  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    stats::runif(1)
  if(is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else
  {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    base::set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  for(i in seq_along(object$rate))
  {
    origrates <- object$rate[[i]]

    # Population
    object$pop[[i]] <- object$pop[[i]] + matrix(stats::rnorm(nx*ny,0,sd=sigma*object$pop[[i]]),nrow=nx)
    object$pop[[i]] <- pmax(object$pop[[i]],0)

    # Deaths
    deaths <- matrix(stats::rpois(nx*ny, object$rate[[i]]*object$pop[[i]]), nrow=nx)

    # Rates
    object$rate[[i]] <- deaths/object$pop[[i]]
    object$rate[[i]][object$pop[[i]]==0] <- 0
    object$rate[[i]][object$rate[[i]]==0] <- origrates[object$rate[[i]]==0]
  }

  return(object)
}

