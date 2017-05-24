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

