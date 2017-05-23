.onAttach <- function(...)
{
  version <- packageVersion("indmortality2")
  packageStartupMessage(paste("This is indmortality",version,"\n"))
}
