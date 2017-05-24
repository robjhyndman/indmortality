.onAttach <- function(...)
{
  version <- utils::packageVersion("indmortality2")
  base::packageStartupMessage(paste("This is indmortality",version,"\n"))
}
