cat(date(),'\n', file = stderr())
options(scipen=100)

if (interactive()) {
  .First <- function() try(utils::loadhistory("~/.Rhistory"))
  .Last <- function() try(utils::savehistory("~/.Rhistory"))
}

setwd <- function(...) {
  base::setwd(...)
  options(prompt=paste(getwd(), "$ ", sep=""))
}

setwd(system("echo $HOME", intern=T)[1])
