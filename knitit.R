#script to knit the document
library(rmarkdown)
library(knitr)
setwd("C:/Users/rhartman/OneDrive - California Department of Water Resources/smelt cages/SMTWaterQuality/docs")
render("index.Rmd", output_format = "html_document")


# Git add.
gitadd <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add --all"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

# Git commit.
gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

# Git push.
gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}
gitadd()
gitcommit(msg = paste("Auto update", Sys.time()))
gitpush()
