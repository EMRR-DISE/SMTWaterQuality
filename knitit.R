#script to knit the document

library(knitr)
knit("docs/index.Rmd", output = "docs/index.html")


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
gitcommit()
gitpush()
