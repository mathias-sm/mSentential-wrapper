#!/usr/bin/env Rscript

# Load a few useful libraries
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(pander))

# Check the arguments and fail informatively if need be
args = commandArgs(trailingOnly=TRUE)
if (length(args)!=2) {
  stop("One argument must be supplied: `./wrapper.R <formula> <request>` with request in `necessary`, `possible`, `what-follows`, `probability` and `verify`.", call.=FALSE)
}

# Check whether the file we expect is here
sourcefile <- "mSentential-v1.1_utf8.lisp"
if(!file.exists(sourcefile)){
  print_warn <- function(x) {
    cat(paste0("[\033[93mWARNING\033[0m] ", x, "\n"))
  }
  print_warn("You don't seem to have the right mReasoner file already.")
  print_warn("Let's download it from modeltheory.org and convert it to UTF-8.")
  destfile <- "mSentential-v1.1.lisp"
  download.file("http://modeltheory.org/programs/mSentential-v1.1.lisp",
                destfile,
                method="auto")
  writeLines(iconv(readLines(destfile),
                   from = "ISO-8859-15", to = "UTF8"),
             sourcefile)
  unlink(destfile)
  print_warn("Done.\n")
}

# Construct the full lisp-compatible formula
formula.to.evaluate <- paste0("(inference '(",args[1],") '",args[2],"?)")

cat(paste0("Table: summary of the output for the formula `",
             formula.to.evaluate,
             "` under extreme parameters for sigma and gamma"))

# This code does the following:
# * reads the experiment code,
# * replaces the placeholder with the formula,
# * feeds it to `ecl` with quieting parameters and the right libraries,
# * parses the out as CSV structured data,
# * and finally summarizes it in a nice pander table
fileConn <- file("tmp.lisp")
readLines("run-experiment.lisp") %>%
  gsub(pattern = "###-formula-placeholder-###", replace=formula.to.evaluate, x=.) %>%
  writeLines(fileConn)
close(fileConn)

system2("ecl",
        c("-q",
          "--load", "split-sequence.lisp", 
          "--load", sourcefile,
          "--load", "tmp.lisp"),
        stdout=T, stderr="/dev/null") %>%
  data.frame(output=.) %>%
  separate(output, c("V1","V2","V3"), sep=",") %>%
  transmute(UseSys2=V2>0.8, UseWeak=V3<0.1, result=V1) %>%
  table %>%
  pander(style = "grid", split.table = Inf)

# Removes the tmp file
unlink("tmp.lisp")
