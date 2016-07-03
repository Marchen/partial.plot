require(roxygen2)
require(devtools)


get.this.file.dir <- function() {
   	cmdArgs <- commandArgs(trailingOnly = FALSE)
   	needle <- "--file="
   	match <- grep(needle, cmdArgs)
   	if (length(match) > 0) {
   		# Rscript
   		return(dirname(sub(needle, "", cmdArgs[match])))
   	} else {
   		# 'source'd via R console
   		return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
   	}
}

setwd(get.this.file.dir())

roxygenize(clean = TRUE)
build()
build(binary = TRUE, args = "--preclean")
install()


