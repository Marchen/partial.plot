require(roxygen2)
require(devtools)
require(rmarkdown)

#------------------------------------------------------------------------------
#	Change working directory to package directory
#------------------------------------------------------------------------------
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

old.wd <- setwd(get.this.file.dir())


#------------------------------------------------------------------------------
#	Build documentation.
#------------------------------------------------------------------------------
unlink("man", recursive = TRUE)
document()

unlink("inst/doc", recursive = TRUE)
build_vignettes()


#------------------------------------------------------------------------------
#	Build source package
#------------------------------------------------------------------------------
if (!file.exists("build")) {
	dir.create("build")
}
build(path = "build", vignettes = FALSE)


#------------------------------------------------------------------------------
#	Restore working directory.
#------------------------------------------------------------------------------
setwd(old.wd)
rm(old.wd)
