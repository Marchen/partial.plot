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


#-------------------------------------------------------------------------------
#	Build package
#-------------------------------------------------------------------------------
# Build documentation.
roxygenize(clean = TRUE)
build_vignettes()

# Build package
build(path = "../repos/src/contrib")
if (version$os == "mingw32") {
	build(
		binary = TRUE, args = "--preclean",
		path = "../repos/bin/windows/contrib/3.3/"
	)
} else {
	build(
		binary = TRUE, args = "--preclean",
		path = "../repos/bin/macosx/mavericks/contrib/3.3/"
	)
}
install()


#-------------------------------------------------------------------------------
#	Deploy
#-------------------------------------------------------------------------------
path.repos <- file.path(get.this.file.dir(), "../repos/")

tools::write_PACKAGES(
	file.path(path.repos, "src/contrib"), type = "source"
)
tools::write_PACKAGES(
	file.path(path.repos, "bin/windows/contrib/3.3/"), type = "win.binary"
)
tools::write_PACKAGES(
	file.path(path.repos, "bin/macosx/mavericks/contrib/3.3/"), type = "mac.binary"
)
