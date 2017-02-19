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
#	Get R version.
#------------------------------------------------------------------------------
r.ver <- paste(
	version$major, strsplit(version$minor, "\\.")[[1]][1], sep = "."
)


#------------------------------------------------------------------------------
#	Set path for Rtools.
#------------------------------------------------------------------------------
rtools <- c(
	"3.0" = ";C:/Rtools31/bin;C:/Rtools31/gcc-4.6.3/bin",
	"3.1" = ";C:/Rtools32/bin;C:/Rtools32/gcc-4.6.3/bin",
	"3.2" = ";C:/Rtools33/bin;C:/Rtools33/gcc-4.6.3/bin",
	"3.3" = ";C:/Rtools34/bin;C:/Rtools34/mingw_32/bin"
)

Sys.setenv(PATH = paste0(Sys.getenv("PATH"), rtools[r.ver]))


#------------------------------------------------------------------------------
#	Build documentation.
#------------------------------------------------------------------------------
roxygenize(clean = TRUE)
build_vignettes()


#------------------------------------------------------------------------------
#	Install partial.plot package before building vignettes.
#------------------------------------------------------------------------------
system("Rscript -e library(devtools);install()")


#------------------------------------------------------------------------------
#	Build package
#------------------------------------------------------------------------------
# Build source package
build(path = "../repos/src/contrib", vignettes = FALSE)

# Build binary package
if (version$os == "mingw32") {
	bin.path <- "../repos/bin/windows/contrib/%s"
} else {
	bin.path <- "../repos/bin/macosx/mavericks/contrib/%s"
}
bin.path <- normalizePath(sprintf(bin.path, r.ver))
if (!file.exists(bin.path)) {
	dir.create(bin.path)
}
build(binary = TRUE, args = "--preclean", path = bin.path, vignettes = FALSE)


#------------------------------------------------------------------------------
#	Deploy
#------------------------------------------------------------------------------
path.repos <- file.path(get.this.file.dir(), "../repos/")

tools::write_PACKAGES(
	file.path(path.repos, "src/contrib"), type = "source"
)
tools::write_PACKAGES(
	file.path(path.repos, sprintf("bin/windows/contrib/%s/", r.ver)),
	type = "win.binary"
)
tools::write_PACKAGES(
	file.path(path.repos, sprintf("bin/windows/contrib/%s/", r.ver)),
	type = "mac.binary"
)


#------------------------------------------------------------------------------
#	Restore working directory.
#------------------------------------------------------------------------------
setwd(old.wd)
rm(old.wd)



