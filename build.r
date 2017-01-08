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
#	Install package before building vignettes.
#------------------------------------------------------------------------------
system("Rscript -e library(devtools);install()")


#------------------------------------------------------------------------------
#	Build documentation.
#------------------------------------------------------------------------------
roxygenize(clean = TRUE)

#clean_vignettes()
build_vignettes()

render("vignettes/partial.plot.j.rmd", encoding = "utf-8")
render("vignettes/partial.plot.rmd", encoding = "utf-8")


#------------------------------------------------------------------------------
#	Build package
#------------------------------------------------------------------------------
# Build source package
build(path = "../repos/src/contrib")

# Build binary package
if (version$os == "mingw32") {
	bin.path <- "../repos/bin/windows/contrib/%s/"
} else {
	bin.path = "../repos/bin/macosx/mavericks/contrib/%s/"
}
r.ver <- paste(version$major, strsplit(version$minor, "\\.")[[1]][1], sep = ".")
bin.path <- sprintf(bin.path, r.ver)
if (!dir.exists(bin.path)) {
	dir.create(bin.path)
}
build(binary = TRUE, args = "--preclean", path = bin.path)

install()


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



