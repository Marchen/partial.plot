#'	@include pp.settings.r
pp.colors <- setRefClass(
	"pp.colors",
	fields = list(settings = "pp.settings", group = "character")
)


pp.colors$methods(
	initialize = function(settings) {
		# Initialize fields.
		if (missing(settings)) {
			return()
		}
		initFields(settings = settings)
		# Make group depending on number of factor variables.
		if (!length(settings$x.names.factor) == 0) {
			.self$group <- combine.columns(
				settings$data[settings$x.names.factor], settings$sep
			)
		} else {
			.self$group <- rep("all", nrow(settings$data))
		}
	}
)


pp.colors$methods(
	colors.for.groups = function() {
		"
		Make color vector for each group.
		"
		result <- color.ramp(group, pal = settings$col, unique.pal = TRUE)
		return(result)
	}
)


pp.colors$methods(
	colors.for.observations = function() {
		"
		Make color vector for each observation.
		"
		result <- color.ramp(group, pal = settings$col, unique.pal = FALSE)
		return(result)
	}
)

