#------------------------------------------------------------------------------
#'	(Internal) A reference class to draw partial relationship graph.
#'
#'	@field settings
#'		a \code{\link{pp.settings}} object to keep settings of the function.
#'
#'	@include pp.settings.r
#------------------------------------------------------------------------------
pp.drawer <- setRefClass(
	"pp.drawer",
	fields = list(settings = "pp.settings", vt = "matrix")
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	initialize = function(settings) {
		"
		Initialize object.
		"
		if (missing(settings)) {
			return()
		}
		initFields(settings = settings)
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	xy.values = function() {
		"
		Find possible x and y values.
		"
		x.name <- .self$settings$x.names.numeric[1]
		if (.self$settings$draw.relationship) {
			relationship <- .self$settings$relationship
			if (is.null(relationship$upper) | .self$settings$plot.type == "3D") {
				x <- relationship[[x.name]]
				y <- relationship$fit
			} else {
				x <- rep(relationship[[x.name]], 2)
				y <- c(relationship$upper, relationship$lower)
			}
		} else {
			x <- y <- numeric()
		}
		if (.self$settings$draw.residual & .self$settings$has.residual) {
			x <- c(x, .self$settings$data[[x.name]])
			y <- c(y, .self$settings$residual)
		}
		return(data.frame(x = x, y = y))
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	open.plot.window = function() {
		"
		Open new plot window.
		"
		# Open plot window.
		if (.self$settings$add) {
			return()
		}
		xy <- .self$xy.values()
		args <- list(
			xy$x, xy$y, type = "n", xlab = .self$settings$xlab,
			ylab = .self$settings$ylab
		)
		args <- c(args, .self$settings$other.pars)
		do.call(plot, args)
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw = function() {
		"
		Draw graph.
		"
		if (.self$settings$plot.type == "2D") {
			.self$open.plot.window()
			if (.self$settings$draw.hist) {
				.self$draw.hist()
			}
		}
		if (.self$settings$draw.interval & .self$settings$has.relationship) {
			.self$draw.interval()
		}
		if (
			.self$settings$draw.relationship & .self$settings$has.relationship
		) {
			.self$draw.relationship()
		}
		if (.self$settings$draw.residual & .self$settings$has.residual) {
			.self$draw.residual()
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.residual = function() {
		"
		Draw partial residuals.
		"
		if (.self$settings$plot.type == "2D") {
			.self$draw.residual.2d()
		} else {
			.self$draw.residual.3d()
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.residual.2d = function() {
		"
		Draw 2D residuals.
		"
		args <- list(
			x = .self$settings$data[[.self$settings$x.names.numeric]],
			y = .self$settings$residual
		)
		args <- c(args, .self$settings$parman$par.obs())
		args <- .self$settings$set.function.args(args, points)
		do.call(points, args)
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.residual.3d = function() {
		"
		Draw 3D residuals.
		"
		dat <- .self$settings$data
		x.names <- .self$settings$x.names.numeric
		fun.3d <- .self$settings$fun.3d
		resid <- .self$settings$residual
		if (identical(fun.3d, image)) {
			points(
				dat[[x.names[1]]], dat[[x.names[2]]], pch = 16, col = "white"
			)
		} else if (identical(fun.3d, persp)) {
			xy <- trans3d(
				dat[[x.names[1]]], dat[[x.names[2]]], resid, .self$vt
			)
			args <- list(x = xy$x, y = xy$y)
			args <- .self$settings$set.function.args(args, points)
			do.call(points, args)
		} else if (identical(fun.3d, rgl::persp3d)) {
			rgl::points3d(dat[[x.names[1]]], dat[[x.names[2]]], resid)
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.relationship = function() {
		"
		Draw partial relationships.
		"
		if (.self$settings$plot.type == "2D") {
			.self$draw.relationship.2d()
		} else {
			.self$draw.relationship.3d()
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.interval = function() {
		"
		Draw intervals of partial relationships.
		"
		if (.self$settings$plot.type == "2D") {
			.self$draw.interval.2d()
		} else {
			.self$draw.interval.3d()
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.relationship.2d = function() {
		for (i in .self$settings$unique.group) {
			"
			Draw partial relationship lines in 2D graph.
			"
			current.data <- .self$settings$relationship.split[[i]]
			args <- list(
				x = current.data[[.self$settings$x.names.numeric]],
				y = current.data$fit
			)
			args <- c(args, .self$settings$parman$par.group(i))
			args <- .self$settings$set.function.args(args, graphics::lines)
			do.call(graphics::lines, args)
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.interval.2d = function() {
		"
		Draw intervals of partial.relationship in 2D graph.
		"
		for (i in .self$settings$unique.group) {
			current.data <- .self$settings$relationship.split[[i]]
			x <- current.data[[.self$settings$x.names.numeric]]
			x <- c(x, rev(x))
			y <- c(current.data$lower, rev(current.data$upper))
			polygon(
				x, y, border = NA,
				col = trans.color(.self$settings$parman$par.group(i)$col)
			)
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.relationship.3d = function() {
		"
		Draw 3D partial relationship.
		"
		relationship <- .self$settings$relationship
		x.names <- .self$settings$x.names
		z.matrix <- matrix(
			.self$settings$relationship$fit, nrow = .self$settings$resolution
		)
		if (identical(.self$settings$fun.3d, image)) {
			col <- color.ramp(
				z.matrix, .self$settings$parman$col, unique.pal = TRUE
			)
		} else if (identical(.self$settings$fun.3d, persp)) {
			col <- .self$settings$parman$colors.for.persp(z.matrix)
		} else {
			col <- color.ramp(z.matrix, .self$settings$parman$col)
		}
		args <- list(
			z = z.matrix,
			x = unique(relationship[[x.names[1]]]),
			y = unique(relationship[[x.names[2]]]),
			xlab = .self$settings$xlab, ylab = .self$settings$ylab,
			zlab = .self$settings$zlab, col = col
		)
		args <- .self$settings$set.function.args(args)
		if (identical(.self$settings$fun.3d, persp) & is.null(args$zlim)) {
			args$zlim <- range(.self$xy.values()$y)
		}
		result <- do.call(.self$settings$fun.3d, args)
		if (identical(.self$settings$fun.3d, persp)) {
			.self$vt <- result
		}
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.interval.3d = function() {
		"
		Draw 3D interval of partial relationship (not implimented).
		"
		#####   DO NOTHING   #####
	}
)


#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.hist = function() {
		"
		Draw histogram of explanatory variable.
		"
		x.data <- .self$settings$data[[.self$settings$x.names.numeric]]
		h <- hist(x.data, plot = FALSE)
		h$counts <- h$counts / sum(h$counts)
		usr.hist <- old.usr <- par("usr")
		usr.hist[3:4] <- c(0, 1)
		par(usr = usr.hist)
		plot(
			h, col = "gray90", border = "gray90", ylim = c(0, 1), main = "",
			xaxt = "n", xlab = "", ylab = "", yaxt = "n", add = TRUE
		)
		par(usr = old.usr)
		box()
	}
)
