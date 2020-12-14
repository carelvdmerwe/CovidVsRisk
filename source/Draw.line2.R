function (x.vals = NULL, y.vals = NULL, marker.vals = NULL, 
	line.name = NULL, offset = c(0.5, 0.5, 0.5, 0.5), pos = "Orthog", 
	axis.col = "black", ax.name.col = "black", ax.name.size = 0.65) 
{
	usr <- par("usr")
	test <- rep(NA, 4)
	test[1] <- min(x.vals) < usr[1]
	test[2] <- max(x.vals) > usr[2]
	test[3] <- min(y.vals) < usr[3]
	test[4] <- max(y.vals) > usr[4]
	if (any(test)) 
		stop("Points not entirely in graph")
	if (!is.element(pos[1], c("Hor", "Orthog", "Paral"))) 
		stop("Argument pos must be one of 'Hor','Orthog' or 'Paral' ")
	if (pos[1] == "Hor") {
		par(las = 1)
		adjust <- c(0.5, 1, 0.5, 0)
	}
	if (pos[1] == "Orthog") {
		par(las = 2)
		adjust <- c(0, 0, 0, 0)
	}
	if (pos[1] == "Paral") {
		par(las = 0)
		adjust <- c(0.5, 0.5, 0.5, 0.5)
	}
	marker.mat <- cbind(x.vals, y.vals, marker.vals)
	marker.mat <- marker.mat[order(marker.mat[, 1]), ]
	x.vals <- marker.mat[, 1]
	y.vals <- marker.mat[, 2]
	if (!is.null(marker.vals)) 
		marker.vals <- marker.mat[, 3]
	if (y.vals[1] == y.vals[length(y.vals)] & x.vals[1] == x.vals[length(x.vals)]) 
		type.line <- "plot.5"
	if (y.vals[1] == y.vals[length(y.vals)] & x.vals[1] != x.vals[length(x.vals)]) {
		gradient <- 0
		intercept <- y.vals[1]
		type.line <- "plot.1"
	}
	if (y.vals[1] != y.vals[length(y.vals)] & x.vals[1] == x.vals[length(x.vals)]) {
		gradient <- Inf
		intercept <- x.vals[1]
		type.line <- "plot.2"
	}
	if (y.vals[1] != y.vals[length(y.vals)] & x.vals[1] != x.vals[length(x.vals)]) {
		gradient <- (y.vals[1] - y.vals[length(y.vals)])/(x.vals[1] - 
			x.vals[length(x.vals)])
		intercept <- y.vals[1] - gradient * x.vals[1]
		y1.ster <- gradient * usr[1] + intercept
		y2.ster <- gradient * usr[2] + intercept
		x1.ster <- (usr[3] - intercept)/gradient
		x2.ster <- (usr[4] - intercept)/gradient
		if (gradient > 0) 
			type.line <- "plot.3"
		if (gradient < 0) 
			type.line <- "plot.4"
	}
	plot.1 <- function(marker.mat, line.name = NULL, offset = NULL, 
		adjust = NULL, axis.col = "black", ax.name.col = "black", 
		ax.name.size = 0.65) {
		abline(h = marker.mat[1, 2], col = axis.col)
		if (!is.null(line.name)) {
			if (ncol(marker.mat) == 3) {
				if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
					3] < 0) 
					mtext(text = line.name, side = 4, line = 0 + 
						offset[4], adj = adjust[4], at = marker.mat[1, 
						2], col = ax.name.col, cex = ax.name.size)
				else mtext(text = line.name, side = 2, line = 0 + 
					offset[2], adj = adjust[2], at = marker.mat[1, 
					2], col = ax.name.col, cex = ax.name.size)
			}
		}
	}
	plot.2 <- function(marker.mat, line.name = NULL, offset = NULL, 
		adjust = NULL, axis.col = "black", ax.name.col = "black", 
		ax.name.size = 0.65) {
		abline(v = marker.mat[1, 1], col = axis.col)
		if (!is.null(line.name)) {
			if (ncol(marker.mat) == 3) {
				test <- order(marker.mat[, 2])[c(1, nrow(marker.mat))]
				if (marker.mat[test[2], 3] - marker.mat[test[1], 
					3] > 0) 
					mtext(text = line.name, side = 3, line = 0 + 
						offset[3], adj = adjust[3], at = marker.mat[1, 
						1], col = ax.name.col, cex = ax.name.size)
				else mtext(text = line.name, side = 1, line = 0 + 
					offset[1], adj = adjust[1], at = marker.mat[1, 
					1], col = ax.name.col, cex = ax.name.size)
			}
		}
	}
	plot.3 <- function(y1.ster, y2.ster, x1.ster, x2.ster, usr, 
		marker.mat, line.name = NULL, offset = NULL, adjust = NULL, 
		axis.col = "black", ax.name.col = "black", ax.name.size = 0.65) {
		if (y1.ster >= usr[3] & y2.ster >= usr[4]) {
			lines(c(usr[1], x2.ster), c(y1.ster, usr[4]), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) 
						mtext(text = line.name, side = 2, line = 0 + 
							offset[2], adj = adjust[2], at = y1.ster, 
							col = ax.name.col, cex = ax.name.size)
					else mtext(text = line.name, side = 3, line = 0 + 
						offset[3], adj = adjust[3], at = x2.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
		if (y1.ster > usr[3] & y2.ster < usr[4]) {
			lines(c(usr[1], usr[2]), c(y1.ster, y2.ster), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) 
						mtext(text = line.name, side = 2, line = 0 + 
							offset[2], adj = adjust[2], at = y1.ster, 
							col = ax.name.col, cex = ax.name.size)
					else mtext(text = line.name, side = 4, line = 0 + 
						offset[4], adj = adjust[4], at = y2.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
		if (y1.ster < usr[3] & y2.ster > usr[4]) {
			lines(c(x1.ster, x2.ster), c(usr[3], usr[4]), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) 
						mtext(text = line.name, side = 1, line = 0 + 
							offset[1], adj = adjust[1], at = x1.ster, 
							col = ax.name.col, cex = ax.name.size)
					else mtext(text = line.name, side = 3, line = 0 + 
						offset[3], adj = adjust[3], at = x2.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
		if (y1.ster < usr[3] & y2.ster < usr[4]) {
			lines(c(x1.ster, usr[2]), c(usr[3], y2.ster), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) 
						mtext(text = line.name, side = 1, line = 0 + 
							offset[1], adj = adjust[1], at = x1.ster, 
							col = ax.name.col, cex = ax.name.size)
					else mtext(text = line.name, side = 4, line = 0 + 
						offset[4], adj = adjust[4], at = y2.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
	}
	plot.4 <- function(y1.ster, y2.ster, x1.ster, x2.ster, usr, 
		marker.mat, line.name = NULL, offset = NULL, adjust = NULL, 
		axis.col = "black", ax.name.col = "black", ax.name.size = 0.65) {
		if (y1.ster > usr[4] & y2.ster > usr[3]) {
			lines(c(x2.ster, usr[2]), c(usr[4], y2.ster), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) 
						mtext(text = line.name, side = 3, line = 0 + 
							offset[3], adj = adjust[3], at = x2.ster, 
							col = ax.name.col, cex = ax.name.size)
					else mtext(text = line.name, side = 4, line = 0 + 
						offset[4], adj = adjust[4], at = y2.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
		if (y1.ster < usr[4] & y2.ster > usr[3]) {
			lines(c(usr[1], usr[2]), c(y1.ster, y2.ster), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) 
						mtext(text = line.name, side = 2, line = 0 + 
							offset[2], adj = adjust[2], at = y1.ster, 
							col = ax.name.col, cex = ax.name.size)
					else mtext(text = line.name, side = 4, line = 0 + 
						offset[4], adj = adjust[4], at = y2.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
		if (y1.ster > usr[4] & y2.ster < usr[3]) {
			lines(c(x2.ster, x1.ster), c(usr[4], usr[3]), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) {
						mtext(text = line.name, side = 3, line = 0 + 
							offset[3], adj = adjust[3], at = x2.ster, 
							col = ax.name.col, cex = ax.name.size)
					}
					else mtext(text = line.name, side = 1, line = 0 + 
						offset[1], adj = adjust[1], at = x1.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
		if (y1.ster < usr[4] & y2.ster < usr[3]) {
			lines(c(usr[1], x1.ster), c(y1.ster, usr[3]), col = axis.col)
			if (!is.null(line.name)) {
				if (ncol(marker.mat) == 3) {
					if (marker.mat[1, 3] - marker.mat[nrow(marker.mat), 
						3] > 0) 
						mtext(text = line.name, side = 2, line = 0 + 
							offset[2], adj = adjust[2], at = y1.ster, 
							col = ax.name.col, cex = ax.name.size)
					else mtext(text = line.name, side = 1, line = 0 + 
						offset[1], adj = adjust[1], at = x1.ster, 
						col = ax.name.col, cex = ax.name.size)
				}
			}
		}
	}
	plot.5 <- function(marker.mat, axis.col = 1) {
		abline(h = marker.mat[1, 2], col = axis.col)
		abline(v = marker.mat[1, 1], col = axis.col)
	}
	switch(type.line, plot.1 = plot.1(marker.mat = marker.mat, 
		line.name = line.name, offset = offset, adjust = adjust, 
		axis.col = axis.col, ax.name.col = ax.name.col, ax.name.size = ax.name.size), 
		plot.2 = plot.2(marker.mat = marker.mat, line.name = line.name, 
			offset = offset, adjust = adjust, axis.col = axis.col, 
			ax.name.col = ax.name.col, ax.name.size = ax.name.size), 
		plot.3 = plot.3(marker.mat = marker.mat, line.name = line.name, 
			offset = offset, y1.ster = y1.ster, y2.ster = y2.ster, 
			x1.ster = x1.ster, x2.ster = x2.ster, usr = usr, 
			adjust = adjust, axis.col = axis.col, ax.name.col = ax.name.col, 
			ax.name.size = ax.name.size), plot.4 = plot.4(marker.mat = marker.mat, 
			line.name = line.name, offset = offset, y1.ster = y1.ster, 
			y2.ster = y2.ster, x1.ster = x1.ster, x2.ster = x2.ster, 
			usr = usr, adjust = adjust, axis.col = axis.col, 
			ax.name.col = ax.name.col, ax.name.size = ax.name.size), 
		plot.5 = plot.5(marker.mat = marker.mat), axis.col = axis.col)
	list(gradient = gradient, intercept = intercept)
}
