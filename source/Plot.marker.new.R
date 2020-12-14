function (x, y, grad, mark, expand = 10, marker.size, col = 1, 
	offset.m = 0.25, side.label = "left", pos.m = NULL) 
{
	rad <- offset.m
	usr <- par("usr")
	uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
	mm <- 1/(uin[1] * 25.4)
	d <- (expand + 2) * mm
	b <- d * sqrt(1/(1 + grad * grad))
	a <- b * grad
	if (is.infinite(grad)) {
		if (sign(grad) > 0) {
			text(x = x, y = rad * sin(atan(grad)) + y + d, labels = mark, 
				cex = marker.size, pos = pos.m, offset = offset.m, 
				col = col, adj = c(0.5, 0.5))
		}
		else {
			text(x = x, y = rad * sin(atan(grad)) + y - d, labels = mark, 
				cex = marker.size, pos = pos.m, offset = offset.m, 
				col = col, adj = c(0.5, 0.5))
		}
	}
	else {
		if (side.label == "left") {
			text(-rad * cos(atan(grad)) + x - b, -rad * sin(atan(grad)) + 
				y - a, labels = mark, offset = NULL, adj = c(0.5, 
				0.5), pos = NULL, cex = marker.size, col = col)
		}
		if (side.label == "right") {
			text(rad * cos(atan(grad)) + x + b, rad * sin(atan(grad)) + 
				y + a, labels = mark, offset = NULL, adj = c(0.5, 
				0.5), pos = NULL, cex = marker.size, col = col)
		}
	}
}
