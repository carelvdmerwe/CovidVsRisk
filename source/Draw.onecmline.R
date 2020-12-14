function (x, y, grad, expand = 10, both.sides = FALSE, col = 1) 
{
	usr <- par("usr")
	uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
	mm <- 1/(uin[1] * 25.4)
	d <- expand * mm
	b <- d * sqrt(1/(1 + grad * grad))
	a <- b * grad
	if (is.infinite(grad)) {
		if (sign(grad) > 0) 
			lines(c(x, x), c(y, y + d), col = col)
		else lines(c(x, x), c(y, y - d), col = col)
	}
	else {
		if (both.sides == FALSE) 
			lines(c(x, x + b), c(y, y + a), col = col)
		else lines(c(x - b, x + b), c(y - a, y + a), col = col)
	}
}
