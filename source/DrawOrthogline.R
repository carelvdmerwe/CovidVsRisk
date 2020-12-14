function (x1, y1, x2, y2, val1 = NULL, val2 = NULL, px, py, ort.lty) 
{
    if (length(ort.lty) == 1) 
        ort.lty <- c(ort.lty, "black")
    if (x1 - x2 == 0) 
        gradient <- Inf
    else {
        if (y1 - y2 == 0) 
            gradient <- 0
        else gradient <- (y1 - y2)/(x1 - x2)
    }
    if (gradient == 0) {
        x.star <- px
        y.star <- y1
        prediction <- val1 + (x.star - x1) * (val2 - val1)/(x2 - 
            x1)
    }
    else {
        if (gradient == Inf) {
            x.star <- x1
            y.star <- py
            prediction <- val1 + (y.star - y1) * (val2 - val1)/(y2 - 
                y1)
        }
        else {
            intcept <- y1 - gradient * x1
            intcept.star <- py + (1/gradient) * px
            x.star <- (intcept.star - intcept)/(gradient + 1/gradient)
            y.star <- intcept + gradient * x.star
            prediction <- val1 + (x.star - x1) * (val2 - val1)/(x2 - 
                x1)
        }
    }
    lines(x = c(px, x.star), y = c(py, y.star), lty = as.numeric(ort.lty[1]), 
        col = ort.lty[2])
    prediction
}
