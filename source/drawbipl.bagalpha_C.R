function (Z, z.axes, z.axes.names = NULL, p, ax = NULL, ax.col, 
    ax.name.col, ax.name.size, alpha = 0.5, basket.beta = 0.5, 
    basket.n = 36, pch.samples.size = 1, c.hull.n = 10, class.vec, 
    conf.alpha = NULL, conf.type = conf.type, ellipse.alpha = NULL, 
    ellipse.kappa = NULL, exp.factor = 1.2, label = TRUE, label.size, 
    large.scale = FALSE, line.width = 1, markers = TRUE, marker.size, 
    max.num, means.plot = FALSE, offset = rep(0.5, 4), offset.m, 
    pch.means = 15, pch.means.size = 1, pos, pos.m, predictions.mean = NULL, 
    predictions.sample = NULL, ort.lty = 1, side.label, specify.bags, 
    specify.beta.baskets, specify.classes, specify.ellipses = NULL, 
    strepie = c(1, 1), Title = NULL, Tukey.median = TRUE, Z.means.mat, lblplot,
    ...) 
{
     bagcords <- list()
	if (!is.null(Z.means.mat) & ncol(Z.means.mat) == 5) 
        Z.means.mat <- data.frame(Z.means.mat[, 1:5], pch.means.size = pch.means.size, 
            stringsAsFactors = FALSE)
    .axes.plot <- function(Z, Z.means.mat, z.axes, z.axes.names, 
        p, ax, ax.col, ax.name.col, ax.name.size, markers, usr, 
        label, label.size, marker.size, offset, offset.m, pos, 
        pos.m, side.label, strepie, predictions.sample, predictions.mean, 
        ort.lty) {#print(z.axes)
        if (is.null(ax)) 
            axes <- NULL
        else axes <- (1:p)[ax]
        if (!is.null(predictions.sample)) {
            predictions <- data.frame(matrix(NA, nrow = length(axes), 
                ncol = length(predictions.sample)))
            dimnames(predictions) <- list(1:nrow(predictions), 
                paste("s", predictions.sample, sep = ""))
        }
        if (!is.null(predictions.mean)) {
            predictions <- data.frame(matrix(NA, nrow = length(axes), 
                ncol = length(predictions.mean)))
            dimnames(predictions) <- list(1:nrow(predictions), 
                paste("m", predictions.mean, sep = ""))
        }
        r.names <- NULL
        for (i in axes) {
            marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 
                1:3]
            x.vals <- marker.mat[, 1]
            y.vals <- marker.mat[, 2]
            if (is.null(z.axes.names)) {
                axis.name <- paste("v", i, sep = "")
            }
            else {
                axis.name <- z.axes.names[i]
            }
            r.names[i] <- axis.name
            std.markers <- zapsmall(marker.mat[, 3])
            x.invals <- x.vals[x.vals < usr[2] & x.vals > usr[1] & 
                y.vals < usr[4] & y.vals > usr[3]]
            if (length(x.invals) < 2) {
                warning(paste("Less than 2 markers on axis ", 
                  i, ". Increase n.int."))
                marker.mat <- z.axes[[i]][, 1:3]
                x.vals <- marker.mat[, 1]
                y.vals <- marker.mat[, 2]
                std.markers <- zapsmall(marker.mat[, 3])
                x.invals <- x.vals[x.vals < usr[2] & x.vals > 
                  usr[1] & y.vals < usr[4] & y.vals > usr[3]]
                y.invals <- y.vals[x.vals < usr[2] & x.vals > 
                  usr[1] & y.vals < usr[4] & y.vals > usr[3]]
                tick.labels <- std.markers[x.vals < usr[2] & 
                  x.vals > usr[1] & y.vals < usr[4] & y.vals > 
                  usr[3]]
                Draw.line2(x.vals = x.invals, y.vals = y.invals, 
                  marker.vals = tick.labels, line.name = axis.name, 
                  ax.name.size = ax.name.size, axis.col = ax.col$ax.col[i], 
                  ax.name.col = ax.name.col[i], offset = offset, 
                  pos = pos)
                next
            }
            y.invals <- y.vals[x.vals < usr[2] & x.vals > usr[1] & 
                y.vals < usr[4] & y.vals > usr[3]]
            tick.labels <- std.markers[x.vals < usr[2] & x.vals > 
                usr[1] & y.vals < usr[4] & y.vals > usr[3]]
            uit <- Draw.line2(x.vals = x.invals, y.vals = y.invals, 
                marker.vals = tick.labels, line.name = axis.name, 
                ax.name.size = ax.name.size, axis.col = ax.col$ax.col[i], 
                ax.name.col = ax.name.col[i], offset = offset, 
                pos = pos)
            if (!is.null(predictions.sample)) {
                for (ss in predictions.sample) {
                  predictions[i, paste("s", ss, sep = "")] <- round(DrawOrthogline(x1 = x.invals[1], 
                    y1 = y.invals[1], x2 = x.invals[length(x.invals)], 
                    y2 = y.invals[length(y.invals)], val1 = tick.labels[1], 
                    val2 = tick.labels[length(x.invals)], px = Z[ss, 
                      1], py = Z[ss, 2], ort.lty = ort.lty), 
                    digits = 4)
                }
            }
            if (!is.null(predictions.mean)) {
                for (ss in predictions.mean) {
                  predictions[i, paste("m", ss, sep = "")] <- round(DrawOrthogline(x1 = x.invals[1], 
                    y1 = y.invals[1], x2 = x.invals[length(x.invals)], 
                    y2 = y.invals[length(y.invals)], val1 = tick.labels[1], 
                    val2 = tick.labels[length(x.invals)], px = Z.means.mat[ss, 
                      1], py = Z.means.mat[ss, 2], ort.lty = ort.lty), 
                    digits = 4)
                }
            }
            gradient <- uit$gradient
            for (j in 1:length(x.invals)) Draw.onecmline(x = x.invals[j], 
                y = y.invals[j], grad = -1/gradient, expand = strepie[1], 
                both.sides = TRUE, col = ax.col$tickmarker.col[i])
            if (markers == TRUE) {
                x.labvals <- x.invals
                y.labvals <- y.invals
            }
            else {
                x.labvals <- x.invals[c(1, length(x.invals))]
                y.labvals <- y.invals[c(1, length(y.invals))]
                tick.labels <- tick.labels[c(1, length(tick.labels))]
            }
            for (j in 1:length(x.labvals)) Plot.marker.new(x = x.labvals[j], 
                y = y.labvals[j], grad = -1/gradient, mark = tick.labels[j], 
                expand = strepie[1], marker.size = marker.size, 
                col = ax.col$marker.col[i], pos.m = NULL, offset.m = offset.m[i], 
                side.label = side.label[i])
        }
        if (is.null(predictions.sample) & is.null(predictions.mean)) 
            predictions <- NULL
        if (!is.null(predictions)) {
            predictions <- na.omit(predictions)
            predictions <- as.matrix(predictions)
            dimnames(predictions)[[1]] <- r.names[!is.na(r.names)]
        }
        return(predictions)
    }
    .samples.plot <- function(Z, pch.samples.size, pch.means, 
        Z.means.mat, specify.classes, usr, label, label.size, 
        class.vec,lblplot) {
        if (ncol(Z) == 2) 
            Z <- cbind(Z, pch.means)
        if (ncol(Z) == 3) 
            Z <- cbind(Z, 1)
        if (ncol(Z) == 4) 
            Z <- cbind(Z, 1)
        if (ncol(Z) == 5) 
            Z <- cbind(Z, pch.samples.size)
        Z.plot <- Z
        x.vals <- Z.plot[, 1]
        y.vals <- Z.plot[, 2]
        invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < 
            usr[4] & y.vals > usr[3]
        Z.plot <- Z.plot[invals, ]
        if (is.null(Z.means.mat)) 
            J <- 0
        else J <- nrow(Z.means.mat)
        classes <- unique(dimnames(Z.means.mat)[[1]])[specify.classes]
        legend.labs <- classes
        if (label == TRUE) 
          text(Z[, 1][lblplot], Z[, 2][lblplot] - 0.015 * (usr[4] - 
            usr[3]), labels = dimnames(Z)[[1]][lblplot], cex = label.size)
        for (j in classes) {
            Z.class <- Z[class.vec == j, , drop = FALSE]
            Z.class <- data.frame(Z.class[, 1:2], pch.samples.samp = Z.class[1, 
                3], colr = as.character(Z.class[1, 4]), lty = Z.class[1, 
                5], pch.samples.size = Z.class[1, 6], stringsAsFactors = FALSE)
            if (!is.null(specify.classes)) {
                for (i in 1:nrow(Z.class)) points(x = Z.class[i, 
                  1], y = Z.class[i, 2], pch = Z.class[i, 3], 
                  col = Z.class[i, 4], cex = Z.class[i, 6], bg = Z.class[i, 
                    4])
            }
        }
    }
    .bags.plot <- function(Z, Z.means.mat, specify.bags, class.vec, 
        usr, alpha1, max.num, line.width, Tukey.median, c.hull.n) {
        J <- nrow(Z.means.mat)
        classes <- unique(dimnames(Z.means.mat)[[1]])[specify.bags]
        legend.labs.bags <- classes
        for (j in classes) {
            Z.class <- Z[class.vec == j, ]
            chr <- Z.class[1, 3]
            colr <- as.character(Z.class[1, 4])
            lty <- Z.class[1, 5]
            flush.console()
            cat(paste("bag for class ", j, " with ", nrow(Z.class), 
                " samples in class ", j, sep = ""), "\n")
            if (nrow(Z.class) > max.num) 
                Z.class <- Z.class[sample(1:nrow(Z.class), max.num), 
                  ]
            x <- Z.class[, 1]
            y <- Z.class[, 2]
            if (is.vector(x) || (is.matrix(x) && !is.data.frame(x))) {
                if (!is.numeric(x)) 
                  stop(message = "x is not a numeric dataframe or vector.")
            }
            if ((!is.matrix(x) && !is.vector(x)) || is.data.frame(x)) {
                if ((!is.data.frame(x) && !is.numeric(x)) || 
                  (!all(sapply(x, data.class) == "numeric"))) 
                  stop(message = "x is not a numeric dataframe or vector.")
            }
            x <- as.matrix(x)
            if (dim(x)[2] != 1) 
                stop(message = "x is not a vector.")
            if (is.vector(y) || (is.matrix(y) && !is.data.frame(y))) {
                if (!is.numeric(y)) 
                  stop(message = "y is not a numeric dataframe or vector.")
            }
            if ((!is.matrix(y) && !is.vector(y)) || is.data.frame(y)) {
                if ((!is.data.frame(y) && !is.numeric(y)) || 
                  (!all(sapply(y, data.class) == "numeric"))) 
                  stop(message = "y is not a numeric dataframe or vector.")
            }
            y <- as.matrix(y)
            if (dim(y)[2] != 1) 
                stop(message = "y is not a vector.")
            if (nrow(x) != nrow(y)) 
                stop(message = "x and y should have the same length!")
            na.x <- !is.finite(x)
            na.y <- !is.finite(y)
            ok <- !(na.x | na.y)
            x <- x[ok, , drop = FALSE]
            y <- y[ok, , drop = FALSE]
            n <- nrow(x)
            if (length(x) == 0) 
                stop(message = "All observations have missing values")
            if (n == 1) 
                stop(message = "The sample size should be at least two!")
            dimny <- dimnames(y)[[1]]
            if (length(dimny) == 0) 
                dimny <- 1:n
            if (n < c.hull.n) {
                hull <- chull(x, y)
                points(x, y, pch = as.vector(chr)[1], col = as.vector(colr)[1])
                polygon(x[hull], y[hull], density = 0, col = as.vector(colr)[1], 
                  lwd = line.width, lty = as.vector(lty)[1])
                count <- legend.labs.bags == j
                legend.labs.bags[count] <- paste(legend.labs.bags[count], 
                  ". (C hull)", sep = "")
                warning(paste("Samples too few for alpha bag for class", 
                  j, ". Convex hull constructed with all sample points shown. "))
            }
            else {
                storage.mode(x) <- "double"
                storage.mode(y) <- "double"
                interpx <- rep(0, 2 * n)
                storage.mode(interpx) <- "double"
                interpy <- rep(0, 2 * n)
                storage.mode(interpy) <- "double"
                datatyp <- matrix(0, n, 3)
                storage.mode(datatyp) <- "double"
                datatyp2 <- matrix(0, n, 2)
                storage.mode(datatyp2) <- "double"
                pxpy <- matrix(0, n, 3)
                storage.mode(pxpy) <- "double"
                whisk <- 2
                #abagplot.uit <- .Fortran("abagplot", as.integer(n), 
                #  as.integer(alpha1), x, y, as.integer(whisk), 
                ##  tukm = double(2), interpx = interpx, interpy = interpy, 
                 # num = as.integer(0), datatyp = datatyp, indoutl = integer(n), 
                 # datatyp2 = datatyp2, pxpy = pxpy, boxpl = as.integer(0), 
                 # nointer = as.integer(0), PACKAGE = "UBbipl")
                #tukmedian <- abagplot.uit$tukm
                #x.vec <- abagplot.uit$interpx
                #y.vec <- abagplot.uit$interpy
                #if (all(x.vec == 0) & all(y.vec == 0)) 
                #  stop(message = " x and y both null vectors")
                #nie.nul <- !((x.vec == 0) & (y.vec == 0))
                #if (Tukey.median) {
                #  points(x = tukmedian[1], y = tukmedian[2], 
                #    pch = 10, cex = 0.9, col = as.vector(colr))
                #}
		
		
		tempout2a <<- cbind(x,y)
		tempout3a <<- alpha1


		bagcords[[j]] <- compute.bagplot_C(cbind(x,y), factor = 1,alph=as.integer(alpha1)/100)$hull.bag
		#polygon(bagcords_Hull[[3]],border = fillcolslines[3],lwd=1,lty=3)

		tempout1a <<- bagcords[[j]]


                polygon(bagcords[[j]] , density = 0, 
                  col = colr, lwd = line.width, lty = as.vector(lty))
		#    bagcords[[j]] <<- cbind(x.vec[nie.nul], y.vec[nie.nul])
            }
        }
        return(legend.labs.bags)
    }
    .kap.ellipse <- function(Z, Z.means.mat, specify.ellipses, 
        class.vec, line.width, ellipse.kappa, ellipse.alpha) {
        J <- nrow(Z.means.mat)
        classes <- unique(dimnames(Z.means.mat)[[1]])[specify.ellipses]
        legend.labs.ellipses <- classes
        for (j in classes) {
            Z.class <- Z[class.vec == j, ]
            chr <- Z.class[1, 3]
            colr <- as.character(Z.class[1, 4])
            lty <- Z.class[1, 5]
            flush.console()
            cat(paste("ellipse for class ", j, " with ", nrow(Z.class), 
                " samples in class ", j, sep = ""), "\n")
            x <- Z.class[, 1]
            y <- Z.class[, 2]
            Xmat <- cbind(x, y)
            if (!is.null(ellipse.kappa)) 
                ConCentrEllipse(Xmat, kappa = ellipse.kappa, 
                  col = colr, lty = lty, type = "l", lwd = line.width)
            if (!is.null(ellipse.alpha)) 
                ConCentrEllipse(Xmat, kappa = sqrt(qchisq(ellipse.alpha, 
                  2)), type = "l", col = colr, lty = lty, lwd = line.width)
        }
        return(legend.labs.ellipses)
    }
    .beta.basket.plot <- function(Z, Z.means.mat, specify.beta.baskets, 
        class.vec, line.width, basket.beta, colr, basket.n) {
        J <- nrow(Z.means.mat)
        classes <- unique(dimnames(Z.means.mat)[[1]])[specify.beta.baskets]
        legend.labs.baskets <- classes
        for (j in classes) {
            Z.class <- Z[class.vec == j, ]
            chr <- Z.class[1, 3]
            colr <- as.character(Z.class[1, 4])
            lty <- Z.class[1, 5]
            flush.console()
            cat(paste("basket for class ", j, " with ", nrow(Z.class), 
                " samples in class ", j, sep = ""), "\n")
            x <- Z.class[, 1]
            y <- Z.class[, 2]
            ScatterplotBaskets(x = x, y = y, n = basket.n, pp = basket.beta, 
                lty = lty, col = colr, lwd = line.width)
        }
        return(legend.labs.baskets)
    }
    if (alpha < 0 | alpha > 0.99) 
        stop(message = "alpha not to be negative or larger than 0.99")
    alpha.entered <- alpha
    alpha <- round(alpha, digits = 2)
    if (abs(alpha.entered - alpha) > 0) 
        cat("alpha has been rounded to ", alpha, "\n")
    p <- length(z.axes)
    alpha1 <- 100 * alpha
    if (means.plot == FALSE & is.null(specify.bags) & is.null(specify.classes) & 
        is.null(specify.ellipses)) 
        stop("You have specified nothing to be plotted")
    if (large.scale) {
        if (nrow(Z.means.mat) == 1) 
            stop("This option should only be chosen if there are at least two different classes")
        specify.bags <- NULL
        specify.classes <- NULL
        specify.ellipses <- NULL
        specify.beta.baskets <- NULL
        plot(Z.means.mat[, 1] * exp.factor, Z.means.mat[, 2] * 
            exp.factor, xlim = range(Z.means.mat[, 1] * exp.factor), 
            ylim = range(Z.means.mat[, 2] * exp.factor), xaxt = "n", 
            yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", 
            yaxs = "i", asp = 1, ...)
    }
    else plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 
        1] * exp.factor), ylim = range(Z[, 2] * exp.factor), 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", 
        xaxs = "i", yaxs = "i", asp = 1, ...)
    usr <- par("usr")
    legend.lab.bags <- NULL
    if (means.plot == TRUE & large.scale == TRUE) {
        if (!is.null(predictions.sample)) {
            predictions.sample <- NULL
            warning("Predictions for sample points only available when large.scale == FALSE \n")
        }
        for (i in 1:nrow(Z.means.mat)) {
            points(x = Z.means.mat[i, 1], y = Z.means.mat[i, 
                2], pch = Z.means.mat[i, 3], col = Z.means.mat[i, 
                4], cex = Z.means.mat[i, 6])
        }
        legend.labs <- unique(dimnames(Z.means.mat)[[1]])
        predictions <- .axes.plot(Z = Z, Z.means.mat = Z.means.mat, 
            z.axes = z.axes, z.axes.names = z.axes.names, p = p, 
            ax = ax, ax.name.size = ax.name.size, ax.col = ax.col, 
            ax.name.col = ax.name.col, markers = markers, usr = usr, 
            label = label, label.size = label.size, marker.size = marker.size, 
            offset = offset, offset.m = offset.m, pos = pos, 
            pos.m = pos.m, side.label = side.label, strepie = strepie, 
            predictions.sample = NULL, predictions.mean = predictions.mean, 
            ort.lty = ort.lty)
        warning("When means are plotted on a large scale graph no bags or samples are plotted\n")
    }
    if (means.plot == TRUE & large.scale == FALSE) {
        if (!is.null(predictions.mean)) {
            warning("Predictions for class means more suitable when large.scale == TRUE \n")
        }
        for (i in 1:nrow(Z.means.mat)) {
            points(x = Z.means.mat[i, 1], y = Z.means.mat[i, 
                2], pch = Z.means.mat[i, 3], col = Z.means.mat[i, 
                4], cex = Z.means.mat[i, 6])
        }
        legend.labs <- unique(dimnames(Z.means.mat)[[1]])
        predictions <- .axes.plot(Z = Z, Z.means.mat = Z.means.mat, 
            z.axes = z.axes, z.axes.names = z.axes.names, p = p, 
            ax = ax, ax.name.size = ax.name.size, ax.col = ax.col, 
            ax.name.col = ax.name.col, markers = markers, usr = usr, 
            label = label, label.size = label.size, marker.size = marker.size, 
            offset = offset, offset.m = offset.m, pos = pos, 
            pos.m = pos.m, side.label = side.label, strepie = strepie, 
            predictions.sample = predictions.sample, predictions.mean = predictions.mean, 
            ort.lty = ort.lty)
    }
    if (means.plot == FALSE & large.scale == FALSE) {
        if (!is.null(predictions.mean)) {
            warning("Predictions for class means only available when means.plot == TRUE \n")
            prediction.means <- NULL
        }
        legend.labs <- unique(dimnames(Z.means.mat)[[1]])
        predictions <- .axes.plot(Z = Z, Z.means.mat = Z.means.mat, 
            z.axes = z.axes, z.axes.names = z.axes.names, p = p, 
            ax = ax, ax.name.size = ax.name.size, ax.col = ax.col, 
            ax.name.col = ax.name.col, markers = markers, usr = usr, 
            label = label, label.size = label.size, marker.size = marker.size, 
            offset = offset, offset.m = offset.m, pos = pos, 
            pos.m = pos.m, side.label = side.label, strepie = strepie, 
            predictions.sample = predictions.sample, predictions.mean = NULL, 
            ort.lty = ort.lty)
    }
    if (!is.null(specify.classes)) 
        .samples.plot(Z = Z, pch.samples.size = pch.samples.size, 
            pch.means = pch.means, Z.means.mat = Z.means.mat, 
            specify.classes = specify.classes, usr = usr, label = label, 
            label.size = label.size, class.vec = class.vec, lblplot = lblplot)
    if (!is.null(specify.bags)) 
        legend.lab.bags <- .bags.plot(Z = Z, Z.means.mat = Z.means.mat, 
            specify.bags = specify.bags, usr = usr, class.vec = class.vec, 
            alpha1 = alpha1, max.num = max.num, line.width = line.width, 
            Tukey.median = Tukey.median, c.hull.n = c.hull.n)
    if (!is.null(specify.ellipses) & (!is.null(ellipse.kappa) | 
        !is.null(ellipse.alpha))) {
        legend.lab.ellipses <- .kap.ellipse(Z = Z, Z.means.mat = Z.means.mat, 
            specify.ellipses = specify.ellipses, class.vec = class.vec, 
            line.width = line.width, ellipse.kappa = ellipse.kappa, 
            ellipse.alpha = ellipse.alpha)
    }
    if (!is.null(specify.beta.baskets)) 
        legend.lab.beta.baskets <- .beta.basket.plot(Z = Z, Z.means.mat = Z.means.mat, 
            specify.beta.baskets = specify.beta.baskets, class.vec = class.vec, 
            basket.beta = basket.beta, basket.n = basket.n, line.width = line.width, 
            col = colr)
    if (!is.null(conf.alpha)) {
        J <- nrow(Z.means.mat)
        classes.cc <- rownames(Z.means.mat)
        tel <- 0
        for (j in classes.cc) {
            tel = tel + 1
            Z.class.cc <- Z[class.vec == j, ]
            n.class.cc <- nrow(Z.class.cc)
            x.cc <- Z.means.mat[tel, 1]
            y.cc <- Z.means.mat[tel, 2]
            if (conf.type == "both") {
                draw.circle(r = sqrt(qchisq(conf.alpha, 2))/(sqrt((nrow(Z) - 
                  J) * n.class.cc)), h1 = x.cc, h2 = y.cc, col = Z.class.cc[1, 
                  4], lwd = line.width)
                draw.circle(r = sqrt(qchisq(conf.alpha, 2))/(sqrt((nrow(Z) - 
                  J))), h1 = x.cc, h2 = y.cc, col = Z.class.cc[1, 
                  4], lwd = line.width)
            }
            if (conf.type == "with.n.factor") 
                draw.circle(r = sqrt(qchisq(conf.alpha, 2))/(sqrt((nrow(Z) - 
                  J) * n.class.cc)), h1 = x.cc, h2 = y.cc, col = Z.class.cc[1, 
                  4], lwd = line.width)
            if (conf.type == "without.n.factor") 
                draw.circle(r = sqrt(qchisq(conf.alpha, 2))/(sqrt((nrow(Z) - 
                  J))), h1 = x.cc, h2 = y.cc, col = Z.class.cc[1, 
                  4], lwd = line.width)
        }
    }
    title(main = (ifelse(is.null(Title), paste(alpha1, " % BAG PLOT", 
        sep = ""), Title)))
    assign("bagcords_W",bagcords,envir=globalenv()) 
	list(legend.lab.bags = legend.lab.bags, predictions = predictions,bagcords = bagcords)
}
