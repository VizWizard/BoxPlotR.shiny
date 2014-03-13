# coef = range parameter passed to box plot function
# coef == 0 --> Spear
# coef < 0 --> Tukey
# coef >0 --> Altman percentiles


myboxplot.stats <- function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE){

    nna <- !is.na(x)
    n <- sum(nna)
    stats <- stats::fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    # define outliers
    if (coef == 0) # Spear 
        do.out <- FALSE
    else if (coef < 0) { # Tukey
        out <- if (!is.na(iqr)) {
            x < (stats[2L] - coef *(-1)* iqr) | x > (stats[4L] + coef *(-1) * iqr)
        }
        else !is.finite(x)
        if (any(out[nna], na.rm = TRUE)) 
        	# Define whiskers to span range of all data points that are not outliers
            stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
    } else { # Altman
    	  stats <- quantile(x, c(coef/100,.25,.5,.75,1-coef/100), na.rm = TRUE)
          out <- x < stats[1] | x > stats[5]
    }
    conf <- if (do.conf) 
        stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
    list(stats = stats, n = n, conf = conf, out = if (do.out) x[out & nna] else numeric())
}

boxplot <- function (x, ..., range = 1.5, width = NULL, varwidth = FALSE, 
    notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"), 
    col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, 
        outwex = 0.5), horizontal = FALSE, add = FALSE, at = NULL){
        
    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)) 
        attributes(args)$names != ""
    else rep_len(FALSE, length(args))
    groups <- if (is.list(x)) 
        x
    else args[!namedargs]
    if (0L == (n <- length(groups))) 
        stop("invalid first argument")
    if (length(class(groups))) 
        groups <- unclass(groups)
    if (!missing(names)) 
        attr(groups, "names") <- names
    else {
        if (is.null(attr(groups, "names"))) 
            attr(groups, "names") <- 1L:n
        names <- attr(groups, "names")
    }
    cls <- sapply(groups, function(x) class(x)[1L])
    cl <- if (all(cls == cls[1L])) 
        cls[1L]
    else NULL
    for (i in 1L:n) groups[i] <- list(myboxplot.stats(unclass(groups[[i]]), range))
    stats <- matrix(0, nrow = 5L, ncol = n)
    conf <- matrix(0, nrow = 2L, ncol = n)
    ng <- out <- group <- numeric(0L)
    ct <- 1
    for (i in groups) {
        stats[, ct] <- i$stats
        conf[, ct] <- i$conf
        ng <- c(ng, i$n)
        if ((lo <- length(i$out))) {
            out <- c(out, i$out)
            group <- c(group, rep.int(ct, lo))
        }
        ct <- ct + 1
    }
    if (length(cl) && cl != "numeric") 
        oldClass(stats) <- cl
    z <- list(stats = stats, n = ng, conf = conf, out = out, 
        group = group, names = names)
    if (plot) {
        if (is.null(pars$boxfill) && is.null(args$boxfill)) 
            pars$boxfill <- col
        do.call("bxp", c(list(z, notch = notch, width = width, 
            varwidth = varwidth, log = log, border = border, 
            pars = pars, outline = outline, horizontal = horizontal, 
            add = add, at = at), args[namedargs]))
        invisible(z)
    }
    else z
}

