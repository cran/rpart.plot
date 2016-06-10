# palette.R: functions for handling "box.palette" and default "extra" args

# Predefined palettes.  The Color Brewer palettes were used as a
# starting point for these palettes, with manual interpolation to bring
# the number of colors per palette to nine.
# TODO These were created in a rush and could be refined.

Blues   <- c("#F7FBFF", "#EAF3FB", "#DEEBF7", "#D2E3F3", "#C6DBEF", "#B2D2E8", "#9ECAE1", "#84BCDB", "#6BAED6")
Browns  <- c("#FEE391", "#FED370", "#FEC44F", "#FEAE3C", "#FE9929", "#F5841E", "#EC7014", "#DC5E0B", "#CC4C02")
Greens  <- c("#F7FCF5", "#EEF8EA", "#E5F5E0", "#D6EFD0", "#C7E9C0", "#B4E1AD", "#A1D99B", "#8ACE88", "#74C476")
Grays   <- gray(seq(1, .6, length.out=100))
Greys   <- Grays
Oranges <- c("#FFF5EB", "#FEEDDC", "#FEE6CE", "#FDDBB8", "#FDD0A2", "#FDBF86", "#FDAE6B", "#FD9D53", "#FD8D3C")
Reds    <- c("#FFF5F0", "#FEEAE1", "#FEE0D2", "#FDCDB9", "#FCBBA1", "#FCA689", "#FC9272", "#FB7E5E", "#FB6A4A")
# original purples are too close to grays
# Purples <- c("#FCFBFD", "#F5F4F9", "#EFEDF5", "#E4E3F0", "#DADAEB", "#CBCBE3", "#BCBDDC", "#ADABD2", "#9E9AC8")
Purples <- c("#FFFBFF", "#FFF4FF", "#FFEDFF", "#FFE3FF", "#FFDAFF", "#FFCBFF", "#FFBDFF", "#FFABFF", "#FF9AFF")

Bu <- Blues  # alternative names for the above palettes
Bn <- Browns
Gn <- Greens
Gy <- Grays
Or <- Oranges
Rd <- Reds
Pu <- Purples

RdYlGn  <- lighten(rainbow(100, end=.36), .2) # three color palettes
GnYlRd  <- rev(RdYlGn)

# "bifurcated" palettes
# TODO these could formed automatically rather than explicitly like below

ICOL    <- seq(from=4, to=length(Blues), by=1) # start at 4 so no near-whites

BuBu   <- c(Bu[rev(ICOL)], Bu[ICOL])
BuBn   <- c(Bu[rev(ICOL)], Bn[ICOL])
BuGn   <- c(Bu[rev(ICOL)], Gn[ICOL])
BuGy   <- c(Bu[rev(ICOL)], Gy[ICOL])
BuOr   <- c(Bu[rev(ICOL)], Or[ICOL])
BuRd   <- c(Bu[rev(ICOL)], Rd[ICOL])
BuPu   <- c(Bu[rev(ICOL)], Pu[ICOL])

BnBu   <- c(Bn[rev(ICOL)], Bu[ICOL])
BnBn   <- c(Bn[rev(ICOL)], Bn[ICOL])
BnGn   <- c(Bn[rev(ICOL)], Gn[ICOL])
BnGy   <- c(Bn[rev(ICOL)], Gy[ICOL])
BnOr   <- c(Bn[rev(ICOL)], Or[ICOL])
BnRd   <- c(Bn[rev(ICOL)], Rd[ICOL])
BnPu   <- c(Bn[rev(ICOL)], Pu[ICOL])

GnBu   <- c(Gn[rev(ICOL)], Bu[ICOL])
GnBn   <- c(Gn[rev(ICOL)], Bn[ICOL])
GnGy   <- c(Gn[rev(ICOL)], Gn[ICOL])
GnGy   <- c(Gn[rev(ICOL)], Gy[ICOL])
GnOr   <- c(Gn[rev(ICOL)], Or[ICOL])
GnRd   <- c(Gn[rev(ICOL)], Rd[ICOL])
GnPu   <- c(Gn[rev(ICOL)], Pu[ICOL])

GyBu   <- c(Gy[rev(ICOL)], Bu[ICOL])
GyBn   <- c(Gy[rev(ICOL)], Bn[ICOL])
GyGn   <- c(Gy[rev(ICOL)], Gn[ICOL])
GyGy   <- c(Gy[rev(ICOL)], Gy[ICOL])
GyOr   <- c(Gy[rev(ICOL)], Or[ICOL])
GyRd   <- c(Gy[rev(ICOL)], Rd[ICOL])
GyPu   <- c(Gy[rev(ICOL)], Gy[ICOL])

OrBu   <- c(Or[rev(ICOL)], Bu[ICOL])
OrBn   <- c(Or[rev(ICOL)], Bn[ICOL])
OrGn   <- c(Or[rev(ICOL)], Gn[ICOL])
OrGy   <- c(Or[rev(ICOL)], Gy[ICOL])
OrOr   <- c(Or[rev(ICOL)], Or[ICOL])
OrRd   <- c(Or[rev(ICOL)], Rd[ICOL])
OrPu   <- c(Or[rev(ICOL)], Pu[ICOL])

PuBu   <- c(Pu[rev(ICOL)], Bu[ICOL])
PuBn   <- c(Pu[rev(ICOL)], Bn[ICOL])
PuGn   <- c(Pu[rev(ICOL)], Gn[ICOL])
PuGy   <- c(Pu[rev(ICOL)], Gy[ICOL])
PuOr   <- c(Pu[rev(ICOL)], Or[ICOL])
PuRd   <- c(Pu[rev(ICOL)], Rd[ICOL])
PuPu   <- c(Pu[rev(ICOL)], Gy[ICOL])

RdBu   <- c(Rd[rev(ICOL)], Bu[ICOL])
RdBn   <- c(Rd[rev(ICOL)], Bn[ICOL])
RdGn   <- c(Rd[rev(ICOL)], Gn[ICOL])
RdGy   <- c(Rd[rev(ICOL)], Gy[ICOL])
RdOr   <- c(Rd[rev(ICOL)], Or[ICOL])
RdRd   <- c(Rd[rev(ICOL)], Rd[ICOL])
RdPu   <- c(Rd[rev(ICOL)], Pu[ICOL])

predefined.palette.names <- c("AUTO",
    "Blues", "Browns", "Grays", "Greys", "Greens", "Oranges", "Reds", "Purples",
    "Bu", "Bn", "Gn", "Gy", "Or", "Rd", "Pu",
    "RdYlGn", "GnYlRd",
    "BuBu", "BuBn", "BuGn", "BuGy", "BuOr", "BuRd", "BuPu",
    "BnBu", "BnBn", "BnGn", "BnGy", "BnOr", "BnRd", "BnPu",
    "GnBu", "GnBn", "GnGy", "GnGy", "GnOr", "GnRd", "GnPu",
    "GyBu", "GyBn", "GyGn", "GyGy", "GyOr", "GyRd", "GyPu",
    "OrBu", "OrBn", "OrGn", "OrGy", "OrOr", "OrRd", "OrPu",
    "PuBu", "PuBn", "PuGn", "PuGy", "PuOr", "PuRd", "PuPu",
    "RdBu", "RdBn", "RdGn", "RdGy", "RdOr", "RdRd", "RdPu")

predefined.palettes.msg <- paste0(
    "Try something like box.palette=\"blue\" or box.palette=\"Blues\".\n",
    "The predefined palettes are (with an optional \"-\" prefix):\n",
    "    Blues Browns Grays Greys Greens Oranges Reds Purples\n",
    "    Bu Bn Gn Gy Or Rd Pu (alternative names for the above palettes)\n",
    "    BuGn BuBn GnRd etc.  (two color palettes: any combination of two palettes)\n",
    "    RdYlGn GnYlRd        (three color palettes)\n")

convert.predefined.palette <- function(pal, default.pal)
{
    # is pal a predefined palette?
    if(is.character(pal) && length(pal) == 1 && nzchar(pal) &&
       (grepl("[A-Z]", substr(pal, 1, 1), ignore.case=FALSE) || # upper case?
        substr(pal, 1, 1) == "-")) {                            # prefix is "-"?
        # pal is a predefined palette, does it begin with "-"?
        rev <- FALSE
        if(substr(pal, 1, 1) == "-") {
            rev <- TRUE
            pal = substring(pal, 2) # discard "-"
        }
        pal <- match.choices(pal, predefined.palette.names,
                             err.msg=predefined.palettes.msg)
        if(is.auto(pal))
            pal <- default.pal
        pal <- getFromNamespace(pal, "rpart.plot")
        if(rev)
            pal <- rev(pal)
    }
    # check that all elements of pal are in fact colors
    argname <- deparse(substitute(pal))
    if(length(pal) == 0)
        stop0(argname, ": palette length is zero")
    for(i in 1:length(pal)) {
        n <- length(predefined.palette.names)
        if(is.null(pal[i]))
            stop0(argname,
                ": NULL is neither a color nor a palette.\n",
                predefined.palettes.msg)
        try <- try(col2rgb(pal[i]))
        if(is.try.err(try))
            stop0(argname, ": ",
                if(is.character(pal[i][1])) quotify(pal[i])
                else pal[i],
                " is neither a color nor a palette.\n",
                predefined.palettes.msg)
    }
    pal
}
# TRUE if pal is a "bifurcated" palette like GnBu.
# Not completely reliable, but works for the predefined palettes.
# Assume bifurcated if both the following are true:
#  1. the first set of hues is different from the second set of hues
#  2. the first and last hue in the first set are similar

is.bifurcated.palette <- function(pal)
{
    if(length(pal) == 1)
        return(FALSE)
    hue <- rgb2hsv(col2rgb(pal))[1,] # extract hue
    med <- median(hue)
    n2 <- floor(length(hue) / 2)
    mean1 <- mean(hue[1:n2])
    mean2 <- mean(hue[(n2+1):length(hue)])
    abs(mean1 - mean2) > .2 && abs(hue[1] - hue[n2]) < .1
}
# Return an index vector with elements with values in the  range 1 to nquantiles.
# Each element indicates the quantile of the corresponding element in fitted.
# The returned index vector is intended to be used as in index into a color palette.

quantile.index <- function(fitted, pal.halflen)
{
    # length(fitted)==0 if called from get.col.from.bifurcated.palette
    # and no fitted >  Threshold
    if(length(fitted) == 0 || pal.halflen == 1)
        return(1)
    fitted.nona <- fitted
    if(anyNA(fitted)) {
        warning0("NA in fitted values (fitted[", which(is.na(fitted))[1], "] is NA)")
        fitted.nona[is.na(fitted)] <- min(fitted, na.rm=TRUE)
    }
    q <- quantile(fitted.nona,      # e.g. pal.halflen=2 means q=c(0, .5, 1)
                  probs=seq(0, 1, length.out=pal.halflen+1), na.rm=TRUE)
    q <- q[2:length(q)]             # e.g. pal.halflen=2 means q is now c(.5, 1)
    quantile.index <- rep(NA, times=length(fitted.nona))
    # TODO could be vectorized?
    for(ifitted in 1:length(fitted.nona))
        for(iq in 1:length(q)) {
            if(fitted.nona[ifitted] <= q[iq]) {
                quantile.index[ifitted] <- iq
                break
            }
        }
    quantile.index[is.na(fitted)] <- NA
    quantile.index
}
# Returns box colors (like palette[quantile.index]) but values in fitted
# below Threshold take colors in the first part of palette; values above
# Threshold take colors in the second part of palette.
# For example, with palette=BuGn, fitted values below Threshold=.5
# will be blue and values above .5 will be green.

get.col.from.bifurcated.palette <- function(fitted, pal, trace, Threshold, box.palette)
{
    if(trace >= 2)
        printf("box.col threshold %g\n", Threshold)
    # TODO fix this
    if(anyNA(fitted))
        stop0("Bifurcated palettes like box.palette=\"",
              if(box.palette == "AUTO") "BuGn" else box.palette,
              "\" cannot be used for this model\n",
              "       because there are NAs in the fitted values.\n",
              "       Try something like box.palette=\"Blues\"")
    quantile.index1 <- quantile.index(fitted[fitted <= Threshold], length(pal)/2)
    quantile.index2 <- quantile.index(fitted[fitted >  Threshold], length(pal)/2)
    quantile.index <- rep(NA, times=length(fitted))
    quantile.index[fitted <= Threshold] <- quantile.index1
    quantile.index[fitted >  Threshold] <- quantile.index2 + length(pal) / 2
    quantile.index[is.na(fitted)] <- NA
    pal[quantile.index]
}
print.palette <- function(pal)
{
    if(length(pal) > 20)
        printf("   %d colors from %s   to   %s\n",
               length(pal), describe.col(pal[1]), describe.col(pal[length(pal)]))
    else for(i in 1:length(pal))
        printf("   %s\n", describe.col(pal[i]))
}
get.default.args.anova <- function(obj, extra, box.palette, trace, ...,
                                   Fitted,
                                   Threshold=median(Fitted, na.rm=TRUE),
                                   Default.pal="Blues")
{
    pal <- convert.predefined.palette(box.palette, Default.pal)
    if(trace >= 2) {
        printf("expanded box.palette:\n")
        print.palette(pal)
    }
    if(trace >= 2)
        printf("box.col: %s   to   %s\n",
               describe.col(pal[1]),
                            describe.col(pal[length(pal)]))
    list(extra=extra,
         box.col=if(is.bifurcated.palette(pal))
                    get.col.from.bifurcated.palette(Fitted,
                        pal, trace, Threshold, box.palette)
                 else
                    pal[quantile.index(Fitted, length(pal))],
         box.palette=pal)
}
# multiclass response, or two class response with box.palette=list
get.default.args.multiclass <- function(obj, box.palette, trace, class.stats, ...)
{
    if(identical(box.palette, 0))
        return(list(extra=104, box.col=0))
    must.reverse <- FALSE
    used.classes <- unique(sort(class.stats$fitted, na.last=TRUE))
    if(is.auto(box.palette)) {
        must.reverse <- substr(box.palette[1], 1, 1) == "-"
        # palette chosen below so if factor is ordered colors make sense
        box.palette <-
            switch(min(length(used.classes), 6),
                list(Greens),         # comat with anova model          # 1
                list(Blues, Greens),  # compat with binary model (BuGn) # 2
                list(Reds, Grays, Greens),                              # 3
                list(Reds, Oranges, Grays, Greens),                     # 4
                list(Reds, Oranges, Grays, Blues, Greens),              # 5
                list(Reds, Oranges, Purples, Grays, Blues, Greens))     # 6 or more
    }
    if(!is.list(box.palette))
        stop0(
"The rpart model has a multiclass response (not a continuous or binary response).\n",
"Therefore box.palette must be 0 or \"auto\" or a list of palettes.\n",
"e.g. box.palette=list(\"Reds\", \"Oranges\", \"Grays\", \"Blues\", \"Greens\")")
    for(i in 1:length(box.palette))
        box.palette[[i]] <- convert.predefined.palette(box.palette[[i]], "Undefined")
    if(trace >= 2) {
        printf("multiclass box.palette:\n")
        for(i in 1:length(box.palette)) {
            printf("box.palette[[%d]]:\n", i)
            print.palette(box.palette[[i]])
        }
    }
    warning.issued <- FALSE
    if(class.stats$nlev > length(box.palette)) {
        if(length(used.classes) > length(box.palette)) {
            if(trace >= 0) {
                warning.issued <- TRUE
                warning0(
"All boxes will be white (the box.palette argument will be ignored) because\n",
"the number of classes predicted by the model ", class.stats$nlev,
" is greater than length(box.palette) ",
length(box.palette), ".\n",
"To make this warning go away use box.palette=0 or trace=-1.")
            }
            return(list(extra=104, box.col=0))
        } else {
            if(trace >= 0) {
                NULL
#                 warning.issued <- TRUE
#                 warning0(
# "The box colors are not exactly those specified in box.palette.\n",
# "The box palettes have been reassigned because the number ",
# class.stats$nlev, "\n",
# "of response classes is greater than length(box.palette) ",
# length(box.palette), ".\n",
# "(However only 4 classes are predicted by the model, therefore there are enough\n",
# "palettes in box.palette for a unique color per class after reassigning the palettes.)\n",
# "To make this warning go away use box.palette=0 or trace=-1.")
            }
        }
    }
    box.palette <- box.palette[1:min(length(box.palette), length(used.classes))]
    if(must.reverse)
        box.palette <- rev(box.palette)
    if(trace >= 2 || warning.issued) {
        # show how colors are assigned to predicted classes
        ylevel <- attr(obj, "ylevel")
        ylevel <- if(is.null(ylevel)) as.character(ylevel) else ylevel
        max <- max(nchar(paste(ylevel[used.classes])))
        # sformat will be something like "box.col %-7.7s: %s\n"
        if(warning.issued)
            printf("\n")
        sformat <- sprintf("%sbox.col %%-%d.%ds: %%s\n",
            if(warning.issued) "  " else "", max, max)
        for(i in 1:length(used.classes))
            printf(sformat, paste(ylevel[used.classes[i]]),
                   describe.col(box.palette[[i]][length(box.palette[[i]])]))
    }
    match <- match(class.stats$fitted, used.classes)
    box.col <- repl(NA, length(class.stats$fitted))
    first.time <- TRUE
    max.prob.for.class <- apply(class.stats$prob.per.lev, 2, max)
    for(i in 1:length(box.col)) { # paranoia
        ilist <- match[i]
        if(ilist > length(box.palette)) {
            box.col[i] <- "white" # more used classes than elements in box.palette
            if(first.time && trace >= 0)
                warning0(
"Internal error: some boxes are colored white\n",
"because there are more than ", length(box.palette),
" predicted classes ",
"but length(box.palette) is ", length(box.palette), "\n",
"To make this warning go away use box.palette=0 or trace=-1")
            first.time <- FALSE
        } else {
            pal <- box.palette[[ilist]]
            highest.prob <- class.stats$prob.per.lev[i,class.stats$fitted[i]] /
                            max.prob.for.class[class.stats$fitted[i]]
            # highest prob is a fraction from 0 to 1
            # convert to an index from 1 to length(pal)
            box.col[i] <- pal[1 +
                                highest.prob * (length(pal)-1)]
        }
    }
    list(extra=104, box.col=box.col, box.palette=box.palette)
}
# Possibly add a legend (only for multi-level-response models)
# TODO For classes that are never predicted, include them in the
#      legend but with a color of white (right now we simply drop them)
possible.legend <- function(rv, class.stats, box.col, box.palette,
                            legend.x, legend.y, legend.cex)
{
    if(!is.specified(box.palette[1]))
        return()
    if(!is.null(legend.x[1]) && is.na(legend.x[1]))
        return()
    if(!is.null(legend.y[1]) && is.na(legend.y[1]))
        return()
    obj <- rv$obj
    if(obj$method != "class")
        return()
    class.stats <- get.class.stats(obj)
    if(class.stats$nlev <= 2)
        return()
    # get here if multi-level-response model with a box.palette
    used.classes <- 1:class.stats$nlev # same as code in get.default.args.multiclass
    if(class.stats$nlev > length(box.palette))
        used.classes <- unique(sort(class.stats$fitted, na.last=TRUE))
    last.elem <- function(x) x[length(x)]
    box.palette <- sapply(box.palette, last.elem)
    legend <- attr(obj, "ylevels")[used.classes]
    # TODO the automatic positioning of the legend is sometimes incorrect
    xedge <- rv$boxes$x1[1]
    if(is.na(xedge))
        xedge <- rv$branch.x[1,1]
    x <- legend.x
    if(is.null(x)) { # auto horizontal position?
        x <- if(rv$x[1] > .4) rv$xlim[1]                      # left side
             else rv$xlim[2] - .2 * (rv$xlim[2] - rv$xlim[1]) # right side
        if(1.4 * max(strwidth(legend)) > xedge) # not enough horizontal space?
            return()
    }
    y <- legend.y
    if(is.null(y)) # auto vertical position?
        y <- rv$boxes$y2[1]  + 1.5 * strheight("X")
    legend(x=x, y=y, legend=legend,
        col=0, xpd=NA, bty="n", cex=legend.cex * min(1.1 * rv$cex, 1),
        border=0,
        fill=box.palette[1:length(used.classes)])
}
get.default.args.class <- function(obj, box.palette, trace, class.stats, ...)
{
    if(class.stats$nlev == 2 && !is.list(box.palette)) {
        # binomial model (two class response)
        # (note that we use BuGn but rattle::fancyRpartPlot uses GnBu
        # because we want to be compatible with 2-used-class multiclass model,
        # see get.default.args.multiclass)
        get.default.args.anova(obj, extra=106, box.palette, trace, ...,
                               Fitted=class.stats$prob.per.lev[,2],
                               Threshold=.5, Default.pal="BuGn")
    } else # multiclass response, or two class response with box.palette=list
        get.default.args.multiclass(obj, box.palette, trace, class.stats, ...)
}
