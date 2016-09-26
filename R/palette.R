# palette.R: functions for handling "box.palette" and related arguments.

# Predefined palettes.  The Color Brewer palettes were used as a
# starting point for these palettes, with manual interpolation to bring
# the number of colors per palette to nine.
# The darkest color in each palette is still fairly light
# so black text on it can be easily read.

Grays   <- gray(seq(1, .6, length.out=9))
Greys   <- Grays # british spelling
Greens  <- c("#F7FCF5", "#EEF8EA", "#E5F5E0", "#D6EFD0", "#C7E9C0", "#B4E1AD", "#A1D99B", "#8ACE88", "#74C476")
Blues   <- c("#F7FBFF", "#EAF3FB", "#DEEBF7", "#D2E3F3", "#C6DBEF", "#B2D2E8", "#9ECAE1", "#84BCDB", "#6BAED6")
Browns  <- hsv(h=0.06, s=seq(from=0, to=.6, length.out=9), v=seq(from=1, to=.85, length.out=9))
Oranges <- c("#FFF5EB", "#FEEDDC", "#FEE6CE", "#FDDBB8", "#FDD0A2", "#FDBF86", "#FDAE6B", "#FD9D53", "#FD8D3C")
Reds    <- c("#FFF5F0", "#FEEAE1", "#FEE0D2", "#FDCDB9", "#FCBBA1", "#FCA689", "#FC9272", "#FB7E5E", "#FB6A4A")
Purples <- c("#FFFBFF", "#FFF4FF", "#FFEDFF", "#FFE3FF", "#FFDAFF", "#FFCBFF", "#FFBDFF", "#FFABFF", "#FF9AFF")

Gy <- Grays # alternate names for the above palettes
Gn <- Greens
Bu <- Blues
Bn <- Browns
Or <- Oranges
Rd <- Reds
Pu <- Purples

RdYlGn  <- lighten(rainbow(100, end=.36), .2) # three color palettes
GnYlRd  <- rev(RdYlGn)

# two-color diverging palettes

ICOL <- seq(from=3, to=length(Blues), by=1) # start at 3 to avoid near-whites
RICOL <- rev(ICOL)

GyGy <- c(Gy[RICOL], Gy[ICOL])
GyGn <- c(Gy[RICOL], Gn[ICOL])
GyBu <- c(Gy[RICOL], Bu[ICOL])
GyBn <- c(Gy[RICOL], Bn[ICOL])
GyOr <- c(Gy[RICOL], Or[ICOL])
GyRd <- c(Gy[RICOL], Rd[ICOL])
GyPu <- c(Gy[RICOL], Pu[ICOL])

BuGy <- c(Bu[RICOL], Gy[ICOL])
BuGn <- c(Bu[RICOL], Gn[ICOL])
BuBu <- c(Bu[RICOL], Bu[ICOL])
BuBn <- c(Bu[RICOL], Bn[ICOL])
BuOr <- c(Bu[RICOL], Or[ICOL])
BuRd <- c(Bu[RICOL], Rd[ICOL])
BuPu <- c(Bu[RICOL], Pu[ICOL])

BnGy <- c(Bn[RICOL], Gy[ICOL])
BnGn <- c(Bn[RICOL], Gn[ICOL])
BnBu <- c(Bn[RICOL], Bu[ICOL])
BnBn <- c(Bn[RICOL], Bn[ICOL])
BnOr <- c(Bn[RICOL], Or[ICOL])
BnRd <- c(Bn[RICOL], Rd[ICOL])
BnPu <- c(Bn[RICOL], Pu[ICOL])

GnGy <- c(Gn[RICOL], Gy[ICOL])
GnGn <- c(Gn[RICOL], Gn[ICOL])
GnBu <- c(Gn[RICOL], Bu[ICOL])
GnBn <- c(Gn[RICOL], Bn[ICOL])
GnOr <- c(Gn[RICOL], Or[ICOL])
GnRd <- c(Gn[RICOL], Rd[ICOL])
GnPu <- c(Gn[RICOL], Pu[ICOL])

OrGy <- c(Or[RICOL], Gy[ICOL])
OrGn <- c(Or[RICOL], Gn[ICOL])
OrBu <- c(Or[RICOL], Bu[ICOL])
OrBn <- c(Or[RICOL], Bn[ICOL])
OrOr <- c(Or[RICOL], Or[ICOL])
OrRd <- c(Or[RICOL], Rd[ICOL])
OrPu <- c(Or[RICOL], Pu[ICOL])

PuGy <- c(Pu[RICOL], Gy[ICOL])
PuGn <- c(Pu[RICOL], Gn[ICOL])
PuBu <- c(Pu[RICOL], Bu[ICOL])
PuBn <- c(Pu[RICOL], Bn[ICOL])
PuOr <- c(Pu[RICOL], Or[ICOL])
PuRd <- c(Pu[RICOL], Rd[ICOL])
PuPu <- c(Pu[RICOL], Pu[ICOL])

RdGy <- c(Rd[RICOL], Gy[ICOL])
RdGn <- c(Rd[RICOL], Gn[ICOL])
RdBu <- c(Rd[RICOL], Bu[ICOL])
RdBn <- c(Rd[RICOL], Bn[ICOL])
RdOr <- c(Rd[RICOL], Or[ICOL])
RdRd <- c(Rd[RICOL], Rd[ICOL])
RdPu <- c(Rd[RICOL], Pu[ICOL])

remove(ICOL)
remove(RICOL)

predefined.palette.names <- c("AUTO",
    "Grays", "Greys", "Greens", "Blues", "Browns", "Oranges", "Reds", "Purples",
    "Gy", "Gn", "Bu", "Bn", "Or", "Rd", "Pu",
    "RdYlGn", "GnYlRd",
    "GyGy", "GyGn", "GyBu", "GyBn", "GyOr", "GyRd", "GyPu",
    "GnGy", "GnGn", "GnBu", "GnBn", "GnOr", "GnRd", "GnPu",
    "BuGy", "BuGn", "BuBu", "BuBn", "BuOr", "BuRd", "BuPu",
    "BnGy", "BnGn", "BnBu", "BnBn", "BnOr", "BnRd", "BnPu",
    "OrGy", "OrGn", "OrBu", "OrBn", "OrOr", "OrRd", "OrPu",
    "PuGy", "PuGn", "PuBu", "PuBn", "PuOr", "PuRd", "PuPu",
    "RdGy", "RdGn", "RdBu", "RdBn", "RdOr", "RdRd", "RdPu")

predefined.palettes.msg <- paste0(
    "Try something like box.palette=\"blue\" or box.palette=\"Blues\".\n",
    "The predefined palettes are (with an optional \"-\" prefix):\n",
    "    Grays Greys Greens Blues Browns Oranges Reds Purples\n",
    "    Gy Gn Bu Bn Or Rd Pu (alternative names for the above palettes)\n",
    "    BuGn BuBn GnRd etc.  (two-color diverging palettes: any combination of two palettes)\n",
    "    RdYlGn GnYlRd        (three color palettes)\n")

is.predefined.palette <- function(pal)
{
    is.character(pal) &&
    length(pal) == 1  &&
    nzchar(pal)       &&
    (grepl("[A-Z]", substr(pal, 1, 1), ignore.case=FALSE) || # upper case first letter
        substr(pal, 1, 1) == "-")                            # or first letter is "-"
}
# This can deal with the following examples:
#   pal = "auto"                    (becomes default.pal)
#   pal = "Reds"                    (becomes the vector of colors defined by Reds)
#   pal = c("tan1", "tan2", "tan3") (no change)
#   pal = c("-Reds", "Blues")
#   pal = c("tan3", "tan2", "tan1", "Blues")
#   pal = c("auto", "-Reds")

expand.palette <- function(pal, default.pal)
{
    argname <- deparse(substitute(pal))
    original.pal <- pal
    pal <- NULL
    for(i in seq_along(original.pal)) {
        pal_i <- original.pal[i]
        if(is.auto(pal_i)) # need upper case first letter for is.predefined.palette
            pal_i <- if(substr(pal_i, 1, 1) == "-") "-AUTO" else "AUTO"
        if(is.predefined.palette(pal_i)) {
            # pal_i is a predefined palette, does it begin with "-"?
            rev <- FALSE
            if(substr(pal_i, 1, 1) == "-") {
                rev <- TRUE
                pal_i = substring(pal_i, 2) # discard "-"
            }
            pal_i <- match.choices(pal_i, predefined.palette.names,
                                   argname="box.palette",
                                   err.msg=predefined.palettes.msg)
            if(is.auto(pal_i))
                pal_i <- default.pal
            pal_i <- getFromNamespace(pal_i, "rpart.plot")
            if(rev)
                pal_i <- rev(pal_i)
        }
        pal <- c(pal, pal_i)
    }
    # check that all elements of pal are in fact colors
    if(length(pal) == 0)
        stop0(argname, ": palette length is zero")
    for(i in seq_along(pal)) {
        pal_i <- pal[i]
        if(is.null(pal_i))
            stop0(argname,
                ": NULL is neither a color nor a palette.\n",
                predefined.palettes.msg)
        try <- try(col2rgb(pal_i))
        if(is.try.err(try))
            stop0(argname, ": ",
                if(is.character(pal_i[1])) quotify(pal_i)
                else pal_i,
                " is neither a color nor a palette.\n",
                predefined.palettes.msg)
    }
    pal
}
# TRUE if pal is a two-color diverging palette like GnBu.
#
# Not completely reliable, but works for the predefined
# palettes (except for BnOr and BnRd)
#
# Assume diverging if both the following are true:
#  1. the hues in the first half are quite different from the second half
#  2. the first and last hue in the first half are similar
# where "first half" means the colors in the first half of the pal vector.

is.diverging <- function(pal)
{
    if(length(pal) == 1)
        return(FALSE)
    if(length(pal) == 2) # two color palettes are always considered as diverging
        return(TRUE)
    hue <- rgb2hsv(col2rgb(pal))[1,] # extract hue
    n2 <- floor(length(hue) / 2)
    mean1 <- mean(hue[1:n2])
    mean2 <- mean(hue[(n2+1):length(hue)])
    # max(sd) below is necessary for grays where sd(hue)==0
    abs(mean1 - mean2) / max(.02, sd(hue)) > 1.5 && abs(hue[1] - hue[n2]) < .1
}
# Return an index vector with elements with values in the  range 1 to nquantiles.
# Each element indicates the quantile of the corresponding element in fitted.
# The returned index vector is intended to be used as in index into a color palette.

quantile.index <- function(fitted, pal.halflen)
{
    # length(fitted)==0 if called from get.col.from.diverging.palette
    # and no fitted > pal.thresh
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
# below pal.thresh take colors in the first half of palette; values above
# pal.thresh take colors in the second half of palette.
# For example, with palette=BuGn, fitted values below pal.thresh=.5
# will be blue and values above .5 will be green.

get.col.from.diverging.palette <- function(fitted, pal, trace, box.palette, pal.thresh)
{
    if(anyNA(fitted)) # TODO fix this
        stop0("Diverging palettes like box.palette=\"",
              if(box.palette == "auto" || box.palette == "-auto") "BuGn"
              else box.palette,
              "\" cannot be used for this model\n",
              "       because there are NAs in the fitted values.\n",
              "       Try something like box.palette=\"Blues\"")
    quantile.index1 <- quantile.index(fitted[fitted <= pal.thresh], length(pal)/2)
    quantile.index2 <- quantile.index(fitted[fitted >  pal.thresh], length(pal)/2)
    quantile.index <- rep(NA, times=length(fitted))
    quantile.index[fitted <= pal.thresh] <- quantile.index1
    quantile.index[fitted >  pal.thresh] <- quantile.index2 + length(pal) / 2
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
handle.anova.palette <- function(obj, box.palette, trace,
                                 fitted, pal.thresh,
                                 default.pal="Blues")
{
    original.pal <- box.palette
    pal <- expand.palette(box.palette, default.pal)
    diverging <- is.diverging(pal)
    if(trace >= 2)
        printf("handle.anova.palette\n")
    if(trace >= 1)
        printf("box.palette %s(%s): %s to %s\n",
               if(is.predefined.palette(original.pal))
                    sprintf("\"%s\" ", original.pal)
               else
                    "",
               if(diverging)
                    sprintf("pal.thresh %g", pal.thresh)
                else
                    "not diverging",
               describe.col(pal[1], show.hex=FALSE),
               describe.col(pal[length(pal)], show.hex=FALSE))
    if(trace >= 3) {
        printf("expanded box.palette:\n")
        print.palette(pal)
    }
    list(box.col=if(diverging)
                    get.col.from.diverging.palette(fitted,
                        pal, trace, box.palette, pal.thresh)
                 else
                    pal[quantile.index(fitted, length(pal))],
         box.palette=pal)
}
# multiclass response, or two class response with box.palette=list
handle.multiclass.palette <- function(obj, box.palette, trace, class.stats)
{
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
        if(trace >= 1)
            printf("multiclass auto box.palette: %s\n",
                switch(min(length(used.classes), 6),
                    "list(Greens)",                                        # 1
                    "list(Blues, Greens)",                                 # 2
                    "list(Reds, Grays, Greens)",                           # 3
                    "list(Reds, Oranges, Grays, Greens)",                  # 4
                    "list(Reds, Oranges, Grays, Blues, Greens)",           # 5
                    "list(Reds, Oranges, Purples, Grays, Blues, Greens)")) # 6 or more
    }
    if(length(box.palette) == 1) # allow the user to specify a single color or palette
        box.palette <- as.list(repl(box.palette, length(used.classes)))
    if(!is.list(box.palette))
        stop0(
"The rpart model has a multiclass response (not a continuous or binary response).\n",
"Therefore box.palette must be \"auto\", or a list of palettes, or a single color or palette.\n",
"e.g. box.palette=list(\"Reds\", \"Oranges\", \"Grays\", \"Blues\", \"Greens\")")
    for(i in 1:length(box.palette))
        box.palette[[i]] <- expand.palette(box.palette[[i]], "Undefined")
    if(trace >= 3) {
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
            return(list(box.col=get.bg()))
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
            box.col[i] <- pal[1 + highest.prob * (length(pal)-1)]
        }
    }
    list(box.col=box.col, box.palette=box.palette)
}
# Possibly add a legend (only for multi-level-response models)
# TODO For classes that are never predicted, include them in the
#      legend but with a color of white (right now we simply drop them)

possible.palette.legend <- function(rv, class.stats, box.col, box.palette,
                                    legend.x, legend.y, legend.cex)
{
    if(!is.specified(box.palette[1]) || all(box.palette == get.bg()))
        return()
    if(!is.null(legend.x[1]) && is.na(legend.x[1]))
        return()
    if(!is.null(legend.y[1]) && is.na(legend.y[1]))
        return()
    obj <- rv$obj
    if(obj$method != "class")
        return()
    if(class.stats$nlev <= 2)
        return()
    # get here if multi-level-response model with a list box.palette
    used.classes <- 1:class.stats$nlev # same as code in handle.multiclass.palette
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
get.palette.fitted <- function(default.fitted, node.labs, pal.node.fun, trace)
{
    fitted <- default.fitted
    if(pal.node.fun) {
        # convert warning to errors (because silent=TRUE doesn't work for warnings)
        old.warn <- getOption("warn")
        on.exit(options(warn=old.warn))
        options(warn=2)
        # extract first number in each label, if possible
        # two regexs separated by | below, first handles numbers with digits before
        # the point (if any), second handle numbers without digits before the point
        regex <- "-?[0-9]+\\.?[0-9]*e?-?[0-9]*|-?\\.[0-9]+e?-?[0-9]*"
        m <- regexpr(regex, node.labs)
        labs <- regmatches(node.labs, m)
        labs[m == -1] <- NA # m==-1 for entries in node.labs which don't match regex
        fitted <- try(as.numeric(labs), silent=TRUE)
        if(trace >= 2)
            cat("as.numeric(node.labs):", fitted, "\n")
        if(is.try.err(fitted) || anyNA(fitted)) {
            cat("\nnode labs generated by node.fun:\n")
            print(node.labs)
            cat("\n")
            stop0("Cannot convert node labs to numeric\n",
"To make this error go away, change node.fun or use pal.node.fun=FALSE")
        }
    }
    fitted
}
# like median but if median is largest or smallest value in x then adjust it
# (so e.g. for diverging palettes we use at least one divergent color)
smart.median <- function(x)
{
    median <- median(x, na.rm=TRUE)
    sort <- sort.unique(x)
    len <- length(sort)
    if(len >= 2) {
        if(median == sort[1])
            median <- (sort[1] + sort[2]) / 2
        else if(median == sort[len])
            median <- (sort[len] + sort[len-1]) / 2
    }
    median
}
# invoked if box.palette is specified (including when box.palette="auto")
handle.box.palette <- function(obj, box.palette, trace, class.stats, node.labs,
                               pal.thresh, # if not specified then auto
                               node.fun.name, pal.node.fun)
{
    get.pal.thresh <- function(default.pal.thresh)
    {
        check.numeric.scalar(pal.thresh, na.ok=TRUE, null.ok=TRUE)
        if(is.null(pal.thresh) || is.na(pal.thresh))
            default.pal.thresh
        else
            pal.thresh
    }
    #--- handle.box.palette starts here ---
    # return list(box.col, box.palette) where box.palette is the expanded box.palette
    # if statements below matches code in internal.node.labs() in node.labs.R
    pal.node.fun <-
        pal.node.fun && !identical(node.fun.name, "NULL")
    if(obj$method %in% c("poisson", "exp")) {
        fitted <- get.palette.fitted(obj$frame$yval2[,1], # rate
                                     node.labs, pal.node.fun, trace)
        handle.anova.palette(obj, box.palette, trace, fitted,
            get.pal.thresh(smart.median(fitted)))
    } else if(obj$method == "mrt") {
        fitted <- get.palette.fitted(apply(obj$frame$yval2, 1, which.max),
                                     node.labs, pal.node.fun, trace)
        handle.anova.palette(obj, box.palette, trace, fitted,
            get.pal.thresh(smart.median(fitted)))
    } else if(obj$method == "anova" || is.numeric.response(obj)) {
        fitted <- get.palette.fitted(obj$frame$yval,
                                     node.labs, pal.node.fun, trace)
        handle.anova.palette(obj, box.palette, trace, fitted,
            get.pal.thresh(smart.median(fitted)))
    } else if(obj$method == "class" || is.class.response(obj)) {
        if(class.stats$nlev != 2 || is.list(box.palette)) {
            if(trace >= 2)
                printf("handle.box.palette for class response invokes handle.multiclass.palette\n")
            # multiclass response, or two class response with box.palette=list
            # (here we ignore pal.thresh and pal.node.fun)
            handle.multiclass.palette(obj, box.palette, trace, class.stats)
        } else {
            # binomial model (two class response)
            # (note that we use BuGn although rattle::fancyRpartPlot uses GnBu
            # because we want to be compatible with 2-used-class multiclass model,
            # see handle.multiclass.palette)
            fitted <- get.palette.fitted(class.stats$fitted,
                                         node.labs, pal.node.fun, trace)
            pal.thresh <-
                get.pal.thresh(
                    if(pal.node.fun) smart.median(fitted) else .5)
            if(trace >= 2)
                printf("handle.box.palette for class response invokes ")
            handle.anova.palette(obj, box.palette, trace,
                              fitted=get.palette.fitted(class.stats$prob.per.lev[,2],
                                                node.labs, pal.node.fun, trace),
                              pal.thresh, default.pal="BuGn")
        }
    } else # unrecognized response, can probably never get here
        list(box.col=get.bg(), box.palette=get.bg())
}
# handle the box.col and box.palette arguments possibly specified by the user
handle.box.palette.args <- function(obj, trace, box.col, box.palette,
    pal.thresh, # if not specified then auto
    pal.node.fun,
    node.fun.name, class.stats, node.labs)
{
    retval <- list(box.col=box.col, box.palette=box.palette)

    bg <- get.bg() # never returns NA or 0

    # ignore box.palette if box.col is specified
    if(is.specified(box.col) || !is.specified(box.palette))
        retval$box.palette <- bg
    else # convert box.palette to box.col and expand box.palette
        retval <- handle.box.palette(obj, box.palette, trace, class.stats, node.labs,
                    pal.thresh, node.fun.name, pal.node.fun)

    # convert zeroes in box.col to bg, but retain NAs
    is.na.box.col <- is.na(box.col)
    retval$box.col <- set.zero.to.bg(retval$box.col, bg)
    retval$box.col[is.na.box.col] <- NA

    retval # return list(box.col, box.palette)
}
