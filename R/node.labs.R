# node.labs.R: functions for generating labels

EX0                                 <- 0
EX1.NOBS                            <- 1
EX2.CLASS.RATE                      <- 2
EX3.MISCLASS.RATE                   <- 3
EX4.PROB.PER.CLASS                  <- 4
EX5.PROB.PER.CLASS.DONT             <- 5
EX6.PROB.2ND.CLASS                  <- 6
EX7.PROB.2ND.CLASS.DONT             <- 7
EX8.PROB.FITTED.CLASS               <- 8
EX9.PROB.ACROSS.ALL                 <- 9
EX10.PROB.ACROSS.ALL.2ND.CLASS      <- 10
EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT <- 11

extra.help <- function()
{
    cat0(
"\n",
"The 'extra' argument:\n",
"    0  No extra information\n",
"    1  Number of observations in the node\n",
"    2  Class models: Classification rate (ncorrect/nobservations)\n",
"       Poisson and exp models: number of events\n",
"    3  Class models: Misclassification rate\n",
"    4  Class models: Probability per class\n",
"    5  Class models: Like 4 but don't display the fitted class\n",
"    6  Class models: Probability of second class only\n",
"    7  Class models: Like 6 but don't display the fitted class\n",
"    8  Class models: Probability of the fitted class\n",
"    9  Class models: Probability relative to all observations\n",
"    10 Class models: like 9 but display the probability of the second class only\n",
"\n",
"    Add 100 to also display the percentage of observations in the node\n",
"\n")
}
is.vec <- function(x) {
    (NROW(x) == 1 || NCOL(x) == 1) && NROW(x) * NCOL(x) > 0
}
is.numeric.response <- function(obj) {
    # see if we have the fields necessary for
    # get.anova.labs (but not get.class.labs)
    is.vec(obj$frame$yval) && is.null(obj$frame$yval2)
}
is.class.response <- function(obj) {
    # check that we have the fields necessary for get.class.labs
    yval2 <- obj$frame$yval2
    NCOL(yval2) >= 5 &&
    colnames(yval2)[length(colnames(yval2))] == "nodeprob" &&
    is.vec(obj$frame$n) &&
    is.vec(obj$frame$wt)
}
is.multiclass.response <- function(obj) {
    is.class.response(obj) && NCOL(obj$frame$yval2) > 6
}
# call node.fun or obj$functions$text, and check its args and returned value
internal.node.labs <- function(x, node.fun, node.fun.name, type, extra,
                               under, xsep, digits, varlen,
                               prefix, suffix, class.stats, under.percent)
{
    stopifnot(is.numeric(extra) || is.logical(extra))
    stopifnot(length(extra) == 1)
    ex <- if(extra < 100) extra else extra - 100
    if(ex < 0 || floor(ex) != ex) {
        extra.help()
        stop0("extra=", extra, " is illegal")
    }
    stopifnot(extra >= 0)
    stopifnot(is.character(x$method) && length(x$method) == 1) # sanity check
    frame <- x$frame
    labs <-
        if(x$method == "anova")
            get.anova.labs(x, extra, under, digits, xsep, varlen, under.percent)
        else if(x$method == "class")
            get.class.labs(x, extra, under, digits, xsep, varlen,
                           class.stats, under.percent)
        else if(x$method == "poisson" || x$method == "exp")
            get.poisson.labs(x, extra, under, digits, xsep, varlen, under.percent)
        else if(x$method == "mrt")
            get.mvpart.labs(x, extra, under, digits, xsep, varlen)
        else {
            if(is.numeric.response(x)) {
                warning0("Unrecognized rpart object: treating as a numeric response model")
                if(x$method == "user")
                    x$method = "user.with.numeric.response" # used only in err msgs
                get.anova.labs(x, extra, under, digits, xsep, varlen, under.percent)
            } else if(is.class.response(x)) {
                warning0("Unrecognized rpart object: treating as a class response model")
                if(x$method == "user")
                    x$method = "user.with.class.response" # used only in err msgs
                get.class.labs(x, extra, under, digits, xsep, varlen,
                               class.stats, under.percent)
            } else {
                warning0("Unrecognized rpart object")
                check.func.args(x$functions$text, "x$functions$text",
                      function(yval, dev, wt, ylevel, digits, n, use.n) NA)
                labs <- x$functions$text(
                          yval=if(is.null(frame$yval2)) frame$yval else frame$yval2,
                          dev=frame$dev, wt=frame$wt, ylevel=attr(x, "ylevels"),
                          digits=abs(digits), n=frame$n, use.n=extra)
                check.returned.labs(x, labs, "x$functions$text()")
                if(under)
                    labs <- sub("\n", "\n\n", labs) # replace \n with \n\n
                labs
            }
        }
    if(!is.null(node.fun)) {
        # call user's node.fun
        node.fun <- check.func.args(node.fun, "node.fun",
                        function(x, labs, digits, varlen) NA)
        labs <- node.fun(x, labs, abs(digits), varlen)
        check.returned.labs(x, labs, node.fun.name)
    }
    labs <- paste0(prefix, labs, suffix)
    is.leaf <- is.leaf(frame)
    if(type == TYPE0.default || type == TYPE3.fancy.no.all)
        labs[!is.leaf] <- NA # no labels for internal nodes
    else if(type == TYPE5.varname.in.node) { # use split variable in interior nodes
        splits <- as.character(frame$var[!is.leaf])
        splits <- my.abbreviate(splits, varlen)
        labs[!is.leaf] <- splits
    }
    labs
}
get.anova.labs <- function(x, extra, under, digits, xsep, varlen, under.percent)
{
    frame <- x$frame
    fitted <- format0(frame$yval, digits)
    newline <- if(under) "\n\n" else "\n"
    ex <- if(extra < 100) extra else extra - 100
    labs <-
        if(ex == EX0)
            fitted
        else if(ex == EX1.NOBS) # add n?
            sprint("%s%sn=%s", fitted, newline, format0(frame$n, digits))
        else if (ex == EX2.CLASS.RATE) {
            extra.help()
            stop0("extra=", extra,
' is legal only for "class", "poisson" and "exp" models (you have an "anova" model)')
        }
        else if (ex > EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT) {
            extra.help()
            stop0("extra=", extra, " is illegal")
        } else { # ex >= EX3.MISCLASS.RATE && ex <= EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT
            extra.help()
            stop0("extra=", extra,
' is legal only for "class" models (you have an "anova" model)')
        }

    if(extra >= 100) {   # add percent?
        sep <- # space or newline before percent
            if(under.percent == 0) {
                "  "
            } else if(under.percent == 1) {
                if(under)
                    "\n\n"
                else
                    "\n"
            } else if(under.percent == 2) {
                if(extra == 100)
                    newline
                else
                    "  "
            }
        labs <- sprint("%s%s%s%%", labs, sep,
                        formatf(100 * frame$wt / frame$wt[1],
                                digits=max(0, abs(digits)-2)))
    }
    labs
}
get.class.stats <- function(x)
{
    # columns of yval2 for e.g. a two-level response are: fitted n1 n2 prob1 prob2
    yval2 <- x$frame$yval2
    if(NCOL(yval2) < 5)
        stop0("is.class.response(x) yet frame$yval2 is not ",
              "a matrix with five or more columns")
    fitted <- yval2[, 1] # fitted level as an integer
    if(NCOL(yval2) %% 2 == 0) { # new style yval2?
        stopifnot(colnames(yval2)[length(colnames(yval2))] == "nodeprob")
        nlev <- (ncol(yval2) - 2) / 2
    } else # old style yval2
        nlev <- (ncol(yval2) - 1) / 2
    stopifnot(nlev > 1)
    stopifnot(floor(nlev) == nlev)
    n.per.lev <- yval2[, 1 + (1:nlev), drop=FALSE]
    # aug 2012: commented out the following check because it
    # incorrectly fails when cases weights are used in the rpart model
    # stopifnot(sum(n.per.lev[1,]) == ntotal) # sanity check
    prob.per.lev <- yval2[, 1 + nlev + (1:nlev), drop=FALSE]
    # dec 2012: loosened following check to allow for numerical error
    stopifnot(abs(sum(prob.per.lev[1,]) - 1) < 1e-8) # sanity check
    list(yval2=yval2,
         fitted=fitted,
         nlev=nlev,
         # Aug 2019: ylevels is necessary for the multiclass model legend when
         # the last level in the response is unused in the training data and thus
         # does't appear in yval2 e.g. see "unusedlev" in the rpart.plot tests
         ylevels=attr(x, "ylevels"),
         n.per.lev=n.per.lev,
         prob.per.lev=prob.per.lev)
}
rescale.prob.across.all <- function(class.stats, scale, ntotal)
{
    scale <- matrix(rep(rowSums(class.stats$n.per.lev), each=class.stats$nlev),
                    ncol=class.stats$nlev, byrow=TRUE)
    class.stats$prob.per.lev * scale / ntotal
}
get.class.labs <- function(x, extra, under, digits, xsep, varlen,
                           class.stats, under.percent)
{
    frame <- x$frame
    n <- x$frame$n
    ntotal <- n[1]
    print.all.probs <- TRUE
    ex <- if(extra < 100) extra else extra - 100
    if(ex == EX2.CLASS.RATE || ex == EX3.MISCLASS.RATE) { # classification rate?
        if(is.null(xsep))
            xsep <- " / "
        ncorrect <- double(nrow(frame))
        # columns of yval2 for e.g. a two-level response are: fitted n1 n2 prob1 prob2
        for(i in 1:nrow(frame))
            ncorrect[i] <- class.stats$yval2[i, class.stats$yval2[i, 1] + 1]
    } else if(ex == EX6.PROB.2ND.CLASS || ex == EX7.PROB.2ND.CLASS.DONT) { # 2nd prob only?
        if(class.stats$nlev != 2)
            warning0("extra=", extra, " but the response has ",
                      class.stats$nlev, " levels (only the 2nd level is displayed)")
        class.stats$prob.per.lev <- class.stats$prob.per.lev[, 2]
        print.all.probs <- FALSE
    } else if(ex == EX8.PROB.FITTED.CLASS) { # prob of fitted class?
        temp <- double(nrow(class.stats$prob.per.lev))
        for(i in 1:nrow(class.stats$prob.per.lev))
            temp[i] <- class.stats$prob.per.lev[i, class.stats$fitted[i]]
        class.stats$prob.per.lev <- temp
        print.all.probs <- FALSE
    } else if(ex == EX9.PROB.ACROSS.ALL) { # probability across all nodes
        class.stats$prob.per.lev  <- rescale.prob.across.all(class.stats, scale, ntotal)
    } else if(ex == EX10.PROB.ACROSS.ALL.2ND.CLASS || # prob of 2nd class only, all nodes?
              ex == EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT) {
        if(class.stats$nlev != 2)
            warning0("extra=", extra, " but the response has ",
                      class.stats$nlev, " levels (only the 2nd level is displayed)")
        class.stats$prob.per.lev  <- rescale.prob.across.all(class.stats, scale, ntotal)
        class.stats$prob.per.lev <- class.stats$prob.per.lev[, 2]
        print.all.probs <- FALSE
    }
    if(is.null(xsep))
        xsep <- "  " # two spaces
    n.per.lev <- format0(class.stats$n.per.lev, digits)
    n.per.lev <- apply(matrix(n.per.lev, ncol=class.stats$nlev),
                       1, paste.with.breaks, collapse=xsep)
    prob.per.lev <- formatf(class.stats$prob.per.lev, abs(digits),
                            strip.leading.zeros=print.all.probs)

    if(print.all.probs)
        prob.per.lev <- apply(matrix(prob.per.lev, ncol=class.stats$nlev),
                              1, paste.with.breaks, collapse=xsep)

    ylevel <- attr(x, "ylevels")
    # fitted level as a string
    # (as.character below converts factor levels to character)
    sfitted <-
        if(is.null(ylevel)) as.character(class.stats$fitted)
        else ylevel[class.stats$fitted]
    sfitted <- my.abbreviate(sfitted, varlen)
    newline <- if(under) "\n\n" else "\n"

    labs <-
        if(ex == EX0)
            sfitted
        else if(ex == EX1.NOBS)
            paste0(sfitted, newline, n.per.lev)
        else if(ex == EX2.CLASS.RATE)
            paste0(sfitted, newline, ncorrect, xsep, n)
        else if(ex == EX3.MISCLASS.RATE)
            paste0(sfitted, newline, n - ncorrect, xsep, n)
        else if(ex == EX4.PROB.PER.CLASS    ||
                ex == EX6.PROB.2ND.CLASS    ||
                ex == EX8.PROB.FITTED.CLASS ||
                ex == EX9.PROB.ACROSS.ALL   ||
                ex == EX10.PROB.ACROSS.ALL.2ND.CLASS) {
            paste0(sfitted, newline, prob.per.lev)
        } else if(ex == EX5.PROB.PER.CLASS.DONT ||
                ex == EX7.PROB.2ND.CLASS.DONT ||
                ex == EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT)
            prob.per.lev
        else {
            extra.help()
            stop0("extra=", ex, " is illegal")
        }

    if(extra >= 100) { # add percent?
        sep <- switch(ex+1,  # figure out where to put percent (same line? below? etc.)
                      newline,                    # EX0 (may be a double newline)
                      "\n",                       # EX1.NOBS
                      "\n",                       # EX2.CLASS.RATE
                      "\n",                       # EX3.MISCLASS.RATE
                      "\n",                       # EX4.PROB.PER.CLASS
                      newline,                    # EX5.PROB.PER.CLASS.DONT
                      if(under) "  " else "\n",   # EX6.PROB.2ND.CLASS
                      if(under) "\n\n" else "\n", # EX7.PROB.2ND.CLASS.DONT
                      if(under) "  " else "\n",   # EX8.PROB.FITTED.CLASS
                      "\n",                       # EX9.PROB.ACROSS.ALL
                      if(under) "  " else "\n",   # EX10.PROB.ACROSS.ALL.2ND.CLASS
                      if(under) "\n\n" else "\n") # EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT

        sep <- # space or newline before percent
            if(under.percent == 0) {
                "  "
            } else if(under.percent == 1) {
                if(sep == "\n\n")
                    "\n\n"
                else
                    "\n"
            } else if(under.percent == 2) {
                sep
            }
        labs <- sprint("%s%s%s%%", labs, sep,
                        formatf(100 * frame$wt / frame$wt[1],
                                digits=max(0, abs(digits)-2)))
    }
    labs
}
# this function is also used for exp
get.poisson.labs <- function(x, extra, under, digits, xsep,
                             varlen, under.percent)
{
    frame <- x$frame
    rate  <- format0(frame$yval2[,1], digits)
    nbr <- format0(frame$yval2[,2], digits)
    newline <- if(under) "\n\n" else "\n"
    ex <- if(extra < 100) extra else extra - 100
    if(ex == EX0)
        labs <- rate
    else if(ex == EX1.NOBS) {      # add number of events and n?
        if(is.null(xsep))
            xsep <- " / "
        labs <- sprint("%s%s%s%s%s",
            rate, newline, nbr, xsep, format0(frame$n, digits))
    } else if(ex == EX2.CLASS.RATE) {    # add number of events?
        labs <- sprint("%s%s%s", rate, newline, nbr)
        newline <- "  "     # want percent, if any, on same line
    } else if (ex > EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT) {
        extra.help()
        stop0("extra=", extra, " is illegal")
    } else { # ex >= EX3.CLASS.RATE && ex <= EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT
        extra.help()
        stop0("extra=", extra,
' is legal only for "class" models (you have a \"", x$method, "\" model)')
    }
    if(extra >= 100) {        # add percent?
        sep <- # space or newline before percent
            if(under.percent == 0) {
                "  "
            } else if(under.percent == 1) {
                if(under)
                    "\n\n"
                else
                    "\n"
            } else if(under.percent == 2) {
                if(under)
                    "\n\n"
                else
                    "\n"
            }
        labs <- sprint("%s%s%s%%", labs, sep,
                        formatf(100 * frame$wt / frame$wt[1],
                                digits=max(0, abs(digits)-2)))
    }
    labs
}
print.node.labs.and.stop <- function(labs, fun.name, ...)
{
    cat("\nnode labs:\n")
    print(labs)
    cat("\n")
    stop0("the call to ", fun.name, " returned a bad result: ", ...)
}
# check returned labs because split.fun or node.fun may be user supplied
check.returned.labs <- function(x, labs, fun.name)
{
    if(length(labs) == 0)
        print.node.labs.and.stop(labs, fun.name, "length(labs) == 0")
    if(!is.character(labs)) {
        labs <- as.character(labs)
        if(anyNA(labs))
            print.node.labs.and.stop(labs, fun.name, "NA in labs")
    }
    if(length(labs) != nrow(x$frame))
        print.node.labs.and.stop(labs, fun.name, "\nthe number ", length(labs),
            " of returned labels is not equal to the number of rows in frame ",
            nrow(x$frame))
}
# similar to paste but insert \n as necessary to break up long lines
paste.with.breaks <- function(x, collapse)
{
    n.per.line <- 100
    len <- length(x)
    if(len >= 7) {
        # want approximately equal number of values in each line
        n.per.line <-
            if     (len == 11)     6    # 6 numbers per line
            else if(len == 9)      5
            else if(len == 7)      4
            else if(len %% 7 == 0) 7
            else if(len %% 6 == 0) 6
            else if(len %% 5 == 0) 5
            else if(len %% 4 == 0) 4
            else if(len %% 3 == 0) 6
            else if(len %% 2 == 0) 6
            else                   5
    }
    s <- ""
    for(i in 1:len)
        s <- if(i == len)
                paste0(s, x[i])
             else if(i %% n.per.line == 0)
                paste0(s, x[i], "\n")
             else
                paste0(s, x[i], collapse)
    s
}
