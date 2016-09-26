# node.labs.R: functions for generating labels

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

# call node.fun or obj$functions$text, and check its args and returned value
internal.node.labs <- function(x, node.fun, node.fun.name, type, extra,
                               under, xsep, digits, varlen, prefix, suffix, class.stats)
{
    stopifnot((is.numeric(extra) || (is.logical(extra)) && length(extra) == 1))
    frame <- x$frame
    labs <-
        if(x$method == "anova")
            get.anova.labs(x, extra, under, digits, xsep, varlen)
        else if(x$method == "class")
            get.class.labs(x, extra, under, digits, xsep, varlen, class.stats)
        else if(x$method == "poisson" || x$method == "exp")
            get.poisson.labs(x, extra, under, digits, xsep, varlen)
        else if(x$method == "mrt")
            get.mvpart.labs(x, extra, under, digits, xsep, varlen)
        else {
            if(is.numeric.response(x)) {
                warning0("Unrecognized rpart object: treating as a numeric response model")
                if(x$method == "user")
                    x$method = "user.with.numeric.response" # used only in err msgs
                get.anova.labs(x, extra, under, digits, xsep, varlen)
            } else if(is.class.response(x)) {
                warning0("Unrecognized rpart object: treating as a class response model")
                if(x$method == "user")
                    x$method = "user.with.class.response" # used only in err msgs
                get.class.labs(x, extra, under, digits, xsep, varlen, class.stats)
            } else {
                warning0("Unrecognized rpart object")
                check.func.args(x$functions$text, "x$functions$text",
                      function(yval, dev, wt, ylevel, digits, n, use.n) NA)
                labs <- x$functions$text(
                          yval=if(is.null(frame$yval2)) frame$yval else frame$yval2,
                          dev=frame$dev, wt=frame$wt, ylevel=attr(x, "ylevels"),
                          digits=digits, n=frame$n, use.n=extra)
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
        labs <- node.fun(x, labs, digits, varlen)
        check.returned.labs(x, labs, node.fun.name)
    }
    labs <- paste0(prefix, labs, suffix)
    if(type == TYPE.default || type == TYPE.fancy.noall)
        labs[!is.leaf(frame)] <- NA # no labels for internal nodes
    labs
}
get.anova.labs <- function(x, extra, under, digits, xsep, varlen)
{
    frame <- x$frame
    fitted <- format0(frame$yval, digits)
    newline <- if(under) "\n\n" else "\n"
    ex <- if(extra < 100) extra else extra - 100
    labs <-
        if(ex == 0)
            fitted
        else if(ex == 1) # add n?
            sprintf("%s%sn=%s", fitted, newline, format0(frame$n, digits))
        else
            stop0("extra=", extra, " is illegal (for method=\"", x$method, "\")")

    if(extra >= 100) {   # add percent?
        sep <- if(extra == 100) newline else "  "
        labs <- sprintf("%s%s%s%%", labs, sep,
                        formatf(100 * frame$wt / frame$wt[1], digits=max(0, digits-2)))
    }
    labs
}
get.class.stats <- function(obj)
{
    # columns of yval2 for e.g. a two-level response are: fitted n1 n2 prob1 prob2
    yval2 <- obj$frame$yval2
    if(NCOL(yval2) < 5)
        stop0("is.class.response(obj) yet frame$yval2 is not ",
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
         n.per.lev=n.per.lev,
         prob.per.lev=prob.per.lev)
}
get.class.labs <- function(obj, extra, under, digits, xsep, varlen, class.stats)
{
    frame <- obj$frame
    n <- obj$frame$n
    ntotal <- n[1]
    print.all.probs <- TRUE
    ex <- if(extra < 100) extra else extra - 100
    if(ex == 2 || ex == 3) {        # classification rate?
        if(is.null(xsep))
            xsep <- " / "
        ncorrect <- double(nrow(frame))
        # columns of yval2 for e.g. a two-level response are: fitted n1 n2 prob1 prob2
        for(i in 1:nrow(frame))
            ncorrect[i] <- class.stats$yval2[i, class.stats$yval2[i, 1] + 1]
    } else if(ex == 6 || ex == 7) { # 2nd prob only?
        if(class.stats$nlev != 2)
            warning0("extra=", extra, " but the response has ",
                      class.stats$nlev, " levels (only the 2nd level is displayed)")
        class.stats$prob.per.lev <- class.stats$prob.per.lev[, 2]
        print.all.probs <- FALSE
    } else if(ex == 8) {            # prob of fitted class?
        temp <- double(nrow(class.stats$prob.per.lev))
        for(i in 1:nrow(class.stats$prob.per.lev))
            temp[i] <- class.stats$prob.per.lev[i, class.stats$fitted[i]]
        class.stats$prob.per.lev <- temp
        print.all.probs <- FALSE
    } else if(ex == 9) {           # scale probs by proportion of obs in node?
        scale <- matrix(rep(rowSums(class.stats$n.per.lev), each=class.stats$nlev),
                        ncol=class.stats$nlev, byrow=TRUE)
        class.stats$prob.per.lev  <- class.stats$prob.per.lev * scale / ntotal
    }
    if(is.null(xsep))
        xsep <- "  " # two spaces
    n.per.lev <- format0(class.stats$n.per.lev, digits)
    n.per.lev <- apply(matrix(n.per.lev, ncol=class.stats$nlev),
                       1, paste.with.breaks, collapse=xsep)
    prob.per.lev <- formatf(class.stats$prob.per.lev, digits,
                            strip.leading.zeros=print.all.probs)
    if(print.all.probs)
        prob.per.lev <- apply(matrix(prob.per.lev, ncol=class.stats$nlev),
                              1, paste.with.breaks, collapse=xsep)

    ylevel <- attr(obj, "ylevel")
    # fitted level as a string
    sfitted <- if(is.null(ylevel)) as.character(class.stats$fitted)
               else ylevel[class.stats$fitted]
    sfitted <- my.abbreviate(sfitted, varlen)

    newline <- if(under) "\n\n" else "\n"

    labs <-
        if(ex == 0)
            sfitted
        else if(ex == 1)
            paste0(sfitted, newline, n.per.lev)
        else if(ex == 2)
            paste0(sfitted, newline, ncorrect, xsep, n)
        else if(ex == 3)
            paste0(sfitted, newline, n - ncorrect, xsep, n)
        else if(ex == 4 || ex == 6 || ex == 8 || ex == 9)
            paste0(sfitted, newline, prob.per.lev)
        else if(ex == 5 || ex == 7)
            prob.per.lev
        else
            stop0("extra=", extra, " is illegal (for method=\"", obj$method, "\")")

    if(extra >= 100) { # add percent?
        sep <- switch(ex+1,  # figure out where to put percent (same line? below? etc.)
                      newline,                    # 0 (may be a double newline)
                      "\n",                       # 1
                      "\n",                       # 2
                      "\n",                       # 3
                      "\n",                       # 4
                      newline,                    # 5
                      if(under) "  " else "\n",   # 7 # modified march 2016
                      if(under) "\n\n" else "\n", # 7
                      "  ",                       # 8
                      "\n")                       # 9

        labs <- sprintf("%s%s%s%%", labs, sep,
                        formatf(100 * frame$wt / frame$wt[1], digits=max(0, digits-2)))
    }
    labs
}
get.poisson.labs <- function(obj, extra, under, digits, xsep, varlen)
{
    frame <- obj$frame
    rate  <- format0(frame$yval2[,1], digits)
    nbr <- format0(frame$yval2[,2], digits)
    newline <- if(under) "\n\n" else "\n"
    ex <- if(extra < 100) extra else extra - 100
    if(ex == 0)
        labs <- rate
    else if(ex == 1) {      # add number of events and n?
        if(is.null(xsep))
            xsep <- " / "
        labs <- sprintf("%s%s%s%s%s",
            rate, newline, nbr, xsep, format0(frame$n, digits))
    } else if(ex == 2) {    # add number of events?
        labs <- sprintf("%s%s%s", rate, newline, nbr, digits)
        newline <- "  "     # want percent, if any, on same line
    } else
        stop0("extra=", extra, " is illegal (for method=\"", obj$method, "\")")

    if(extra >= 100)        # add percent?
        labs <- sprintf("%s%s%s%%", labs, newline,
                        formatf(100 * frame$wt / frame$wt[1], digits=max(0, digits-2)))
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
check.returned.labs <- function(obj, labs, fun.name)
{
    if(length(labs) == 0)
        print.node.labs.and.stop(labs, fun.name, "length(labs) == 0")
    if(!is.character(labs)) {
        labs <- as.character(labs)
        if(anyNA(labs))
            print.node.labs.and.stop(labs, fun.name, "NA in labs")
    }
    if(length(labs) != nrow(obj$frame))
        print.node.labs.and.stop(labs, fun.name, "\nthe number ", length(labs),
            " of returned labels is not equal to the number of rows in frame ",
            nrow(obj$frame))
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
