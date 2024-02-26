# rpart.rules.R:

allowed.styles <- c("wide", "tall", "tallw")

rpart.rules <- function(x=stop("no 'x' argument"),
                        style="wide", cover=FALSE, nn=FALSE,
                        roundint=TRUE, clip.facs=FALSE,
                        varorder=NULL, ...)
{
    if(!inherits(x, "rpart"))
        stop("Not an rpart object")

    ret <- check.if.dot.arg.supported.by.rpart.rules(...)
        extra         <- ret$extra
        digits        <- ret$digits
        varlen        <- ret$varlen
        faclen        <- ret$faclen
        trace         <- ret$trace
        facsep        <- ret$facsep
        eq            <- ret$eq
        lt            <- ret$lt
        ge            <- ret$ge
        and           <- ret$and
        when          <- ret$when
        because       <- ret$because
        null.model    <- ret$null.model
        response.name <- ret$response.name
        rpart.predict <- ret$rpart.predict # hidden arguments for rpart.predict
        where         <- ret$where

    obj <- x
    style <- match.choices(style, allowed.styles)
    cover <- check.boolean(cover)
    nn <- check.boolean(nn)
    roundint <- check.boolean(roundint)
    clip.facs <- check.boolean(clip.facs)
    rpart.predict <- check.boolean(rpart.predict)
    digits <- process.digits.arg(digits)
    if(digits < 0) # non negative because we use standard data.frame formatting
        digits <- -digits
    varlen <- check.integer.scalar(varlen, logical.ok=FALSE)
    faclen <- check.integer.scalar(faclen, logical.ok=FALSE)
    obj$varinfo <- get.modelframe.info(obj, roundint, trace,
                                       parent.frame(), "rpart.rules")
    if(is.null(response.name)) # not explicitly specified by the user?
        response.name <- obj$varinfo$response.name
    stopifnot.string(response.name)
    stopifnot.string(facsep)
    # we trim spaces around these because print.data.frame
    # unavoidably adds spaces between columns
    stopifnot.string(eq, allow.empty=TRUE);
    eq <- trim.surrounding.space(eq)
    stopifnot.string(lt, allow.empty=TRUE);
    lt <- trim.surrounding.space(lt)
    stopifnot.string(ge, allow.empty=TRUE);
    ge <- trim.surrounding.space(ge)
    stopifnot.string(and, allow.empty=TRUE);
    and <- trim.surrounding.space(and)
    stopifnot.string(when, allow.empty=TRUE);
    when <- trim.surrounding.space(when)
    if(when == "" && (style %in% c("tall", "tallw") || rpart.predict))
        when <- ":EMPTY:"
    else if(nrow(obj$frame) == 1)   # null model? (no rules)
        when <- null.model          # hack
    stopifnot.string(because, allow.empty=TRUE);
    because <- trim.surrounding.space(because)
    stopifnot.string(null.model)
    trace <- as.numeric(check.numeric.scalar(trace, logical.ok=TRUE))
    # we get the variable names from the splits because
    # attr(x$terms,"dataClasses") sometimes doesn't save the actual
    # variable names (e.g. model fit9 in slowtests/rpart.report.R)
    varnames <-
        if(nrow(obj$frame) == 1)        # null model? (no rules)
            varnames <- ":NULL.MODEL:"
        else
            unique(rownames(obj$splits))

    ret <- get.raw.rules(obj, extra, varlen, faclen, roundint, trace,
                         facsep, varnames)
        rules          <- ret$rules
        nrules.per.var <- ret$nrules.per.var

    if(trace >= 1)
        trace.print.rules(rules, "raw rules")

    rules <- process.rules(obj, rules, style, cover, nn, clip.facs,
                rpart.predict, where,
                eq, lt, ge, and, when, because, null.model,
                digits, trace, varorder, varlen,
                nrules.per.var, varnames,
                response.name,
                obj$method == "class" || is.class.response(obj),
                attr(obj, "ylevels"))

    node.numbers <- rownames(rules)

    if(trace >= 1)
        trace.print.rules(rules, "processed rules")

    class(rules) <- c("rpart.rules", "data.frame")
    attr(rules, "style") <- style
    attr(rules, "eq")    <- eq
    attr(rules, "and")   <- and
    attr(rules, "when")  <- when

    if(rpart.predict) {
        # return a vector of strings, one string for each element of where
        # TODO sometimes still too much whitespace e.g. example(rpart.predict)
        # TODO must also trim trailing space
        rules <- capture.output(print.rpart.rules(rules))
        rules <- rules[-1] # drop data.frame column names
        # trim leading single space added by print.data.frame in print.rpart.rules
        rules <- gsub("^ ", "", rules)
    }
    rules
}
print.rpart.rules <- function(x=stop("no 'x' argument"),
                              style=attr(x, "style"), ...)
{
    old.warnPartialMatchDollar <- getOption("warnPartialMatchDollar")
    if(is.boolean(old.warnPartialMatchDollar)) # prevents problem when old value is NULL
        on.exit(options(warnPartialMatchDollar=old.warnPartialMatchDollar))
    options(warnPartialMatchDollar=FALSE)

    # some hand holding for an easy error: specifying digits in print.rpart.rules
    dots <- match.call(expand.dots=FALSE)$...
    if(!is.null(dots$di) || !is.null(dots$dig) || !is.null(dots$digi) ||
       !is.null(dots$digit) || !is.null(dots$digits))
        stop0("specify 'digits' in rpart.rules (not in print.rpart.rules)")

    if(is.boolean(old.warnPartialMatchDollar))
        options(warnPartialMatchDollar=old.warnPartialMatchDollar)

    stop.if.dot.arg.used(...)
    style <- match.choices(style, allowed.styles)
    old.width <- options(width=1e4)$width
    on.exit(options(width=old.width))
    if(style == "wide") {
        class(x) <- "data.frame"
        print(x, row.names=FALSE)
    } else if(style == "tall" || style == "tallw")
        print_style_tall(x, style, eq=attr(x, "eq"),
                         and=attr(x, "and"), when=attr(x, "when"))
    else
        stop0("illegal style ", style)
}
# The raw rules are a data.frame like:
#   lab fit iclass cover Girth= Girth< Girth>= Height= Height< Height>=
#        18     18    48           12
#        31     31    29           16      12
#        56     56    23                   16
#
# Or like this for a multiclass model (lab inited, multiple fits in fit colum):
#   lab         fit iclass cover   sex= sex< sex>= age= age< age>=
#   1st .74 .16 .10      1    11 female                         35
#   1st .54 .26 .20      1     7   male                   46
#   3rd .21 .25 .54      3     9   male                   46    35
#   3rd .15 .21 .64      3    73                                35
#
# The raw data.frame has full variable names (varlen is only applied later)
#
# fit is the fit displayed in the leaf (could be a prob or vector of probs)
#
# lab is used only for multiclass models
#
# iclass is used only to sort rows on the fitted class for multiclass models
#   for anova models, iclass is floor(fit)
#   for class models, iclass is the fitted class as an integer

get.raw.rules <- function(obj, extra, varlen, faclen, roundint, trace,
                          facsep, varnames)
{
    ret <- get.node.and.split.labs(obj, extra, faclen, roundint, trace,
                                  facsep, under.percent=2)
        node.labs  <- ret$node.labs
        split.labs <- ret$split.labs

    frame <- obj$frame
    is.leaf <- is.leaf(frame)
    node.numbers <- as.numeric(row.names(frame))

    # map frame row to splits row (matrix: column is iframe, row is isplit)
    iframe.to.isplit.mat <- descendants(node.numbers)

    maxrules <- 1e3 # arb
    nrules <- 0

    # first 4 columns are always lab, fit, iclass, cover; then 3 cols per variable
    # column names are c("lab", "fit", "iclass", "cover",
    #                    "Girth=", "Girth<", "Girth>=", "Height=", "Height<", "Height>=")
    rules <- matrix("", nrow=maxrules, ncol=4 + 3 * length(varnames))
    colnames(rules) <- c("lab", "fit", "iclass", "cover",
                          paste0(rep(varnames, each=3), c("=", "<", ">=")))
    rules <- as.data.frame(rules, stringsAsFactors=FALSE)

    # number of rules each variable is in, for sorting columns on var importance
    nrules.per.var <- repl(0, length(varnames))
    names(nrules.per.var) <- varnames

    trace1(trace, "\n")
    for(iframe in 1:nrow(frame)) if(is.leaf[iframe]) { # for each leaf in frame
        nrules <- nrules + 1
        if(nrules > maxrules)
            stopf("too many rules (maximum number of rules is %d)", maxrules)
        ret <- get.rule(obj, rules[nrules,], nrules.per.var,
                        iframe, node.numbers, node.labs, split.labs,
                        iframe.to.isplit.mat, trace)
            rules[nrules,] <- ret$rule
            nrules.per.var <- ret$nrules.per.var
    }
    trace1(trace, "\n")
    rules <- rules[1:nrules, , drop=FALSE]
    # rownames are node numbers (we use them only if nn=TRUE)
    rownames(rules) <- rownames(obj$frame)[is.leaf]
    list(rules=rules, nrules.per.var=nrules.per.var)
}
get.node.and.split.labs <- function(obj, extra, faclen, roundint, trace,
                                    facsep, under.percent)
{
    class.stats <- NULL
    if(obj$method == "class" || is.class.response(obj))
        class.stats <- get.class.stats(obj)
    extra <- handle.extra.for.rules(extra, obj, class.stats)
    list(node.labs =
            internal.node.labs(obj,
                node.fun=NULL, node.fun.name="NULL",
                type=TYPE0.default, extra=extra,
                under=FALSE, xsep=NULL,
                digits=-10, # we will apply digits later
                varlen=0,   # full variable names in raw data.frame
                prefix="", suffix="", class.stats, under.percent),
        split.labs =
            internal.split.labs(obj, type=TYPE4.fancy.all,
                digits=-10, # we will apply digits later
                varlen=0,   # full variable names in raw data.frame
                faclen=faclen, roundint=roundint,
                clip.facs=FALSE, # factor names are not stripped in raw data.frame
                clip.left.labs=FALSE, clip.right.labs=FALSE, xflip=FALSE,
                trace=trace, facsep=facsep,
                # note: parse split.lab code later relies on "|" in these args
                # we use | and not space to allow space in varnames
                eq="|=|", logical.eq="|=|", lt="|<|", ge="|>=|",
                split.prefix="", right.split.suffix="",
                split.suffix="", right.split.prefix=""))
}
handle.extra.for.rules <- function(extra, obj, class.stats)
{
    if(is.numeric(extra)) {
        stopifnot(length(extra) == 1)
        if(extra >= 100)
            extra <- extra - 100
        if(extra == EX0)
            extra <- get.default.extra(obj, class.stats) - 100
        else if(extra == EX1.NOBS ||
                extra == EX2.CLASS.RATE ||
                extra == EX3.MISCLASS.RATE) {
            warning0(
"extra=", extra, " is not supported by rpart.rules (although useable for plots)")
            extra <- get.default.extra(obj, class.stats) - 100
        } else if(extra == EX5.PROB.PER.CLASS.DONT ||
                  extra == EX7.PROB.2ND.CLASS.DONT ||
                  extra == EX11.PROB.ACROSS.ALL.2ND.CLASS.DONT)
            extra <- extra - 1 # must have class label for parse.split.lab
    } else if(is.auto(extra, n=1)) {
        extra <- get.default.extra(obj, class.stats)
    } else
        stop0("rpart.rules: illegal extra")
    if(obj$method == "poisson" || obj$method == "exp")
        extra <- 0
    if(extra < 100)
        extra <- extra + 100
    extra
}
# descendants is lifted verbatim from rpart version 4.1-13 dated 2018-02-23
descendants <- function(nodes, include = TRUE)
{
    n <- length(nodes)
    if (n == 1L) return(matrix(TRUE, 1L, 1L))
    ind <- 1:n
    desc <- matrix(FALSE, n, n)
    if (include) diag(desc) <- TRUE
    parents <- match((nodes %/% 2L), nodes)
    lev <- floor(log(nodes, base = 2))
    desc[1L, 2L:n] <- TRUE
    for (i in max(lev):2L) {
        desc[cbind(ind[parents[lev == i]], ind[lev == i])] <- TRUE
        parents[lev == i] <- parents[parents[lev == i]]
        lev[lev == i] <- i - 1L
    }
    desc
}
get.rule <- function(obj, rule, nrules.per.var,
                     iframe, node.numbers, node.labs, split.labs,
                     iframe.to.isplit.mat, trace)
{
    ret <- parse.node.lab(node.labs[iframe]) # note that iframe indexes a leaf
        rule$lab   <- ret$lab
        rule$fit   <- ret$fit
        rule$cover <- ret$cover

    rule$iclass <- floor(obj$frame[iframe, "yval"]) # predicted class for class models

    # The order of the splits in path is important.
    # Later splits take precedence because so if we have
    # path=c("root", "Girth< 16.15", "Girth< 12.45")
    # then 12.45 takes precedence over 16.15
    path <- split.labs[iframe.to.isplit.mat[, iframe]]
    trace1(trace, "iframe %3d node %3d path %s\n",
           iframe, node.numbers[iframe], bar.to.space(path))
    stopifnot(path[1] == "root")
    path <- path[-1] # drop root
    for(split.lab in path) {
        ret <- parse.split.lab(split.lab, trace)
        if(!var.is.in.rule(ret$varname, rule))
            nrules.per.var[ret$varname] <- nrules.per.var[ret$varname] + 1
        rule[paste0(ret$varname, ret$op)] <- ret$cut
    }
    list(rule=rule, nrules.per.var=nrules.per.var)
}
trace.print.rules <- function(rules, msg) # only used if trace > 0
{
    old.width <- options(width=1e4)$width
    on.exit(options(width=old.width))
    cat0(msg, ":\n")
    class(rules) <- "data.frame"
    print(rules) # all columns are character except iclass
    cat0("\n")
}
process.rules <- function(obj, rules, style, cover, nn, clip.facs,
                          rpart.predict, where,
                          eq, lt, ge, and, when, because, null.model,
                          digits, trace, varorder, varlen,
                          nrules.per.var, varnames, response.name,
                          is.class.response, ylevels)
{
    ret <- format_fit(rules$fit, digits, is.class.response)
        rules$fit <- ret$fit
        rowmaxs   <- ret$rowmaxs
        ncol.fit  <- ret$ncol.fit
    fit <- rules$fit # needed for fit.colname()
    rules <- order.rows(rules, rowmaxs)
    ret <- order.cols(rules, varorder, varnames, nrules.per.var)
        rules    <- ret$rules
        varnames <- ret$varnames
    if(varlen != 0) { # note that we do this only after using varnames to sort cols above
        ret <- apply.varlen.to.colnames(rules, varnames, varlen)
            colnames(rules) <- ret$colnames
            varnames        <- ret$shortnames
    }
    rules.cover <- rules$cover
    rules <- format_rules(rules, style, cover, clip.facs, eq, lt, ge, and, when,
                          digits, trace,
                          response.name, varnames, ncol.fit)
    # retain only used columns
    rules <- rules[, apply(rules, 2, function(col) any(col != "")), drop=FALSE]
    if(!rpart.predict) {
        # all columns unnamed, except first column (response.name)
        colnames(rules) <- c(response.name, repl("", ncol(rules)-1))
    } else { # rpart.predict
        if(nrow(obj$frame) == 1) # null model? (no rules)
            rules <- as.data.frame(matrix(paste0(because, paste0(" ", null.model)),
                                   nrow=nrow(rules)), stringsAsFactors=FALSE)
        else {
            # drop all columns up to column "when"
            iwhen <- match("when", colnames(rules))[1] # index of "when" column
            if(iwhen < ncol(rules)) {
                rules <- rules[, iwhen:ncol(rules), drop=FALSE]
                rules[1] <- because # replace "when" with "because"
            }
        colnames(rules) <- NULL # all columns unnamed
        }
    }
    if(cover) {
        # append cover column (space in "  cover" to shift printed cover column right)
        rules$cover <- sprint("%3.0f%%", as.numeric(rules.cover))
        colnames(rules)[ncol(rules)] <- "  cover"
    }
    if(nn) {
        colnames <- colnames(rules)
        rules <- cbind(rownames(rules), rules, stringsAsFactors=FALSE)
        colnames(rules) <- c("nn", colnames)
    }
    if(rpart.predict) {
        # must generate the rule for each element of where
        if(style != "wide")
            stop0("style = \"", style, "\" is not supported by rpart.predict")
        check.vec(where, "where", na.ok=TRUE)
        nn <- as.numeric(rownames(obj$frame)[where])
        stopifnot(!any(is.na(nn)))
        # rules indexed by node number
        rules.nn <- rules
        rules.nn[1:max(nn), ] <- rules[1, , drop=FALSE]
        for(name in rownames(rules))
            rules.nn[as.numeric(name),] <-
                rules[which(rownames(rules) == name), , drop=FALSE]
        rules <- rules.nn[nn, , drop=FALSE]
        rownames(rules) <- NULL
        # retain only used columns
        rules <- rules[, apply(rules, 2, function(col) any(col != "")), drop=FALSE]
    } else if(ncol.fit > 1) { # multiple probabilities
        # add column names etc. for a nice print
        colnames(rules)[2+nn] <- fit.colname(ylevels, fit, ncol.fit)
        rules[,2+nn] <- paste0("[", rules[,2+nn], "]")
        rules[,3+nn] <- paste0(" ", when)
    }
    # following trim is necessary because we may not use all elements in
    # format.gt and format.le although we called format() with all elements
    trim.leading.space.in.columns(rules)
}
format_fit <- function(fit, digits, is.class.response)
{
    fit <- strsplit(fit, " ", fixed=TRUE) # ".12 .34 .56" becomes ".12" ".34" ".56"
    nrow <- length(fit) # fit is a list
    fit <- matrix(as_numeric_na_ok(unlist(fit)), nrow=nrow, byrow=TRUE) # matrix of floats
    ncol.fit <- ncol(fit)
    rowmaxs <-
        if(ncol.fit == 2) {
            # binomial model with both probabilities predicted e.g. ".12" ".34"
            # want to order rows on prob of 2nd class (ignoring prob of 1st class)
            rowmaxs <- fit[,2]
        } else
            rowmaxs <- apply(fit, 1, max)     # max of each row of fits
    if(ncol.fit == 1) {
        fit <- # align for printing
            if(is.class.response) # probabilities, always 2 decimal places
                format(sprint("%.2f", fit), justify="right")
            else # anova, poisson, or exp model
                format(fit, digits=digits, justify="right")
    } else {
        # Multiple responses per split e.g. ".12" ".34" ".56".
        # Want fixed number of decimal places, don't use format()
        digits <- 2 # ignore digits argument to rpart.rules
        max.rowmaxs <- max(rowmaxs, na.rm=TRUE)
        format <- if(max.rowmaxs >= 1)                # a prob is 1.00 (or greater)?
                    sprint("%%%d.%df", digits+2, digits) # e.g. "%4.2f"
                  else
                    sprint("%%%d.%df", digits+1, digits) # e.g. "%3.2f"
        fit <- matrix(paste(sprint(format, fit)), nrow=nrow(fit)) # format each elem
        fit <- apply(fit, 1, paste, collapse=" ")     # make each row a single string
        fit <- if(max.rowmaxs >= 1)
                    gsub("0.", " .", fit, fixed=TRUE) # "0.12" becomes " .12"
                else
                    gsub("0.", ".", fit, fixed=TRUE)  # "0.12" becomes ".12"
    }
    list(fit=fit, rowmaxs=rowmaxs, ncol.fit=ncol.fit)
}
trim.surrounding.space <- function(s)
{
    # trim leading and trailing space (this trims a maximum of one space on each side)
    gsub("^ | $", "", s)
}
as_numeric_na_ok <- function(x) # as.numeric issues warning if NAs, we don't want that
{
    old.warn <- getOption("warn")   # no warning in as.numeric() if NAs in fit
    on.exit(options(warn=old.warn))
    options(warn=-1)                # temporarily turn off warnings
    as.numeric(x)
}
apply.varlen.to.colnames <- function(rules, varnames, varlen)
{
    shortnames <- my.abbreviate(varnames, varlen)
    colnames <- colnames(rules)
    for(i in seq_along(varnames)) {
        ivar <- 3 * i + 2 # index of "Girth=" column (skip lab,fit,iclass,cover)
        colnames[ivar:(ivar+2)] <-
            sub(varnames[i], shortnames[i], colnames[ivar:(ivar+2)], fixed=TRUE)
    }
    list(colnames=colnames, shortnames=shortnames)
}
format_rules <- function(rules, style, cover, clip.facs, eq, lt, ge, and, when,
                         digits, trace,
                         response.name, varnames, ncol.fit)
{
    n <- function() # generate a new column name
    {
        icol <<- icol + 1
        sprint("c%d", icol)
    }
    #--- format_rules starts here ---
    # build up the new rules column by column

    new <- if(ncol.fit > 1) # multiple responses, add class label
                data.frame(class=rules$lab, fit=rules$fit, stringsAsFactors=FALSE)
           else
                data.frame(fit=rules$fit, stringsAsFactors=FALSE)

    rownames(new) <- rownames(rules)
    new$when <- when
    icol <- 0 # global var for n() function
    # subsequent is TRUE if split is not the first for this rule
    subsequent <- repl(FALSE, nrow(rules))
    for(i in seq_along(varnames)) {
        # process three columns for the variable ("Girth=", "Girth<", "Girth>=")
        # simultaneously process all rows for each column
        varname <- varnames[i]

        ivar <- 3 * i + 2           # index of "Girth=" column (skip lab,fit,iclass,cover)
        rules.eq <- rules[, ivar]   # "Girth=", vector of strings
        rules.lt <- rules[, ivar+1] # "Girth<"
        rules.ge <- rules[, ivar+2] # "Girth>="

        is.eq    <- rules.eq != ""  # vector of bools
        is.lt    <- rules.lt != ""
        is.ge    <- rules.ge != ""
        lt.or.ge <- is.lt | is.ge

        if(any(is.eq)) { # pclass is 1st or 2nd, boolean = 1, boolean = 0
            new[,n()] <- ifelse(subsequent & is.eq, and, "")
            subsequent <- (subsequent | is.eq)
            # formatting for rules with "=" in them works like this:
            # clip.facs FALSE: sex is female & boolean1 = 1 & boolean2 = 0
            # clip.facs TRUE:  female        & boolean1     & not boolean2
            # the checks against "1" and "0" are for for logical variables
            new[,n()] <- ifelse(clip.facs  & is.eq & rules.eq == "0",
                            sprint("not %s", varname),
                         ifelse(clip.facs  & is.eq & rules.eq == "1",
                            sprint("%s    ", varname),
                         ifelse(!clip.facs & is.eq,
                            sprint("%s", varname), "")))
            new[,n()] <- ifelse(is.eq & !clip.facs, eq, "")
            new[,n()] <- ifelse(is.eq & (!clip.facs | (rules.eq != "1" & rules.eq != "0")),
                            rules.eq, "")
        } else if(any(lt.or.ge)) { # Girth < 12.45, Girth >= 16.25, or both
            new[,n()] <- ifelse(subsequent & lt.or.ge, and, "")
            subsequent <- (subsequent | lt.or.ge)
            # add "verysmall" so format rounds .5 upwards, not to even
            verysmall <- exp10(-abs(digits) - 8)
            if(any(is.lt)) # format for printing
                rules.lt <- format(as.numeric(rules.lt) + verysmall,
                                   digits=digits, justify="right")
            if(any(is.ge))
                rules.ge <- format(as.numeric(rules.ge) + verysmall,
                                   digits=digits, justify="right")
            new[,n()] <- ifelse(lt.or.ge, varname, "")
            if(any(is.lt & is.ge)) {
                # at least one row has both lt and ge although any particular row
                # could be Girth 12.45 to 16.25, Girth < 12.45, Girth >= 16.25
                new[,n()] <- ifelse(is.lt & !is.ge,  lt,
                             ifelse(is.ge & !is.lt,  ge,
                             ifelse(is.ge | is.lt,   eq, "")))
                new[,n()] <- ifelse(is.lt & is.ge,   rules.ge,
                             ifelse(is.lt, rules.lt, ""))
                new[,n()] <- ifelse(is.lt & is.ge,   "to",  "")
                new[,n()] <- ifelse(is.lt & is.ge,   rules.lt,
                             ifelse(is.ge, rules.ge, ""))
            } else {                # Girth < 12.45, Girth >= 16.25
                # no row has both lt and ge
                new[,n()] <- ifelse(is.lt,           lt,
                             ifelse(is.ge,           ge, ""))
                new[,n()] <- ifelse(is.lt, rules.lt,
                             ifelse(is.ge, rules.ge,  ""))
            }
        }
    }
    new
}
# predicting multiple probabilities
# add truncated class names like "1st 2nd 3rd" above the ".12 .34 .56"
fit.colname <- function(ylevels, fit, ncol.fit)
{
    # Aug 2019: commented following out because if last level in the response
    # is unused in the training data, then it does't appear in yval2
    # e.g. see "unusedlev" in the rpart.plot tests
    #
    # if(ncol.fit != length(ylevels)) # should never happen
    #     warning0("ncol(fit) ", ncol.fit, " != length(ylevels) ", length(ylevels),
    #              "\n           ylevels(fit):  ", quotify(ylevels), "\n")

    ylevels <- ylevels[1:ncol.fit] # necessary if last level(s) are unused in training data

    width <- unlist(gregexpr(" ", substring(fit, 2)))[1] # position of first space
    if(width < 1) # paranoia                             # substring to skip possible lead space
        width <- 1
    format <- sprint("%%%d.%ds", width, width) # e.g. "%3.3s"
    colname <- paste.collapse(sprint(format, ylevels))
    colname <- paste0(colname, " ") # space for "]" in column entries
    colname                         # "1st 2nd 3rd"
}
# if all elements in a column have leading space, trim that space
trim.leading.space.in.columns <- function(x)
{
    stopifnot(NROW(x) > 0)
    for(j in 1:NCOL(x)) {
        x1 <- x[,j]
        x1 <- x1[x1 != ""]
        len <- unlist(gregexpr("^ +", x1)) # length of leading spaces
        if(!is.null(len)) {
            min <- min(len)                    # shortest leading space
            if(min > 0)
                x[,j] <- substring(x[,j], min+1)
        }
    }
    x
}
# true if any of "Girth=", "Girth<", "Girth>=" is used
var.is.in.rule <- function(varname, rule)
{
    rule[paste0(varname, "=") ] != "" ||
    rule[paste0(varname, "<") ] != "" ||
    rule[paste0(varname, ">=")] != ""
}
# parse split.lab into varname, op, cut
parse.split.lab <- function(split.lab, trace)
{                                                   # split.lab="Girth|<|16.15"
    i <- gregexpr("|", split.lab, fixed=TRUE)[[1]]  # posn of "|" around "|<|"
    if(length(i) != 2 || i[2] < i[1] + 2)           # will fail if | in varname
        stopf("Cannot parse split.lab %s", bar.to.space(split.lab))
    varname <- substring(split.lab, 1, i[1]-1)      # "Girth"
    op      <- substring(split.lab, i[1]+1, i[2]-1) # "<"
    cut     <- substring(split.lab, i[2]+1)         # "16.5"
    trace2(trace,
           "                split.lab %-20.20s varname %s op %s cut %s\n",
           bar.to.space(split.lab), varname, op, cut)
    list(varname=varname, op=op, cut=cut)
}
parse.node.lab <- function(node.lab)
{
    err <- function(node.lab)
        stop0("Cannot parse node.lab \"",
               gsub("\n", "\\\\n", node.lab[1]), "\"") # convert newline to string "\n"

    #--- parse.node.lab starts here ---
    lab <- fit <- cover <- repl("", length(node.lab))
    i <- gregexpr("\n", node.lab, fixed=TRUE)[[1]]
    if(length(i) > 2) {
        # For labels like this: (note extra new line)
        # "Japan\n.00  .00  .00  .11  .89\n.00  .00  .00  .00  .00\n18%"
        i <- c(i[1], i[length(i)])
    }
    if(length(i) == 1) {
        # anova or poisson model "14.94\n25.81%"
        if(i <= 0)
            err(node.lab)
        fit   <- substr(node.lab, 1, i-1)
        cover <- substr(node.lab, i+1, nchar(node.lab)-1)
    } else if(length(i) == 2) {
        # class model: "died\n0.17\n61%" or "1st\n.63  .31  .06\n59%"
        lab   <- substr(node.lab, 1, i[1]-1)
        fit   <- substr(node.lab, i[1]+1, i[2]-1)
        fit   <- gsub("  ", " ", fit) # convert double space to single space
        fit   <- gsub("\n", " ", fit) # convert \n to space
        cover <- substr(node.lab, i[2]+1, nchar(node.lab)-1)
    } else
        err(node.lab)

    list(lab=lab, fit=fit, cover=cover)
}
# convert | to space and add quotes, for use in messages to the user
# (the "|" arises from call to internal.split.labs with eq="|=|", lt="|<|", ge="|>=|")
bar.to.space <- function(s)
{
    quote.with.c(gsub("|", " ", s, fixed=TRUE))
}
# Sort on iclass, then on fit, then order of rows in splits.
# For anova models, iclass and fit are the same.
# For class models, iclass is the fitted class (as an integer)
# Sort on order of rows in splits means left-to-right in tree.

order.rows <- function(rules, rowmaxs)
{
    order <- order(as.numeric(rules$iclass), rowmaxs, 1:length(rowmaxs), na.last=TRUE)
    rules[order, , drop=FALSE]
}
# change order of columns so most important variables first, override with varorder
order.cols <- function(rules, varorder, varnames, nrules.per.var)
{
    order <- order(nrules.per.var, decreasing=TRUE)
    if(!is.null(varorder)) {
        # move variables in varorder to front of order vector
        stopifnot(is.character(varorder))
        varorder <- rev(varorder) # rev(varorder) so first var prepended last
        pmatch <- pmatch(varorder, varnames, duplicates.ok=TRUE)
        for(i in seq_along(pmatch)) {
            if(is.na(pmatch[i]))
                warnf(
"varorder=\"%s\" does not uniquely match one of: %s",
                         varorder[i], paste.trunc(quotify(varnames)))
            else {
                order <- order[order != pmatch[i]] # remove the variable
                order <- c(pmatch[i], order)       # prepend it
            }
        }
    }
    ivar <- 3 * rep(order, each=3) + c(2, 3, 4) # lab,fit,iclass,cover and 3 cols per var
    list(rules    = rules[, c(1:4, ivar), drop=FALSE], # 1:4 is lab,fit,iclass,cover
         varnames = varnames[order])
}
print_style_tall <- function(rules, style, eq, and, when)
{
    newline.with.spaces <- function()
    {
        printf("\n")
        if(style == "tall")
            printf("   ")
        else # "tallw"
            printf(format, "", "  ", "") # prefix space to align with prolog
        printf(format2, "")
        if(have.nn)
            printf(format.nn, "")
    }
    #--- print_style_tall starts here ---
    colnames <- colnames(rules)
    ncol <- ncol(rules)
    have.nn <- colnames[1] == "nn"
    have.cover <- colnames[ncol] == "  cover"
    response.name <- colnames[1 + have.nn]
    class.probs   <- colnames[2 + have.nn]
    have.class.probs <- class.probs != "" # currently used only for multi class models
    if(is.null(and))
        and <- " & "

    # output will be as follows ("format" is for "survived is 0.93"):
    #
    # survived is 0.93 when
    #                  sex is female is
    #                  pclass is 1st or 2nd

    # get format for prolog of each rule   # e.g. "survived is 0.93"
    format <- sprint("%%-%ds %%s %%-%ds", # e.g. "%-8s %s %-4s"
                     nchar(response.name),
                     max(nchar(rules[, 1 + have.nn])))

    nn.width <- if(have.nn) max(nchar(rules[, 1]))+3 else 0 # +3 for "[] "
    format.nn <- sprint("%%-%ds", nn.width) # e.g. "%-0s" or "%-5s"

    format2 <- "%0.0s"
    if(have.class.probs) {
        # output will be as follows ("format" is for "pclass is 1st"
        # and "format2" is for "[.74 .16 .10]"):
        #
        # pclass is 1st [.74 .16 .10] when
        #                             age >= 35
        #                             survived is survived

        # prefix spaces to align with prolog
        printf(format.nn, "")
        printf(format, "", "  ", "")
        printf("  %s\n", colnames[2 + have.nn])
        if(style == "tallw")
            format2 <- sprint("%%-%ds  ", nchar(colnames[2 + have.nn]))
    }
    for(i in 1:nrow(rules)) {
        if(have.nn)
            printf(format.nn, sprint("[%s] ", rules[i, 1]))
        printf(format, response.name, eq, rules[i, 1 + have.nn])
        for(j in (2 + have.nn):(ncol(rules) - have.cover)) {
            e <- trim.surrounding.space(rules[i, j])
            if(nchar(e)) {
                if(e == when) {
                    if(have.cover)
                        printf(" with cover %-s", gsub("^ *", "", rules[i, ncol]))
                    printf(" %s", if(when == ":EMPTY:") "" else when)
                    newline.with.spaces()
                } else if(e == and)
                    newline.with.spaces()
                else
                    printf(" %s", e)
            }
        }
        printf("\n")
        if(i != nrow(rules))
            printf("\n")
    }
}
# stop.if.dot.arg.used will cause an error message if any args are passed to it.
# We use it to test if any dots arg of the calling function was used, for
# functions that must have a dots arg (to match the generic method) but don't
# actually use the dots.  This helps the user catch mistyped or illegal args.
stop.if.dot.arg.used <- function()
{
    NULL
}
# this also issues an error if an illegal argument name is attempted
# following args must match args of prp() except where commented below
check.if.dot.arg.supported.by.rpart.rules <- function(x=stop("no 'x' arg"),
    type=0,
    extra="auto", # different default
    under=FALSE, fallen.leaves=FALSE,
    nn=FALSE, ni=FALSE, yesno=TRUE,
    branch=if(fallen.leaves) 1 else .2,
    uniform=TRUE, left=TRUE, xflip=FALSE, yflip=FALSE,
    digits=2,
    varlen=0, faclen=0, # different defaults
    # roundint=TRUE,
    cex=NULL, tweak=1,
    # clip.facs=FALSE,
    clip.right.labs=TRUE,
    compress=TRUE, ycompress=uniform,
    Margin=0, space=1, gap=NULL,
    snip=FALSE, snip.fun=NULL, trace=FALSE,

    box.col=0, box.palette=0,
    pal.thresh=NULL, pal.node.fun=FALSE,
    border.col=col,
    round=NULL, leaf.round=NULL,
    shadow.col=0, prefix="", suffix="", xsep=NULL,

    under.font=1, under.col=1, under.cex=.8,

    split.cex=1, split.font=2, split.family=1, split.col=1,
    split.box.col=0, split.border.col=0,
    split.lty=1, split.lwd=NULL, split.round=0,
    split.shadow.col=0,
    split.prefix="", right.split.prefix=NULL,
    split.suffix="", right.split.suffix=NULL,
    facsep=" or ", eq=" is ", # different defaults
    lt=" <  ", ge=" >= ",

    branch.col=if(is.zero(branch.type)) 1 else "gray",
    branch.lty=1, branch.lwd=NULL,
    branch.type=0, branch.tweak=1,
    min.branch.width=.002, branch.fill=branch.col,

    nn.cex=NULL, nn.font=3, nn.family="", nn.col=1,
    nn.box.col=0, nn.border.col=nn.col,
    nn.lty=1, nn.lwd=NULL, nn.round=.3,
    yes.text="yes", no.text="no",

    node.fun=NULL,
    split.fun=NULL,
    FUN="text",

    nspace=branch, minbranch=.3, do.par=TRUE,
    add.labs=TRUE,
    clip.left.labs=(type == 5),
    fam.main="",
    yshift=0, yspace=space, shadow.offset=.4,

    split.adj=NULL, split.yshift=0, split.space=space,
    split.yspace=yspace, split.shadow.offset=shadow.offset,

    nn.adj=.5, nn.yshift=0, nn.space=.8, nn.yspace=.5,

    ygap=gap/2, under.ygap=.5, yesno.yshift=0,
    xcompact=TRUE, ycompact=uniform, xcompact.ratio=.8, min.inter.height=4,

    max.auto.cex=1, min.auto.cex=.15, ycompress.cex=.7, accept.cex=1.1,
    shift.amounts=c(1.5, 2),
    Fallen.yspace=.1,
    boxes.include.gap=FALSE,
    legend.x=NULL, legend.y=NULL, legend.cex=1,
    # extra args for rpart.rules, not in prp arg list
    and=" & ", when=" when ", because=" because ", null.model="null model",
    response.name=NULL,
    # hidden args for rpart.predict
    RPART.PREDICT=FALSE, WHERE=NULL)
{
    warn1 <- function(arg)
    {
        warnf("rpart.rules: ignoring argument '%s'", deparse(substitute(arg)))
    }
    # if(!missing(x))                 warn1(x)
    if(!missing(type))                warn1(type)
    # if(!missing(extra))             warn1(extra)
    if(!missing(under))               warn1(under)
    if(!missing(fallen.leaves))       warn1(fallen.leaves)
    if(!missing(nn))                  warn1(nn)
    if(!missing(ni))                  warn1(ni)
    if(!missing(yesno))               warn1(yesno)
    if(!missing(branch))              warn1(branch)
    if(!missing(uniform))             warn1(uniform)
    if(!missing(left))                warn1(left)
    if(!missing(xflip))               warn1(xflip)
    if(!missing(yflip))               warn1(yflip)
    # if(!missing(digits))            warn1(digits)
    # if(!missing(varlen))            warn1(varlen)
    # if(!missing(faclen))            warn1(faclen)
    # if(!missing(roundint))          warn1(roundint)
    if(!missing(cex))                 warn1(cex)
    if(!missing(tweak))               warn1(tweak)
    # if(!missing(clip.facs))         warn1(clip.facs)
    if(!missing(clip.right.labs))     warn1(clip.right.labs)
    if(!missing(compress))            warn1(compress)
    if(!missing(ycompress))           warn1(ycompress)
    if(!missing(Margin))              warn1(Margin)
    if(!missing(space))               warn1(space)
    if(!missing(gap))                 warn1(gap)
    if(!missing(snip))                warn1(snip)
    if(!missing(snip.fun))            warn1(snip.fun)
    # if(!missing(trace))             warn1(trace)
    if(!missing(box.col))             warn1(box.col)
    if(!missing(box.palette))         warn1(box.palette)
    if(!missing(pal.thresh))          warn1(pal.thresh)
    if(!missing(pal.node.fun))        warn1(pal.node.fun)
    if(!missing(border.col))          warn1(border.col)
    if(!missing(round))               warn1(round)
    if(!missing(leaf.round))          warn1(leaf.round)
    if(!missing(shadow.col))          warn1(shadow.col)
    if(!missing(prefix))              warn1(prefix)
    if(!missing(suffix))              warn1(suffix)
    if(!missing(xsep))                warn1(xsep)
    if(!missing(under.font))          warn1(under.font)
    if(!missing(under.col))           warn1(under.col)
    if(!missing(under.cex))           warn1(under.cex)
    if(!missing(split.cex))           warn1(split.cex)
    if(!missing(split.font))          warn1(split.font)
    if(!missing(split.family))        warn1(split.family)
    if(!missing(split.col))           warn1(split.col)
    if(!missing(split.box.col))       warn1(split.box.col)
    if(!missing(split.border.col))    warn1(split.border.col)
    if(!missing(split.lty))           warn1(split.lty)
    if(!missing(split.lwd))           warn1(split.lwd)
    if(!missing(split.round))         warn1(split.round)
    if(!missing(split.shadow.col))    warn1(split.shadow.col)
    if(!missing(split.prefix))        warn1(split.prefix)
    if(!missing(right.split.prefix))  warn1(right.split.prefix)
    if(!missing(split.suffix))        warn1(split.suffix)
    if(!missing(right.split.suffix))  warn1(right.split.suffix)
    # if(!missing(facsep))            warn1(facsep)
    # if(!missing(eq))                warn1(eq)
    # if(!missing(lt))                warn1(lt)
    # if(!missing(ge))                warn1(ge)
    if(!missing(branch.col))          warn1(branch.col)
    if(!missing(branch.lty))          warn1(branch.lty)
    if(!missing(branch.lwd))          warn1(branch.lwd)
    if(!missing(branch.type))         warn1(branch.type)
    if(!missing(branch.tweak))        warn1(branch.tweak)
    if(!missing(min.branch.width))    warn1(min.branch.width)
    if(!missing(branch.fill))         warn1(branch.fill)
    if(!missing(nn.cex))              warn1(nn.cex)
    if(!missing(nn.font))             warn1(nn.font)
    if(!missing(nn.family))           warn1(nn.family)
    if(!missing(nn.col))              warn1(nn.col)
    if(!missing(nn.box.col))          warn1(nn.box.col)
    if(!missing(nn.border.col))       warn1(nn.border.col)
    if(!missing(nn.lty))              warn1(nn.lty)
    if(!missing(nn.lwd))              warn1(nn.lwd)
    if(!missing(nn.round))            warn1(nn.round)
    if(!missing(yes.text))            warn1(yes.text)
    if(!missing(no.text))             warn1(no.text)
    if(!missing(node.fun))            warn1(node.fun)
    if(!missing(split.fun))           warn1(split.fun)
    if(!missing(FUN))                 warn1(FUN)
    if(!missing(nspace))              warn1(nspace)
    if(!missing(minbranch))           warn1(minbranch)
    if(!missing(do.par))              warn1(do.par)
    if(!missing(add.labs))            warn1(add.labs)
    if(!missing(clip.left.labs))      warn1(clip.left.labs)
    if(!missing(fam.main))            warn1(fam.main)
    if(!missing(yshift))              warn1(yshift)
    if(!missing(yspace))              warn1(yspace)
    if(!missing(shadow.offset))       warn1(shadow.offset)
    if(!missing(split.adj))           warn1(split.adj)
    if(!missing(split.yshift))        warn1(split.yshift)
    if(!missing(split.space))         warn1(split.space)
    if(!missing(split.yspace))        warn1(split.yspace)
    if(!missing(split.shadow.offset)) warn1(split.shadow.offset)
    if(!missing(nn.adj))              warn1(nn.adj)
    if(!missing(nn.yshift))           warn1(nn.yshift)
    if(!missing(nn.space))            warn1(nn.space)
    if(!missing(nn.yspace))           warn1(nn.yspace)
    if(!missing(ygap))                warn1(ygap)
    if(!missing(under.ygap))          warn1(under.ygap)
    if(!missing(yesno.yshift))        warn1(yesno.yshift)
    if(!missing(xcompact))            warn1(xcompact)
    if(!missing(ycompact))            warn1(ycompact)
    if(!missing(xcompact.ratio))      warn1(xcompact.ratio)
    if(!missing(min.inter.height))    warn1(min.inter.height)
    if(!missing(max.auto.cex))        warn1(max.auto.cex)
    if(!missing(min.auto.cex))        warn1(min.auto.cex)
    if(!missing(ycompress.cex))       warn1(ycompress.cex)
    if(!missing(accept.cex))          warn1(accept.cex)
    if(!missing(shift.amounts))       warn1(shift.amounts)
    if(!missing(Fallen.yspace))       warn1(Fallen.yspace)
    if(!missing(boxes.include.gap))   warn1(boxes.include.gap)
    if(!missing(legend.x))            warn1(legend.x)
    if(!missing(legend.y))            warn1(legend.y)
    if(!missing(legend.cex))          warn1(legend.cex)

    list(extra=extra, digits=digits, varlen=varlen, faclen=faclen, trace=trace,
         facsep=facsep, eq=eq, lt=lt, ge=ge, and=and,
         when=when, because=because, null.model=null.model,
         response.name=response.name,
         rpart.predict=RPART.PREDICT, where=WHERE)
}
