# lib.R: miscellaneous definitions for plot.rpart

stop0 <- function(...)          # call.=FALSE, so must use traceback() to see call
    stop(..., call.=FALSE)

warning0 <- function(...)       # set options(warn=2) and traceback() to see the call
    warning(..., call.=FALSE)

printf <- function(format, ...) cat(sprintf(format, ...))  # like c printf

paste0 <- function(...) paste(..., sep="") # paste with no added spaces

paste.with.space <- function(s) paste(s, collapse=" ")

cat0 <- function(...) cat(..., sep="") # cat with no added spaces

recycle <- function(x, refx) rep(x, length.out=length(refx))

repn <- function(x, n) rep(x, length.out=n)

is.left <- function(nodes) nodes %% 2 == 0

is.leaf <- function(frame) frame$var == "<leaf>"

stopifnot.boolean <- function(b) # b==0 or b==1 is also ok
{
    if(length(b) != 1 || !(is.logical(b) || is.numeric(b)) ||
            is.na(b) || !(b == 0 || b == 1)) {
        name <- deparse(substitute(b))
        cat0("\n", name, ": ")
        print(b)
        cat("\n")
        stop0("the ", name,
              " argument is not FALSE or TRUE or 0 or 1 (see above print)")
    }
}
# Check that func is indeed a function and has the same args as the reference function.
check.func.args <- function(func, func.name, ref.func)
{
    if(is.null(func))
        stop0("NULL is not a legal value for ", func.name)
    if(!is.function(func))
        stop0(func.name, " is not a function");
    names <- names(formals(func))
    ref.names <- names(formals(ref.func))
    if(!identical(names, ref.names)) {
        if(length(names) == 0)
            stop0("the ", func.name, " function needs the following args:\n    ",
                paste.with.space(ref.names))
        else
            stop0("the ", func.name, " function needs the following args:\n    ",
                paste.with.space(ref.names),
                "\nYou have:\n    ", paste.with.space(names))
    }
}
# Return TRUE if col matches the background color
# where "match" means if we plot the color we will not see it
is.invisible <- function(col, bg)
{
    all(col == bg | col == 0 | col == "transparent" | is.na(col))
}
is.box.invisible <- function(box.col, border.col, bg)
{
    is.invisible(box.col, bg) && is.invisible(border.col, bg)
}

last.family.global <- "" # global to set.family and used only by that function

set.family <- function(family)
{
    if(family != last.family.global) {
        par(family=family)
        unlockBinding("last.family.global", asNamespace("rpart.plot"))
        last.family.global <<- family   # note <<- not <-
    }
}
# The standard strheight doesn't vectorize properly (as I would expect anyway).
# Also it return negative values for descending ylim (is that a bug?)
# Also it doesn't have a family argument.
# Hence this work around.  Also changed arg order for convenience in this package.
# TODO report vectorization issue.

my.strheight <- function(s, cex=NULL, font=NULL, family="", units="user")
{
    n <- length(s)
    units  <- repn(units, n)
    cex    <- repn(cex, n)
    font   <- repn(font, n)
    family <- repn(family, n)
    height <- double(n)
    if(n > 0) for(i in 1:n) {
        set.family(family[i])
        height[i] <- strheight(s[i], units[i], cex[i], font[i], vfont=NULL)
    }
    abs(height)
}
my.strwidth <- function(s, cex=NULL, font=NULL, family="", units="user")
{
    n <- length(s)
    units  <- repn(units, n)
    cex    <- repn(cex, n)
    font   <- repn(font, n)
    family <- repn(family, n)
    width  <- double(n)
    if(n > 0) for(i in 1:n) {
        set.family(family[i])
        width[i] <- strwidth(s[i], units[i], cex[i], font[i], vfont=NULL)
    }
    abs(width)
}
# Remove excess zeros from exponents, so 1e-02 becomes 1e-2 (the zero prefix
# is seen only some architectures).  Also remove + sign from exponent if any.
# This function will be confused by strings with +e0 or -e0 in unexpected places,
# but works fine if you just pass it strings representing numbers.

clean.exponent <- function(string)
{
    string <- gsub("([0-9])e-0+([1-9])", "\\1e-\\2", string) # 1e-02 becomes 1e-2
    gsub("([0-9])e[+]0([1-9])", "\\1e\\2",  string)          # 1e+02 becomes 1e2
}
# format0 converts the given number (could also be a vector of
# numbers) to a string in the following manner:
# (i)  Calls format on each number individually, so no aggregation
# (ii) Strips excess zeros from exponents: 1e-02 becomes 1e-2
#
# TODO if exponents are used (numbers greater than 1e5) then it would be nice
# to print all numbers with the same exponent to facilitate comparison by eye.
# Also, would be nice to use exponents that are multiples of 3 (engineering notation).

format0 <- function(x, digits=2)
{
    stopifnot(digits >= 0)
    s <- sapply(x, format, digits=if(digits == 0) getOption("digits") else digits)
    clean.exponent(s)
}
# formatf converts the given number (could also be a vector of
# numbers) to a string in the following manner:
# (i)  Uses sprintf %.Df so fixed number of decimal places in all values in x
# (ii) If strip.leading.zeros then strips leading zeros: 0.12 becomes .12
#      (which is useful when space is tight)

formatf <- function(x, digits=2, strip.leading.zeros=FALSE)
{
    s <- sprintf("%.*f", if(digits > 0) digits else 0, x)
    if(strip.leading.zeros)
        s <- gsub("^0\\.([0-9])", ".\\1", s) # 0.12 becomes .12, -0.12 doesn't change
    s
}
# Truncate names to smallest length where all names
# are still unique, but retain at least minlen chars.

unique.substr <- function(names, minlen)
{
    stopifnot(minlen > 0)
    maxlen <- 100 # arbitrary
    if(minlen > maxlen)
        maxlen <- minlen + 1
    nbr.of.names <- length(unique(names))
    for(len in minlen:maxlen)
        if(length(unique(substr(names, 1, len))) == nbr.of.names)
            break
    if(len == maxlen) {
        warning0("could not find unique substring for \"", names[1],
                 "\" and related names")
        return(names) # NOTE: return
    }
    substr(names, 1, len)
}
# my.abbreviate does this:
#   minlen < 0  truncate names (but keep them unique, see unique.substr)
#   minlen = 0  leave names as is
#   minlen > 0  abbreviate names
# Also, if one.is.special and minlen=1 then print names using alphanumeric chars a, b, ...
# Note that this can only handle vecs (not lists etc. like abbreviate)

my.abbreviate <- function(names, minlen, one.is.special=FALSE)
{
    stopifnot(floor(minlen) == minlen) # sanity check, should be an integer
    if(minlen == 1 && one.is.special) {
        if(length(names) > 52) # 52 = 2 * 26 letters in alphabet
            stop0(deparse(substitute(minlen)),
                  "=1 but more than 52 levels: \"", names[1],
                  "\", \"", names[2], ", \"", names[3], "\" ...")
        c(letters, LETTERS)[1:length(names)]
    } else if(minlen > 0)
        abbreviate(names, minlen)
    else if(minlen < 0)
        unique.substr(names, -minlen)
    else # minlen == 0
        paste(names)
}
