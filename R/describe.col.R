# describe.col.R
#
# Note: describe.col(0) or describe.col(NA) will start the graphics window.
#       if a graphics device is not already initiated.
#
# TODO is there an existing library function to do this?
# TODO consider getting rid of check.palette.index

COLORTAB <- NULL # global to describe.col, and used only by describe.col

describe.col <- function(col, show.hex=TRUE, check.palette.index=TRUE)
{
    init.COLORTAB()
    must.convert <- TRUE
    if(is.numeric(col)) {
        if(!isTRUE(all.equal(floor(col), col)))
            stop0("non-integer col is illegal")
        # june 2014: changes needed for changes to col2rgb
        if(is.matrix(col)) {
            if(length(col) == 3)
                must.convert <- FALSE
            else
                stop0("bad format col")
        } else if(length(col) != 1)
            stop0("only one color is allowed")
        else if(check.palette.index) {
            if(col < 0)
                stop0("col ", col,
                      " is illegal (col must be greater than or equal to 0)")
            else if(col > length(palette()))
                stop0("illegal col ", col,
                      " (only ", length(palette()),
                      " colors in the current palette)")
        }
        if(is.zero(col)[1] || is.na(col)[1])
            col <- par("bg")
    }
    if(must.convert)
        col <- col2rgb(col)
    if(length(col) != 3)
        stop0("only one color is allowed")
    ret <- get.closest.col(col)
        icol <- ret$icol # index of closest col in COLORTAB
        dist <- ret$dist # distance to closest col
    closest.col.name <- colors()[icol]
    paste0(rgb(col[1], col[2], col[3], maxColorValue=255),
           " (",
           ifelse(dist != 0, "near ", ""),
           closest.col.name,
           ifelse(show.hex && dist != 0, " ", ""),
           ifelse(show.hex && dist != 0,
               rgb(COLORTAB[icol,1], COLORTAB[icol,2], COLORTAB[icol,3],
                   maxColorValue=255),
               ""),
           ")")
}
init.COLORTAB <- function()
{
    if(is.null(COLORTAB)) {
        colors <- colors()
        tab <- matrix(nrow=length(colors), ncol=3)
        for(i in 1:length(colors()))
            tab[i,] <- col2rgb(colors[i])
        unlockBinding("COLORTAB", asNamespace("rpart.plot"))
        COLORTAB <<- tab
        lockBinding("COLORTAB", asNamespace("rpart.plot"))
    }
}
get.closest.col <- function(col)
{
    # TODO following should probably use HSV distance but this is more intuitive
    dist <- icol <- Inf
    for(i in 1:nrow(COLORTAB)) {
        this.dist <- sum(abs(COLORTAB[i,] - col))
        if(this.dist < dist) {
            dist <- this.dist
            icol <- i
        }
    }
    list(icol=icol, dist=dist)
}
