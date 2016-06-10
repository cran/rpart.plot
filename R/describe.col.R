# describe.col.R
# TODO is there a library function to do this?

COL.TAB <- NULL # global to describe.col, and used only by describe.col

describe.col <- function(col, check.palette.index=FALSE)
{
    if(is.null(COL.TAB)) { # first time? if so, must prepare COL.TAB
        colors <- colors()
        tab <- matrix(nrow=length(colors), ncol=3)
        for(i in 1:length(colors()))
            tab[i,] <- col2rgb(colors[i])
        unlockBinding("COL.TAB", asNamespace("rpart.plot"))
        COL.TAB <<- tab
        lockBinding("COL.TAB", asNamespace("rpart.plot"))
    }
    #--- describe.col starts here ---
    must.convert <- TRUE
    if(is.numeric(col)) {
        if(!all.equal(floor(col), col))
            stop0("non-integer \"col\" is illegal")
        # june 2014: changes needed for changes to col2rgb
        if(is.matrix(col)) {
            if (length(col) == 3)
                must.convert <- FALSE
            else
                stop0("bad format col")
        } else if(length(col) != 1)
            stop0("only one color is allowed")
        else if(identical(col, 0) || is.na(col)) {
            col <- par("bg")
        }
        if(check.palette.index) {
            if(col < 0)
                stop0("col ", col, " is illegal (col must be greater than or equal to 0)")
            else if(col > length(palette()))
                stop0("illegal col ", col,
                      " (only ", length(palette()), " colors in the current palette)")
        }
    }
    if(must.convert)
        col <- col2rgb(col)
    if(length(col) != 3)
        stop0("only one color is allowed")
    min <- imin <- Inf
    for(i in 1:nrow(COL.TAB)) {
        dist <- sum(abs(COL.TAB[i,] - col))
        if(dist < min) {
            min <- dist
            imin <- i
        }
    }
    match.col.name <- colors()[imin]
    paste0(rgb(col[1], col[2], col[3], maxColorValue=255),
           " (",
           ifelse(min != 0, "nearly ", ""),
           match.col.name,
           ifelse(min != 0, " ", ""),
           ifelse(min != 0,
               rgb(COL.TAB[imin,1], COL.TAB[imin,2], COL.TAB[imin,3],
                   maxColorValue=255),
               ""),
           ")")
}
