# show.prp.palettes.R:

show.palette <- function(pal.name, y, left, right, width, strheight)
{
    pal <- expand.palette(pal.name) # converts predefined palette to colors
    text(x=left, y=y+strheight/2, pal.name, adj=0)
    w <- (right - left - width) / length(pal)
    for(i in 1:length(pal))
        rect(left + width + w * (i-1), y,
             left + width + w * i,     y + .8 * strheight,
             col=pal[i], lty=0, border=NA)
}
show.how.prefix.works <- function(y, left, right, width, strheight)
{
    text(0, y + 14 * strheight,
        "Prefix the palette name with \"-\" to reverse the colors. Examples:",
        adj=0)
    left <- .05 + left
    right <- .05 + right
    y <- y + 14.5 * strheight
    yoffset <- function(yoffset) y + yoffset * 1.2 * strheight
    show.palette("Grays",  yoffset(1), left, right, width, strheight)
    show.palette("-Grays", yoffset(2), left, right, width, strheight)
    show.palette("RdBu",   yoffset(3), left, right, width, strheight)
    show.palette("-RdBu",  yoffset(4), left, right, width, strheight)
    show.palette("BuRd",   yoffset(5), left, right, width, strheight)
    text(x=right+strheight, y=yoffset(5)+.3*strheight, "same as -RdBu", adj=0)
}
show.prp.palettes <- function()
{
    cex <- .8
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
    par(mfrow=c(1,1), oma=c(0, 0, 2, 0), mar=c(0, 0, 0, 0), cex=cex)
    # invisible plot
    plot(x=0, y=0, xlim=c(0, 1), ylim=c(1, 0),
         main="prp built-in palettes\n", xpd=NA,
         col=0, bty="n", xlab="", xaxt="n", ylab="", yaxt="n")
    y <- 0 # position down the page
    strheight <- 1.5 * my.strheight("X", cex=cex)
    width <- max(my.strwidth(predefined.palette.names))
    left <- 0
    right <- .3
    for(ipal in 2:length(predefined.palette.names)) { # start at 2 to skip AUTO
        pal.name <- predefined.palette.names[ipal]
        show.palette(pal.name, y, left, right, width, strheight)
        y <- y + strheight
        if(pal.name %in% c("Purples", "Pu", "GyPu", "GnPu", "BuPu", "PuPu", "OrPu"))
            y <- y + 2 * strheight # extra vertical space
        if(pal.name == "YlGnBl") {
            show.how.prefix.works(y, left, right, width, strheight)
            # move to middle of page
            left <- .33
            right <- .63
            y <- 0
        } else if(pal.name == "BnPu") {
            # move to right side of page
            left <- .66
            right <- .99
            y <- 0
        }
    }
    invisible(NULL)
}
