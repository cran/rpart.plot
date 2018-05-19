# yes.no.R

draw.yes.no <- function(yesno, yes.text, no.text,
   type, draw.shadow,
    xflip, left, branch, xlim, ylim, node.xy, lwd,
    yesno.yshift, split.boxes, split.cex, split.box.col, split.border.col,
    split.shadow.col, split.shadow.offset,
    nn.cex, nn.font, nn.family, nn.col, nn.box.col,
    nn.border.col, nn.lty, nn.round, bg)
{
    if(is.null(nn.cex)) # auto cex?
        nn.cex <- .8 * min(split.cex)
    if(xflip == left) {
        temp <- yes.text
        yes.text <- no.text
        no.text <- temp
    }
    yes.height <- my.strheight(yes.text, nn.cex, nn.font, nn.family)
    no.height  <- my.strheight(no.text,  nn.cex, nn.font, nn.family)
    height1 <- 1.5 * max(yes.height, no.height)
    yes.width <- my.strwidth(yes.text, nn.cex, nn.font, nn.family)
    yes.width <- yes.width + 1.5 * my.strwidth("x") # extra space on sides of text
    no.width  <- my.strwidth(no.text,  nn.cex, nn.font, nn.family)
    no.width  <- no.width + 1.5 * my.strwidth("x")
    round <-  nn.round[1] * height1
    border.col <- if(is.box.invisible(split.box.col[1], split.border.col[1], bg))
                      nn.border.col
                  else
                      c(bg, bg)
    if(yesno == 2) {
        split.shadow.col <- 0
        height1 <- height1 / 1.2 # small vertical space to cram "yes" and "no" in
        yes.width <- yes.width / 1.3
        no.width  <- no.width  / 1.3
    } else if(is.invisible(border.col, bg) && is.invisible(split.shadow.col, bg)) { # no border?
        split.shadow.col <- 0    # no shadow if no border
        height1 <- height1 / 1.3 # just small space around box so branch lines not blocked
        yes.width <- yes.width / 1.3
        no.width  <- no.width  / 1.3
    }

    stopifnot(yesno == 0 || yesno == 1 || yesno == 2)
    inodes <- 1      # yesno at root only
    if(yesno == 2) { # yesno at all nodes?
        inodes <- 1:length(split.boxes$x1)
        inodes <- inodes[!is.na(split.boxes$x1)]
    }
    for(inode in inodes) {
        xleft  <- split.boxes$x1[inode] # horiz posn of left text
        xright <- split.boxes$x2[inode] # horiz posn of right text

        # get vertical position of text
        y <- if(type == TYPE.2all.under)
                .5 * (split.boxes$y1[inode] + split.boxes$y2[inode]) # center of split box
            else if(branch >= .5)   # just below center of top split box
                .5 * (split.boxes$y1[inode] + split.boxes$y2[inode]) - .4 * height1
            else                    # near the bottom of the top box
                max(split.boxes$y1[inode], node.xy$y[inode])

        y <- y + yesno.yshift * height1

        # draw the boxes

        if(draw.shadow) {
            if(!is.invisible(split.shadow.col, bg)) {
                draw.shadow(xleft - 1.06 * yes.width, y - .6 * height1,
                            xleft -  .06 * yes.width,  y + .6 * height1,
                            xlim, ylim, round,
                            split.shadow.col[1], split.shadow.offset[inode])

                draw.shadow(xright +  .05 * no.width,  y - .6 * height1,
                            xright + 1.05 * no.width, y + .6 * height1,
                            xlim, ylim, round,
                            split.shadow.col[1], split.shadow.offset[inode])
            }
        } else {
            rounded.rect(xleft - 1.06 * yes.width, y - .6 * height1,
                         xleft -  .06 * yes.width, y + .6 * height1,
                         xlim, ylim, round, nn.box.col[1], border.col[1],
                         nn.lty[1], lwd[inode])

            rounded.rect(xright +  .05 * no.width, y - .6 * height1,
                         xright + 1.05 * no.width, y + .6 * height1,
                         xlim, ylim, round, nn.box.col[1], border.col[1],
                         nn.lty[1], lwd[inode])
        }
        # draw the text
        descender.yes <- grepl("[jpqy]", yes.text)
        descender.no  <- grepl("[jpqy]", no.text)
        xcenter <- xleft - .6 * yes.width
        # The .15 height adjustment is because the driver centers the "yes" accounting for
        # the descender on "y", so must adjust to get "yes" and "no" at approx. same height.
        yadjust <- if(descender.yes) .15 else 0
        text(xcenter, y - yadjust * yes.height, yes.text,
             cex=nn.cex[1], font=nn.font[1], family=nn.family[1], col=nn.col[1])
        xcenter <- xright + .5 * no.width
        yadjust <- if(descender.no) .15 else 0
        text(xcenter, y - yadjust * no.height, no.text,
             cex=nn.cex[1], font=nn.font[1], family=nn.family, col=nn.col[1])
    }
}
