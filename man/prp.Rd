\name{prp}
\alias{prp}
\title{Plot an rpart model.}
\description{
Plot an \code{\link{rpart}} model.

First-time users should use \code{\link{rpart.plot}} instead,
which provides a simplified interface to this function.

For an overview, please see the package vignette
\href{../doc/prp.pdf}{Plotting rpart trees with the rpart.plot package}.

The arguments of this function are a superset of those of
\code{\link{rpart.plot}} and some of the arguments have different
defaults.
In detail the different defaults are:

\tabular{llclcr}{
                     \tab | \tab \code{\link{rpart.plot}} \tab | \tab \code{\link{prp}} \tab | \cr
                     \tab   \tab               \tab   \tab              \tab   \cr
\code{type}          \tab | \tab \code{2}      \tab | \tab \code{0}     \tab | \cr
\code{extra}         \tab | \tab \code{"auto"} \tab | \tab \code{0}     \tab | \cr
\code{fallen.leaves} \tab | \tab \code{TRUE}   \tab | \tab \code{FALSE} \tab | \cr
\code{varlen}        \tab | \tab \code{0}      \tab | \tab \code{-8}    \tab | \cr
\code{faclen}        \tab | \tab \code{0}      \tab | \tab \code{3}     \tab | \cr
\code{box.palette}   \tab | \tab \code{"auto"} \tab | \tab \code{0}     \tab | \cr
}

The defaults are different for historical reasons: for backwards
compatibility the defaults of \code{\link{prp}} haven't changed,
whereas the defaults of \code{\link{rpart.plot}} were changed when
\code{type="auto"} and \code{box.palette} were introduced
in version 2.0.0 of this package.
}
\usage{
prp(x=stop("no 'x' arg"),
    type=0, extra=0, under=FALSE, fallen.leaves=FALSE,
    nn=FALSE, ni=FALSE, yesno=TRUE,
    branch=if(fallen.leaves) 1 else .2,
    uniform=TRUE, left=TRUE, xflip=FALSE, yflip=FALSE,
    digits=2, varlen=-8, faclen=3, roundint=TRUE,
    cex=NULL, tweak=1,
    clip.facs=FALSE, clip.right.labs=TRUE,
    compress=TRUE, ycompress=uniform,
    Margin=0, space=1, gap=NULL,
    snip=FALSE, snip.fun=NULL, trace=FALSE,

    box.col=0, box.palette=0,
    pal.thresh=NULL, pal.node.fun=FALSE,
    border.col=col,
    round=NULL, leaf.round=NULL,
    shadow.col=0, prefix="", suffix="", xsep=NULL,

    under.percent=2, under.font=font, under.col=1, under.cex=.8,

    split.cex=1, split.font=2, split.family=family, split.col=1,
    split.box.col=0, split.border.col=0,
    split.lty=1, split.lwd=NULL, split.round=0,
    split.shadow.col=0,
    split.prefix="", right.split.prefix=NULL,
    split.suffix="", right.split.suffix=NULL,
    facsep=",", eq=" = ", lt=" < ", ge=" >= ",

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
    add.labs=TRUE, clip.left.labs=(type == 5), fam.main="",
    yshift=0, yspace=space, shadow.offset=.4,
    split.adj=NULL, split.yshift=0, split.space=space,
    split.yspace=yspace, split.shadow.offset=shadow.offset,
    nn.adj=.5, nn.yshift=0, nn.space=.8, nn.yspace=.5,
    ygap=gap/2, under.ygap=.5, yesno.yshift=0,
    xcompact=TRUE, ycompact=uniform, xcompact.ratio=.8, min.inter.height=4,
    max.auto.cex=1, min.auto.cex=.15, ycompress.cex=.7, accept.cex=1.1,
    shift.amounts=c(1.5, 2),
    Fallen.yspace=.1, boxes.include.gap=FALSE,
    legend.x=NULL, legend.y=NULL, legend.cex=1,
    ...)
}
\arguments{
  \item{x}{
     An \code{\link{rpart}} object.  The only required argument.
  }
  \item{type}{
    Type of plot. Possible values:

     \bold{0} Default. Draw a split label at each split
              and a node label at each leaf.

     \bold{1} Label all nodes, not just leaves.
              Similar to \code{text.rpart}'s \code{all=TRUE}.

     \bold{2} Like \code{1} but draw the split labels below the node labels.
              Similar to the plots in the CART book.

     \bold{3} Draw separate split labels for the left and right directions.

     \bold{4} Like \code{3} but label all nodes, not just leaves.
              Similar to \code{text.rpart}'s \code{fancy=TRUE}.
              See also \code{clip.right.labs}.\cr

     \bold{5} Show the split variable name in the interior nodes.\cr
  }
  \item{extra}{
     Display extra information at the nodes. Possible values:

     \bold{"auto"} (case insensitive)\cr
Automatically select a value based on the model type, as follows:\cr
\code{extra=106} class model with a binary response\cr
\code{extra=104} class model with a response having more than two levels\cr
\code{extra=100} other models\cr

     \bold{0} Default. No extra information.

     \bold{1} Display the number of observations that fall in the node
              (per class for \code{class} objects;
              prefixed by the number of events for \code{poisson} and \code{exp} models).
              Similar to \code{text.rpart}'s \code{use.n=TRUE}.

     \bold{2} Class models: display the classification rate at the node,
              expressed as the number of correct classifications and the number
              of observations in the node.\cr
              Poisson and exp models: display the number of events.

     \bold{3} Class models: misclassification rate at the node,
              expressed as the number of incorrect classifications and the number
              of observations in the node.

     \bold{4} Class models:
              probability per class of observations in the node
              (conditioned on the node, sum across a node is 1).

     \bold{5} Class models:
              like \code{4} but don't display the fitted class.

     \bold{6} Class models:
              the probability of the second class only.
              Useful for binary responses.

     \bold{7} Class models:
              like \code{6} but don't display the fitted class.

     \bold{8} Class models:
              the probability of the fitted class.

     \bold{9} Class models:
              The probability relative to \emph{all} observations --
              the sum of these probabilities across all leaves is 1.
              This is in contrast to the options above, which give the probability
              relative to observations falling \emph{in the node} --
              the sum of the probabilities across the node is 1.

     \bold{10} Class models:
              Like \code{9} but display the probability of the second class only.
              Useful for binary responses.\cr

     \bold{11} Class models:
              Like \code{10} but don't display the fitted class.\cr

     \bold{+100} Add \code{100} to any of the above to also display
                 the percentage of observations in the node.
                 For example \code{extra=101} displays the number
                 and percentage of observations in the node.
                 Actually, it's a weighted percentage
                 using the \code{weights} passed to \code{rpart}.\cr

    Note: Unlike \code{text.rpart},
    by default \code{prp} uses its own routine for
    generating node  labels (not the function attached to the object).
    See the \code{node.fun} argument.\cr
  }
  \item{under}{
     Applies only if \code{extra > 0}.
     Default \code{FALSE}, meaning put the extra text \emph{in} the box.
     Use \code{TRUE} to put the text \emph{under} the box.
     See also \code{under.cex}.
  }
  \item{fallen.leaves}{
     Default \code{FALSE}.
     If \code{TRUE}, position the leaf nodes at the bottom of the graph.
  }
  \item{nn}{
     Display the node numbers.
     Default \code{FALSE}.
     (In the current implementation
     some overplotting may occur with \code{nn=TRUE}.)
  }
  \item{ni}{
     Display the node indices,
     i.e. the row numbers of the nodes in the object's \code{\link[=rpart.object]{frame}}.
     Default \code{FALSE}.
  }
  \item{yesno}{
     One of\cr
     \bold{0} don't write \code{yes} and \code{no} on the tree
     \cr
     \bold{1} (default) write \code{yes} and \code{no} at the top split
     \cr
     \bold{2} write \code{yes} and \code{no} at all splits.  Seems to work best with
              \code{fallen.leaves=TRUE, split.border.col=1}.
     \cr
     (The \code{yesno} argument is ignored if \code{type=3} or \code{4}.
     Use \code{nn.col} and the other \code{nn} parameters
     to change the color etc. of the \code{yes} and \code{no} text.
     Use \code{yes.text} and \code{no.text} to change the actual text displayed.)
  }
  \item{branch}{
    Controls the shape of the branch lines.
     Specify a value between \code{0} (V shaped branches) and
     \code{1} (square shouldered branches).\cr
     Default is \code{if(fallen.leaves) 1 else .2}.
  }
  \item{uniform}{
    If \code{TRUE} (the default), the vertical spacing of the nodes is uniform.
    If \code{FALSE}, the nodes are spaced proportionally to the fit
    (more precisely, to the difference between a node's deviance and the
    sum of its two children's deviances).
    Very small vertical spaces are automatically artificially expanded
    to make room for the labels, see \code{minbranch}.
    Note: \code{uniform=FALSE} with \code{cex=NULL} (the default)
    can sometimes cause very small text.
  }
  \item{left}{
     Default \code{TRUE}, meaning the left side of a split is the path
     taken if the split condition is true.
     With \code{left=FALSE} the split labels are changed so the right side is true.
  }
  \item{xflip}{
     Default \code{FALSE}. If \code{TRUE}, flip the tree horizontally.
  }
  \item{yflip}{
     Default \code{FALSE}. If \code{TRUE}, flip the tree vertically, so the root
     is at the bottom.
  }
  \item{digits}{
     The number of significant digits in displayed numbers.
     Default \code{2}.\cr
     If \code{0}, use \code{\link{getOption}("digits")}.\cr
     If negative, use the standard \code{\link{format}} function
     (with the absolute value of \code{digits}).\cr\cr
     When \code{digits} is positive, the following details apply:\cr
     Numbers from \code{0.001} to \code{9999} are printed without an exponent
     (and the number of digits is actually only a suggestion,
     see \code{\link{format}} for details).
     Numbers out that range are printed with an ``engineering'' exponent (a multiple of 3).\cr
  }
  \item{varlen}{
     Length of variable names in text at the splits
     (and, for class responses, the class in the node label).
     Default \code{-8}, meaning truncate to eight characters.
     Possible values:\cr

     \bold{0} use full names.\cr

     \bold{greater than 0} call \code{\link{abbreviate}} with the given \code{varlen}.\cr

     \bold{less than 0} truncate variable names to the shortest length where they are still unique,
     but never truncate to shorter than \code{abs(varlen)}.
  }
  \item{faclen}{
     Length of factor level names in splits.
     Default \code{3}, meaning \code{\link{abbreviate}} to three characters.
     Possible values are as \code{varlen} above, except that
     for back-compatibility with \code{\link{text.rpart}} the special value \code{1}
     means represent the factor levels with alphabetic characters
     (\code{a} for the first level, \code{b} for the second, etc.).
  }
  \item{roundint}{
     If \code{roundint=TRUE} (default) and all values of a predictor in the
     training data are integers, then splits for that predictor
     are rounded to integer.
     For example, display \code{nsiblings < 3} instead of \code{nsiblings < 2.5}.\cr
     If \code{roundint=TRUE} and the data used to build the model is no longer
     available, a warning will be issued.\cr
     Using \code{roundint=FALSE} is advised if non-integer values are in fact possible
     for a predictor, even though all values in the training data for that
     predictor are integral.
  }
  \item{cex}{
     Default \code{NULL}, meaning calculate the text size automatically.\cr
     Since font sizes are discrete, the \code{cex} you ask for
     may not be exactly the \code{cex} you get.
  }
  \item{tweak}{
     Adjust the (possibly automatically calculated) \code{cex}.
     Using \code{tweak} is often easier than specifying \code{cex}.\cr
     The default \code{tweak} is \code{1}, meaning no adjustment.\cr
     Use say \code{tweak=1.2} to make the text 20\% larger.\cr
     Since font sizes are discrete,
     a small change to tweak may not actually change the type size,
     or change it more than you want.
  }
\item{clip.facs}{
Default \code{FALSE}.
If \code{TRUE}, print splits on factors as \code{female} instead of
\code{sex = female}; the variable name and equals is dropped.
\cr
Another example: print \code{survived} or \code{died} rather than
\code{survived = survived} or \code{survived = died}.\cr
}
  \item{clip.right.labs}{
     Applies only if \code{type=3} or \code{4}.\cr
     Default is \code{TRUE} meaning ``clip'' the right-hand split labels,
     i.e., don't print \code{variable=}.\cr
     See also \code{clip.left.labs}.
  }
  \item{compress}{
     If \code{TRUE} (the default), make more space
     by shifting nodes horizontally where space is available.
     This often allows larger text.
     (This is the same as \code{plot.rpart}'s argument of the same name,
     except that here the default is \code{TRUE}.)
  }
  \item{ycompress}{
     If \code{TRUE} (the default unless \code{uniform=FALSE}),
     make more space by shifting labels vertically where space is available.
     Actually, this only kicks in
     if the initial automatically calculated \code{cex} is less than \code{0.7}.
     Use \code{ycompress=FALSE} if you feel the resulting display is too messy.
     In the current implementation, the shifting algorithm
     works a little better (allowing larger text)
     with \code{type=1}, \code{2}, or \code{3}. % (i.e. not "fancy").
  }
  \item{Margin}{
     Extra white space around the tree, as a fraction of the graph width.
     Default \code{0}, meaning no extra space.
     To add say 10\% space around the tree use \code{Margin=0.1}.
     (This is the \code{margin} argument of \code{plot.rpart}.
     The name was changed to prevent partial matching with \code{mar},
     which can be passed in as a \dots argument.)
  }
  \item{space}{
     Horizontal space to the box border on each side of the node label text,
     in character widths.
     Default \code{1}.
     Use this (and \code{yspace}) for bigger boxes.
     Since this affects the size of the (possibly invisible) boxes,
     it also affects the graph layout and hence also the automatic calculation of \code{cex}.
  }
  \item{gap}{
     Minimum horizontal gap between the (possibly invisible) boxes, in character widths.
     Default \code{NULL}, meaning automatically choose a suitable value
     (normally \code{1}, but if the graph is very crowded will be set to \code{0},
     permitting boxes to touch to allow a bigger \code{cex}).
     See also \code{space}.
  }
  \item{snip}{
     Default \code{FALSE}.
     Set \code{TRUE} to interactively trim the tree with the mouse.
     See the \href{../doc/prp.pdf}{package vignette} (or just try it).
  }
  \item{snip.fun}{
     Function invoked after each mouse click when \code{snip=TRUE}.
     Default \code{NULL}, meaning no function.
     Otherwise set \code{snip.fun} to your own function
     with the prototype \code{function(tree)}, where \code{tree} is the snipped tree.
     See the \href{../doc/prp.pdf}{package vignette} for an example.

     \bold{The following control the node labels.}
  }
  \item{trace}{
     Default \code{FALSE}.
     Use \code{TRUE} to print the automatically calculated \code{cex}, \code{xlim}, and \code{ylim}.
     Use integer values greater than 1 for more detailed tracing.\cr
  }
  \item{box.col}{
     Color of the boxes around the text.
     Default \code{0}, meaning use the background color.
     If this argument is used, the \code{box.palette} argument is ignored.
  }
  \item{box.palette}{
Palette for coloring the node boxes based on the fitted value.
This is a vector of \code{\link{colors}},
for example \code{box.palette=c("green", "green2", "green4")}.
Small fitted values are displayed with colors at the start of the vector;
large values with colors at the end.
Quantiles are used to partition the fitted values.

The special value \code{box.palette=0} (default for \code{prp}) uses
the background color (typically white).

The special value \code{box.palette="auto"} (default for
\code{rpart.plot}, case insensitive) automatically selects a
predefined palette based on the type of model.

Otherwise specify a \bold{predefined palette}
e.g. \code{box.palette="Grays"} for the predefined gray palette (a range of grays).
The predefined palettes are (see the \code{\link{show.prp.palettes}} function):\cr
\code{Grays} \code{Greys} \code{Greens} \code{Blues} \code{Browns} \code{Oranges} \code{Reds} \code{Purples} \cr
\code{Gy} \code{Gn} \code{Bu} \code{Bn} \code{Or} \code{Rd} \code{Pu} (alternative names for the above palettes)\cr
\code{BuGn} \code{GnRd} \code{BuOr} etc. (two-color diverging palettes: any combination of two of the above palettes)\cr
\code{RdYlGn} \code{GnYlRd} \code{BlGnYl} \code{YlGnBl} (three color palettes)

Prefix the palette name with \code{"-"} to reverse the order of the colors\cr
e.g. \code{box.palette="-auto"} or \code{box.palette="-Grays"}.

The \code{box.palette} argument is ignored if the \code{box.col} palette argument is specified.
}
  \item{pal.thresh}{
Applies when \code{box.palette} is a
two-color diverging palette (such as \code{BuGn}).
Specifies the response threshold to split the two sub-palettes (such as
\code{Bu} and \code{Gn}).
\cr
For example, to display fitted values less than 90 in shades of blue
and values greater than 90 in shades of green, use
\code{pal.thresh=90} with \code{box.palette="BuGn"}.
\cr
By default \code{pal.thresh} is calculated automatically.
(For a two-class response the default threshold is \code{0.5}; for
a continuous response the default is the median fitted value.)
\cr
% (To display fitted values less than 90 all as lightblue
% and values greater than 90 all as lightgreen, use
% \code{box.palette=c("lightblue", "lightgreen"} and \code{pal.thresh=90}.)
Node boxes for fitted values less than the threshold are displayed using
colors from the first sub-palette;
boxes for fitted values greater than the threshold are displayed using
colors from the second sub-palette.
\cr
This argument is ignored if \code{box.palette} isn't a two-color diverging palette,
and is ignored for models with multiple-class responses (more than two classes).
  }
  \item{pal.node.fun}{
Specifies how the \code{box.palette} argument is handled when
the \code{node.fun} argument is specified.
\cr
Default is \code{FALSE}, meaning ignore \code{node.fun} and  use
the fitted value to select the node color from \code{box.palette} as usual.
\cr
If \code{TRUE}, use the label returned by the node
function (instead of the fitted value) to select the color.
The first number in each label returned by the node function is used,
skipping over any non-numeric initial text in the label.
An error message will be issued if a label doesn't include a number.
\cr
This argument is ignored if \code{node.fun} isn't specified,
and is ignored for models with multiple-class responses (more than two
classes).
  }
  \item{border.col}{
     Color of the box border around the text.
     Default \code{col}, the color of the text in the box.
     Use \code{0} for no border.
     (Note: \code{par} settings like \code{col} can be passed in as \dots arguments.
     If not passed in, \code{par("col")} is used.)
  }
  \item{round}{
     Controls the rounding of the corners of the node boxes.
     Default \code{NULL}, meaning calculate automatically.
     Else specify \code{0} for sharp edges, and values greater
     than \code{0} for rounded edges.
     Bigger is more round.
     Values too big for the size of the box get silently reduced.
  }
  \item{leaf.round}{
     Controls the rounding of the corners of the leaf node boxes.
     Default \code{NULL}, meaning use \code{round}.
     Else specify a value greater than or equal to \code{0}.
  }
  \item{shadow.col}{
     Color of the shadow under the boxes.
     Default \code{0}, no shadow.
     Try \code{"gray"} or \code{"darkgray"}.
     (Note: overlapping shadows look better on devices that support \code{\link[=rgb]{alpha}} channels.
     If you get the message "Warning: semi-transparency is not supported" please let me know --
     it means that a fix is needed to the code that determines if the device supports alpha channels.)
  }
  \item{prefix}{
     Default \code{""}.
     Prepend this string to the node labels.
     So could be the name of the fitted response, for instance.
  }
  \item{suffix}{
     Default \code{""}.
     Append this string to the node labels.
     Text after a double newline \code{"\\n\\n"} (if any) will be plotted \emph{under} the box.
     (Actually, double newlines can be used in any of the prefix or suffix
     arguments for this purpose.)
  }
  \item{xsep}{
     String which separates the individual counts and probabilities
     in node labels when \code{extra>0}.
     Default NULL meaning automatically select: usually\cr
     \code{"  "} (two spaces), but \code{" / "} for rates.
     Use \code{xsep="/"} for compatibility with \code{text.rpart}.
     See also \code{facsep}, which separates the factor levels in split labels.\cr
  }
  \item{under.percent}{
    Control whitespace before the percentage (when 100 is used with the \code{extra} argument).
     One of\cr
     \bold{0} put a space before the percentage
     \cr
     \bold{1} put a newline before the percentage
     \cr
     \bold{2} (default) automatically choose a space or newline before the percentage.
     \cr

     \bold{The following control the text under the boxes} (apply only if
     \code{under=TRUE} or there is a double newline \code{\\n\\n} in \code{prefix} or \code{suffix}).
  }
  \item{under.font}{
    Font of the text under the box. Default \code{font} (which can be passed in as a \dots argument).
  }
  \item{under.col}{
    Color of the text under the box.
    Default \code{1}.
  }
  \item{under.cex}{
    Size of the text under the box relative to the text in the box.
    Default \code{.8}, smaller than the text in the box.\cr

    \bold{The following control the split labels.}
  }
  \item{split.cex}{
     Size of the split text relative to \code{cex}
     (which by default is calculated automatically).
     Default \code{1}.
  }
  \item{split.font}{
     Font for the split labels.
     Default \code{2}, bold.
     (Note: use \code{font} to change the \emph{node} label text.)
  }
  \item{split.family}{
     Font family for the split labels.\cr
     Default \code{""},
     or use something like \code{split.family="serif"}.
     (Note: use \code{family} to change the \emph{node} label text.)
  }
  \item{split.col}{
     Color of the split label text.
     Default \code{1}.
     (Note: use \code{col} to change the \emph{node} label text.)
  }
  \item{split.box.col}{
     Color of the split boxes.
     Default \code{0}, meaning use the background color.
  }
  \item{split.border.col}{
     Color of the split box borders.
     Default \code{0}, invisible.
  }
  \item{split.lty}{
     Line type for the split box borders.
     The default is \code{1}, but the border will be invisible
     unless you change the default \code{split.border.col}.
     (Note: use \code{lty} to change the \emph{node} box borders.)
  }
  \item{split.lwd}{
     Line width of the split box border relative to \code{cex}
     (which by default is calculated automatically).
     The border is by default invisible, see \code{split.border.col}.
  }
  \item{split.round}{
     Controls the rounding of the corners of the split boxes.
     Default \code{0}, meaning sharp corners.
     Else specify a value greater than or equal to \code{0}.
     The split boxes are by default invisible,
     see \code{split.box.col} and \code{split.border.col}).
  }
  \item{split.shadow.col}{
     Color of the shadow under the split boxes.
     Default \code{0}, no shadow.
  }
  \item{split.prefix}{
     Default \code{""}.
     Prepend this string to the split labels.
  }
  \item{right.split.prefix}{
     Default \code{split.prefix}.
     Prepend this string to the right split labels.
     Applies only when \code{type=3} or \code{4}.
  }
  \item{split.suffix}{
     Default \code{""}.
     Append this string to the split labels.
  }
  \item{right.split.suffix}{
     Default \code{split.suffix}.
     Append this string to the right split labels.
     Applies only when \code{type=3} or \code{4}.
  }
  \item{facsep}{
     Default \code{","}.
     String which separates the factor levels in split labels.
     See also \code{xsep}, which separates the individual counts when \code{extra} is used.
  }
  \item{eq}{
     Default \code{" = "}.
     String which separates the factor name from the levels in split labels.
     The idea is that you can add or remove spaces around the \code{=}, or use
     words if that suits you.
  }
  \item{lt}{
     Default \code{" < "}.
     String which represents ``less than'' in split labels.
  }
  \item{ge}{
     Default \code{" >= "}.
     String which represents ``greater than or equal'' in split labels.\cr

     \bold{The following control the branches.}
  }
  \item{branch.col}{
     Color of the branch lines.
     Default \code{1}, but set to \code{"gray"} if \code{branch.type} is nonzero.
  }
  \item{branch.lty}{
     Branch line type.
     Default \code{1}.
  }
  \item{branch.lwd}{
     Line width of the branch lines relative to \code{cex}
     (which by default is calculated automatically).
     (Note: \code{branch.lwd} does not control the width of the
     ``wide branches'' drawn when \code{branch.type} is nonzero.)
  }
  \item{branch.type}{
         Default \code{0}.
     If nonzero draw ``wide branches'', with branch widths proportional
     to the parameter selected by \code{branch.type} as follows:

     \bold{0} The default.  The branch lines are drawn conventionally.\cr
     \bold{1} deviance\cr
     \bold{2} sqrt(deviance)\cr
     \bold{3} deviance / nobs\cr
     \bold{4} sqrt(deviance / nobs) (the standard deviation when \code{method="anova"})\cr
     \bold{5} weight (\code{frame$wt}).
              This is the number of observations at the node,
              unless \code{rpart}'s \code{weight} argument was used.\cr
     \bold{6} complexity\cr
     \bold{7} abs(predicted value)\cr
     \bold{8} predicted value - min(predicted value)\cr
     \bold{9} constant (for checking visual perception of the
              relative width of branches).\cr

     Otherwise set \code{branch.type} to your own function.
     The function should take a single argument \code{x}
     (the \code{rpart} object) and return
     a numeric vector of non-negative widths corresponding to rows in \code{frame}.
     See \code{get.branch.widths} in the source code.

     Note: with a nonzero \code{branch.type}, in the current implementation
         the \code{branch} argument will be silently changed to \code{1}
         (if \code{branch > .5}) or \code{0} (if \code{branch < .5})\cr
  }
  \item{branch.tweak}{
     Default \code{1}.
     Applies only if \code{branch.type} is nonzero.
     Use this argument to scale the widths of the branches,
     for example, \code{branch.tweak=.5} to halve the width of the branches.
     (By default, \code{prp} normalizes the widths so the
     widest branch is one-fifth the plot width.)
  }
  \item{min.branch.width}{
    Default \code{0.002}.
    Applies only if \code{branch.type} is nonzero.
    The minimum width of a branch, as a fraction of the page width.
    The width of branches that would be thinner than \code{min.branch.width} is clamped.
    Increase \code{min.branch.width} if the thinnest branches are too skinny on your display device.
  }
  \item{branch.fill}{
     Color used to fill the wide branch lines.
     Applies only if \code{branch.type} is nonzero.
         Default \code{branch.col}.\cr

     \bold{The following control the node numbers (with \code{nn=TRUE)}.}
  }
  \item{nn.cex}{
     Default \code{NULL}, meaning calculate the \code{cex}
     of the node numbers automatically.
     This and the following arguments apply only when \code{nn=TRUE}.
  }
  \item{nn.font}{
     Font for the node numbers.
     Default \code{3}, italic.
  }
  \item{nn.family}{
     Font family for the node numbers.
     Default \code{""}.
  }
  \item{nn.col}{
     Color of the node number text.
     Default \code{1}.
  }
  \item{nn.box.col}{
     Color of the boxes around the node numbers.
     Default \code{0}, meaning use the background color.
  }
  \item{nn.border.col}{
     Color of the box border around the node numbers.
     Default \code{nn.col}.
  }
  \item{nn.lty}{
     Line type of the node number box border.
     Default \code{1}.
  }
  \item{nn.lwd}{
     Line width of the node box border relative to \code{cex}
     (which by default is calculated automatically).
     Default \code{NULL}, meaning use \code{lwd} (which can be passed in as a \dots argument).
  }
  \item{nn.round}{
     Controls the rounding of the corners of the node number boxes.
     Default \code{.3}, meaning small corners.
     Else specify a value greater than or equal to \code{0}.\cr
  }
  \item{yes.text, no.text}{
     Text displayed when \code{yesno=TRUE}.
     Default \code{yes.text="yes"} and \code{no.text="no"}.

     \bold{The following are user definable functions.}
  }
  \item{node.fun}{
     The function that generates the text at the node labels.
     The default is \code{NULL},
     which means use a default function internal to \code{prp}.
     (This is necessary for full support of \code{extra} as described
     in the section on \code{extra} above.)\cr
     Otherwise set \code{node.fun} to your own function
     with the prototype\cr
     \code{function(x, labs, digits, varlen)}\cr
     See the \href{../doc/prp.pdf}{package vignette} for details.
     See also the \code{pal.node.fun} argument.
  }
  \item{split.fun}{
     The function that generates the text at the splits.
     The default is \code{NULL},
     which means use a default function internal to \code{prp}.
     Otherwise set \code{split.fun} to your own function with the
     prototype \code{function(x, labs, digits, varlen, faclen)}
  }
  \item{FUN}{
     The function that displays the text on the screen.
     Default \code{\link{text}}.\cr

     \bold{The following are esoteric parameters, mostly for the graph layout engine.}
  }
  \item{nspace}{
     Applies only when \code{compress=TRUE}.
     Default \code{nspace=branch}.
     The size of the space between a split and a leaf, relative to
     the space between leaves.
  }
  \item{minbranch}{
      Applies only when \code{uniform=FALSE}.  Default \code{.3}.
      The minimum height between levels is clamped at
      \code{minbranch} times the mean interlevel distance.
      Needed because sometimes a split gives little or no improvement in deviance, and
      an interlevel distance strictly proportional to the improvement would
      leave no room for the label.
  }
  \item{do.par}{
     Default \code{TRUE}, meaning adjust the \code{mar} parameter
     so the tree fills the figure region.
     This also sets \code{xpd=NA}.
     These graphic parameters are restored to their original
     state before \code{prp} exits.
     If you explicitly set \code{mar} or \code{xpd}, \code{prp} will
     use your setting regardless of the setting of \code{do.par}.
  }
  \item{add.labs}{
     Default \code{TRUE}, meaning display the labels.
     If \code{FALSE}, gives a bare bones display similar to \code{plot.rpart}.
  }
  \item{clip.left.labs}{
     Like \code{clip.right.labs} but for the left labels.
     Default is \code{FALSE}.
     Note that \code{clip.left.labs} and \code{clip.right.labs} can be vectors,
     indexed on the split number.
  }
  \item{fam.main}{
     Font family for the main text.  Default \code{""}.
     The (inconsistent) name was chosen to minimize partial matching
     with \code{main} and \code{family} which can be passed in as
     in as \dots arguments.
  }
  \item{yshift}{
     Vertical position of the labels, in character heights relative to their default position.
     Default \code{0}.
     Negative values move the text down; positive up
     (the box around the text will follow along).
  }
  \item{yspace}{
     Vertical space to the box border above and below the node label text, in character heights.
     Default \code{space}.
     See the comments for \code{space}.
  }
  \item{shadow.offset}{
     Offset of the shadow from the boxes, in character widths.
     Default \code{.4}
     (but the shadow will be invisible unless the default \code{shadow.col} is changed).
  }
  \item{split.adj}{
     Horizontal position of the split text.
     In string width units, as is the convention for \code{adj} arguments.
     Default \code{NULL}, meaning use \code{adj}
     (which defaults to \code{0.5} but can be passed in as a \dots argument).
     Use values less/more than \code{.5} to shift the text left/right
     (the box around the text will follow along).
  }
  \item{split.yshift}{
     Vertical position of the split labels,
     in character heights relative to their default positions.
     Default \code{0}.
     Negative values move the text down; positive up
     (the box around the text will follow along).
     This adjusts the positions of the split labels relative to the node labels.
     (Use \code{yshift} if you want to shift \emph{both} the split and node labels.)
  }
  \item{split.space}{
     Horizontal space between the split label text and the box,
     in character widths.
     Default \code{space}.
     Affects the size of the box drawn around the text.
     The split boxes are by default invisible
     (see \code{split.box.col} and \code{split.border.col}),
     but nevertheless affect the graph layout used
     in the automatic calculation of \code{cex}.
  }
  \item{split.yspace}{
     Vertical space between the split label text and the box, in character heights.
     Default \code{yspace}.
  }
  \item{split.shadow.offset}{
     Offset of the shadow from the split boxes, in character widths.
     Default \code{shadow.offset}.
     (but the shadow will be invisible unless the default \code{shadow.col} is changed).
  }
  \item{nn.adj}{
     Horizontal position of the node label text.
     Default \code{.5}.
  }
  \item{nn.yshift}{
     Vertical position of the node numbers, in character heights relative to their default positions.
     Default \code{0}.
  }
  \item{nn.space}{
     Horizontal space to the box border on each side of the node number text,
     in character widths.
     Default \code{.8}.
  }
  \item{nn.yspace}{
     Vertical space to the box border above and below the node number text, in character heights.
     Default \code{.5}.
  }
%  \item{auto.gap.cex}{
%    Default \code{.3}.
%    When \code{cex} and \code{gap} are automatically calculated (the default),
%    uses \code{gap=0} if the initial automatically
%    calculated \code{cex} is less than \code{auto.gap.cex}.
%  }
  \item{under.ygap}{
    Applies if text is plotted under the box
    (i.e. if \code{under=TRUE} or there is a double newline in \code{prefix} or \code{suffix}).
    Vertical gap (in char heights) between the lower edge of the box and
    the top of the text under the box.
  }
  \item{yesno.yshift}{
     Vertical position of "yes" and "no"
     in character heights relative to their default position.
     Default \code{0}.
     Applies only when \code{yesno=TRUE}.
  }
  \item{ygap}{
     Minimum vertical gap between boxes, in character heights.
     Default \code{gap/2}.
  }
  \item{xcompact}{
     If \code{TRUE} (the default) and there is too much white space,
     automatically change \code{xlim} to compact the entire tree horizontally.
     This usually only activates for small trees.
     (The \code{xcompact} and \code{ycompact} arguments compact the tree as a whole,
     whereas the \code{compress} and \code{ycompress} arguments move parts of the tree
     into available space.)
  }
  \item{ycompact}{
     If \code{TRUE} (the default) and there is too much vertical space,
     automatically change \code{ylim} to compact the entire tree vertically.
  }
  \item{xcompact.ratio}{
    Default \code{.8}.
    Applies only when \code{xcompact=TRUE}.
    The maximum possible without overplotting is \code{1},
    but compacting by \code{.8} usually
    gives more pleasing spacing (it gives more space).
  }
  \item{min.inter.height}{
    Default \code{4}.
    Applies only when \code{ycompact=TRUE}.
    Minimum height (in units of character height)
    between the lowest label in a layer and the highest label in the layer below it.
  }
  \item{max.auto.cex}{
    Clamp the maximum automatically calculated \code{cex} at this value
    Default \code{1}, meaning never expand \code{cex}, only contract when necessary.
  }
  \item{min.auto.cex}{
    Default \code{.15}.
    Never downscale to less than this when automatically calculating \code{cex},
    even if overplotted labels result.
    (The graph layout algorithm is unstable with \code{cex}'s below \code{0.15}
    -- meaning that the automatic type size may be smaller than necessary.)
  }
  \item{ycompress.cex}{
    Default \code{.7}.
    Applies only when \code{ycompress=TRUE}.
    Apply the \code{ycompress} algorithm if the initial automatically calculated \code{cex} is less than this.
    The idea is that we don't want to shift if we get an acceptable cex without shifting.
    Make \code{Inf} to always attempt shifting.
  }
  \item{accept.cex}{
    Accept shifting only if it causes at least this much improvement in \code{cex}
    (because we don't want to shift if it gives only a small improvement in cex).
    Default \code{1.1} i.e. require at least a 10\% improvement.
    Use \code{0} to always accept shifts and
    \code{Inf} to never accept (or use \code{ycompress=FALSE}).
  }
  \item{shift.amounts}{
    Default \code{c(1.5, 2, 3)}.
    For \code{ycompress}, choose the best \code{cex} yielded
    by shifting nodes by these amounts,
    in multiples of the box heights (after initial scaling).
  }
  \item{Fallen.yspace}{
    Extra space for fallen leaves.
    Default \code{.1}, meaning allow 10\% of the vertical space for the fallen leaves.
    (The name \code{Fallen.yspace} uses upper case to avoid partial matching with \code{fallen.leaves}.)
  }
  \item{boxes.include.gap}{
    Default \code{FALSE}.
    Include \code{gap} and \code{ygap} when drawing the boxes, for debugging purposes.
    (To draw the boxes, see
    \code{box.col}, \code{border.col}, \code{split.box.col}, and \code{split.border.col}.)
    This argument only affects the way the boxes are drawn, not the graph layout algorithm in any way.
    With the optimum \code{cex} at least one pair of boxes displayed in this manner will just touch
    (but none will overlap).
  }
  \item{legend.x}{
    Applies only to models with a multilevel class response
    (not binary or anova models, for which no legend is drawn).\cr
    Horizontal position of the legend.
    Typically a value between \code{0} and \code{1}, although values beyond
    those limits are often useful.\cr
    Default is \code{NULL} meaning automatically position the legend (assuming
    there is enough space). Use \code{NA} for no legend.\cr
    Use \code{trace=TRUE} to see the automatically calculated legend position.
  }
  \item{legend.y}{
    Like \code{legend.x} but for the vertical position of the legend.
  }
  \item{legend.cex}{
    Like \code{legend.x} but for the relative size of the legend text.
    Default is \code{1}.
  }
  \item{\dots}{
    Extra \code{\link{par}} arguments.  Only the ``important'' \code{par} arguments are supported.
    Note that arguments like \code{col} apply only to the \emph{node} labels.
    To affect the split labels or branch lines, use \code{split.col} and \code{branch.col} instead.
  }
}
\value{
A list with the following components.
With the default args most of these are calculated automatically.

  \item{obj}{ The \code{rpart} object.
    Identical to the \code{x} argument passed in unless \code{snip} was used.}
  \item{snipped.nodes}{ The snipped nodes, \code{NULL} unless \code{snip} was used.}
  \item{xlim, ylim}{ The graph limits.}
  \item{x, y}{ The node coords.}
  \item{branch.x, branch.y}{ The branch line coords.}
  \item{labs}{ The node labels.}
  \item{cex}{ The node label \code{cex}.}
  \item{boxes}{ The coords of the boxes around the nodes.}
  \item{split.labs}{ The split labels.}
  \item{split.cex}{ The split label \code{cex}.}
  \item{split.boxes}{ The coords of the boxes around the splits.}
}
\seealso{
  The package vignette \href{../doc/prp.pdf}{Plotting rpart trees with the rpart.plot package}\cr\cr
  \code{\link{rpart.plot}}\cr\cr
Functions in the \code{rpart} package:
  \code{\link{plot.rpart}}
  \code{\link{text.rpart}}
  \code{\link{rpart}}
}
\examples{
data(ptitanic)
tree <- rpart(survived ~ ., data = ptitanic, cp = .02)
                           # cp = .02 because want small tree for demo

old.par <- par(mfrow = c(2,2))
                           # put 4 figures on one page

prp(tree, main = "default prp\n(type =  0, extra = 0)")

prp(tree, main = "type = 4,  extra = 6\nbox.palette = \"auto\"",
    type = 4, extra = 6,   # label all nodes, show prob of second class
    box.palette = "auto",  # auto color the nodes based on the model type
    faclen = 0)            # faclen = 0 to print full factor names

cols <- ifelse(tree$frame$yval == 1, "darkred", "green4")
                           # green if survived

prp(tree, main = "assorted arguments",
    extra = 106,           # display prob of survival and percent of obs
    nn = TRUE,             # display the node numbers
    fallen.leaves = TRUE,  # put the leaves on the bottom of the page
    shadow.col = "gray",   # shadows under the leaves
    branch.lty = 3,        # draw branches using dotted lines
    branch = .5,           # change angle of branch lines
    faclen = 0,            # faclen = 0 to print full factor names
    trace = 1,             # print the auto calculated cex, xlim, ylim
    split.cex = 1.2,       # make the split text larger than the node text
    split.prefix = "is ",  # put "is " before split text
    split.suffix = "?",    # put "?" after split text
    col = cols, border.col = cols,   # green if survived
    split.box.col = "lightgray",   # lightgray split boxes (default is white)
    split.border.col = "darkgray", # darkgray border on split boxes
    split.round = .5)              # round the split box corners a tad

# compare to the plotting functions in the rpart package
plot(tree, uniform = TRUE, compress = TRUE, branch = .2)
text(tree, use.n = TRUE, cex = .8, xpd = NA) # cex is a guess, depends on your window size
title("compare to the plotting functions\nin the rpart package", cex.sub = .8)

par(old.par)
}
\keyword{rpart}
\keyword{recursive}
\keyword{partitioning}
\keyword{tree}
\keyword{CART}
