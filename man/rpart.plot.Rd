\name{rpart.plot}
\alias{rpart.plot}
\title{Plot an \code{rpart} model. A simplified interface to the \code{prp} function.}
\description{
Plot an \code{\link{rpart}} model, automatically tailoring the plot
for the model's response type.

For an overview, please see the package vignette
\href{../doc/prp.pdf}{Plotting rpart trees with the rpart.plot package}.

This function is a simplified front-end to \code{\link{prp}},
with only the most useful arguments of that function, and
with different defaults for some of the arguments.

The different defaults for the \code{extra} and \code{col.palette}
arguments mean that this function automatically creates a colored plot
suitable for the type of model (whereas \code{\link{prp}} by default
creates a minimal plot).  In detail the different defaults are:

\tabular{llclcr}{
                     \tab | \tab \code{\link{rpart.plot}} \tab | \tab \code{\link{prp}} and \code{\link{rpart.plot.version1}} \tab | \cr
                     \tab   \tab               \tab   \tab              \tab   \cr
\code{type}          \tab | \tab \code{2}      \tab | \tab \code{0}     \tab | \cr
\code{extra}         \tab | \tab \code{"auto"} \tab | \tab \code{0}     \tab | \cr
\code{fallen.leaves} \tab | \tab \code{TRUE}   \tab | \tab \code{FALSE} \tab | \cr
\code{varlen}        \tab | \tab \code{0}      \tab | \tab \code{-8}    \tab | \cr
\code{faclen}        \tab | \tab \code{0}      \tab | \tab \code{3}     \tab | \cr
\code{box.palette}   \tab | \tab \code{"auto"} \tab | \tab \code{0}     \tab | \cr
}

The function \code{\link{rpart.plot.version1}} is compatible with
old versions of this function.
}
\usage{
rpart.plot(x=stop("no 'x' arg"),
    type=2, extra="auto",
    under=FALSE, fallen.leaves=TRUE,
    digits=2, varlen=0, faclen=0,
    cex=NULL, tweak=1,
    snip=FALSE,
    box.palette="auto", shadow.col=0,
    ...)
}
\arguments{
To start off, look at the arguments \code{x}, \code{type} and \code{extra}.
Just those arguments will suffice for many users.
If you don't want a colored plot, use \code{box.palette=0}.

  \item{x}{
     An \code{\link{rpart}} object.  The only required argument.
  }
  \item{type}{
    Type of plot. Possible values:

     \bold{0} Draw a split label at each split
              and a node label at each leaf.

     \bold{1} Label all nodes, not just leaves.
              Similar to \code{text.rpart}'s \code{all=TRUE}.

     \bold{2} Default.
              Like \code{1} but draw the split labels below the node labels.
              Similar to the plots in the CART book.

     \bold{3} Draw separate split labels for the left and right directions.

     \bold{4} Like \code{3} but label all nodes, not just leaves.
              Similar to \code{text.rpart}'s \code{fancy=TRUE}.
              See also \code{clip.right.labs}.\cr

  }
  \item{extra}{
     Display extra information at the nodes. Possible values:

     \bold{"auto"} (case insensitive) Default.\cr
Automatically select a value based on the model type, as follows:\cr
\code{extra=106} class model with a binary response\cr
\code{extra=104} class model with a response having more than two levels\cr
\code{extra=100} other models\cr

     \bold{0} No extra information.

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
              % (One could argue that \code{2} should be the default value
              % for \code{class} models.)
              % For example, \code{.81 .19} means that of the observations in the node,
              % the first class has a probability of \code{.81}
              % and the second a probability of \code{.19}.

     \bold{5} Class models:
              like \code{4} but do not display the fitted class.

     \bold{6} Class models:
              the probability of the second class only.
              Useful for binary responses.

     \bold{7} Class models:
              like \code{6} but do not display the fitted class.

     \bold{8} Class models:
              the probability of the fitted class.

     \bold{9} Class models:
              the probabilities times the fraction of observations in the node
              (the probability relative to all observations,
              sum across all leaves is 1).

     \bold{+100} Add \code{100} to any of the above to also display
                 the percentage of observations in the node.
                 For example \code{extra=101} displays the number
                 and percentage of observations in the node.
                 Actually, it's a weighted percentage
                 using the \code{weights} passed to \code{rpart}.\cr

    Note 1: Unlike \code{text.rpart},
    by default \code{prp} uses its own routine for
    generating node  labels (not the function attached to the object).
    See the \code{node.fun} argument.\cr
    Note 2: The \code{extra} argument has special meaning
    for \code{mvpart} objects.
    See the Appendix to this package's vignette.
  }
  \item{under}{
     Applies only if \code{extra > 0}.
     Default \code{FALSE}, meaning put the extra text \emph{in} the box.
     Use \code{TRUE} to put the text \emph{under} the box.
  }
  \item{fallen.leaves}{
     Default \code{TRUE} to position the leaf nodes at the bottom of the graph.
     It can be helpful to use \code{FALSE} if the graph is too crowded
     and the text size is too small.
  }
  \item{digits}{
     The number of significant digits in displayed numbers.
     Default \code{2}.
     If \code{0}, use \code{\link{getOption}("digits")}.\cr
     Details:
     Numbers from \code{0.001} to \code{9999} are printed without an exponent
     (and the number of digits is actually only a suggestion,
     see \code{\link{format}} for details).
     Numbers out that range are printed with an ``engineering'' exponent (a multiple of 3).
  }
  \item{varlen}{
     Length of variable names in text at the splits
     (and, for class responses, the class in the node label).
     Default \code{0}, meaning display the full variable names.
     Possible values:\cr

     \bold{0} use full names (default).\cr

     \bold{greater than 0} call \code{\link{abbreviate}} with the given \code{varlen}.\cr

     \bold{less than 0} truncate variable names to the shortest length where they are still unique,
     but never truncate to shorter than \code{abs(varlen)}.
  }
  \item{faclen}{
     Length of factor level names in splits.
     Default \code{0}, meaning display the full factor names.
     Possible values are as \code{varlen} above, except that
     for back-compatibility with \code{\link{text.rpart}} the special value \code{1}
     means represent the factor levels with alphabetic characters
     (\code{a} for the first level, \code{b} for the second, etc.).
  }
  \item{cex}{
     Default \code{NULL}, meaning calculate the text size automatically.
  }
  \item{tweak}{
     Adjust the (possibly automatically calculated) \code{cex}.
     Default \code{1}, meaning no adjustment.
     Use say \code{tweak=1.2} to make the text 20\% larger.
     However, since font sizes are discrete the \code{cex} you ask for
     may not be exactly the \code{cex} you get.
     And a small change to tweak may not actually change the type size,
     or change it more than you want.
  }
  \item{snip}{
     Default \code{FALSE}.
     Set \code{TRUE} to interactively trim the tree with the mouse.
     See the \href{../doc/prp.pdf}{package vignette} (or just try it).
  }
  \item{box.palette}{
     Palette for coloring the node boxes based on the fitted value.
This is a vector of \code{\link{colors}},
for example \bold{box.palette=c("Red", "Yellow", "Green")}.
Small fitted values are displayed with colors at the start of the vector;
large values with colors at the end.\cr

The special value \bold{box.palette=0} (default) uses the background color (typically white).\cr

The special value \bold{box.palette="auto"} (case insensitive)
automatically selects a predefined palette based on the type of model.

Otherwise specify a predefined palette
e.g. \bold{box.palette="Grays"} for the predefined gray palette (a range of grays).
The predefined palettes are:\cr
\code{Blues} \code{Browns} \code{Grays} \code{Greys} \code{Greens} \code{Oranges} \code{Reds} \code{Purples} \cr
\code{Bu} \code{Bn} \code{Gn} \code{Gy} \code{Or} \code{Rd} \code{Pu} (alternative names for the above palettes)\cr
\code{BuGn} \code{GnRd} \code{BuOr} etc. (two color palettes: any combination of two palettes)\cr
\code{RdYlGn} \code{GnYlRd} (three color palettes)\cr\cr
Prefix the palette name with \bold{-} to reverse the order of the colors\cr
e.g. \code{box.palette="-auto"} or \code{box.palette="-Grays"}.\cr\cr
The \code{box.palette} argument is ignored if the \code{box.col} palette argument is specified.
}
  \item{shadow.col}{
     Color of the shadow under the boxes.
     Default \code{0}, no shadow.
     Try \code{"gray"} or \code{"darkgray"}.
  }
  \item{\dots}{
    Extra arguments passed to \code{\link{prp}} and the plotting routines.
    Any of \code{\link{prp}}'s arguments can be used.
  }
}
\value{
The returned value is identical to that of \code{\link{prp}}.
}
\author{
  Stephen Milborrow, borrowing heavily from the \code{\link{rpart}}
  package by Terry M. Therneau and Beth Atkinson,
  and the R port of that package by Brian Ripley.
}
\seealso{
  The package vignette \href{../doc/prp.pdf}{Plotting rpart trees with the rpart.plot package}\cr\cr
  \code{\link{prp}}\cr\cr
  \code{\link{rpart.plot.version1}}\cr\cr
Functions in the \code{rpart} package:
  \code{\link{plot.rpart}}
  \code{\link{text.rpart}}
  \code{\link{rpart}}
}
\examples{
old.par <- par(mfrow=c(2,2))                    # put 4 figures on one page

data(ptitanic)

#---------------------------------------------------------------------------

binary.model <- rpart(survived ~ ., data=ptitanic, cp=.02)
                                        # cp=.02 for small demo tree

rpart.plot(binary.model,
           main="titanic survived\n(binary response)")

rpart.plot(binary.model,
           box.palette="Grays",         # override default GnBu palette
           main="titanic survived\nbox.palette = \"Grays\"")

#---------------------------------------------------------------------------

anova.model <- rpart(Mileage ~ ., data=cu.summary)

rpart.plot(anova.model,
           shadow.col="gray",           # add shadows just for kicks
           main="miles per gallon\n(continuous response)\n")

#---------------------------------------------------------------------------

multi.class.model <- rpart(Reliability ~ ., data=cu.summary)

rpart.plot(multi.class.model,
           main="vehicle reliability\n(multi class response)")

par(old.par)
}
\keyword{rpart}
\keyword{recursive}
\keyword{partitioning}
\keyword{tree}
\keyword{CART}
