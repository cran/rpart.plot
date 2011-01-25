\name{rpart.plot}
\alias{rpart.plot}
\title{Plot an rpart model.}
\description{
Plot an \code{\link{rpart}} model.
This function combines and extends \code{\link{plot.rpart}} and \code{\link{text.rpart}}
in the \code{\link{rpart}} package.
It automatically scales and adjusts the displayed tree for best fit.

This is a front end to \code{\link{prp}},
with the most useful arguments of that function.
}
\usage{
rpart.plot(x=stop("no 'x' arg"),
    type=0, extra=0, under=FALSE, clip.right.labs=TRUE,
    fallen.leaves=FALSE, branch=if(fallen.leaves) 1 else .2,
    uniform=TRUE,
    digits=2, varlen=-8, faclen=3,
    cex=NULL, tweak=1,
    compress=TRUE, ycompress=uniform,
    snip=FALSE,
    ...)
}
\arguments{
To start off, look at the arguments \code{x}, \code{type} and \code{extra}.
Just those arguments will suffice for many users.
For an overview of the other arguments see \url{../doc/prp.pdf}.

  \item{x}{
     An \code{\link{rpart}} object.  The only required argument.
  }
  \item{type}{
    Type of plot. Five possibilities:

     \code{0} The default.  Draw a split label at each split
              and a node label at each leaf.

     \code{1} Label all nodes, not just leaves.
              Similar to \code{text.rpart}'s \code{all=TRUE}.

     \code{2} Like \code{1} but draw the split labels below the node labels.
              Similar to the plots in the CART book.

     \code{3} Draw separate split labels for the left and right directions.

     \code{4} Like \code{3} but label all nodes, not just leaves.
              Similar to \code{text.rpart}'s \code{fancy=TRUE}.
              See also \code{clip.right.labs}.\cr

  }
  \item{extra}{
     Display extra information at the nodes. Possible values:

     \code{0} No extra information (the default).

     \code{1} Display the number of observations that fall in the node
              (per class for \code{class} objects;
              prefixed by the number of events for \code{poisson} and \code{exp} models).
              Similar to \code{text.rpart}'s \code{use.n=TRUE}.

     \code{2} Class models: display the classification rate at the node,
              expressed as the number of correct classifications and the number
              of observations in the node.\cr
              Poisson and exp models: display the number of events.

     \code{3} Class models: misclassification rate at the node,
              expressed as the number of incorrect classifications and the number
              of observations in the node.

     \code{4} Class models:
              probability per class of observations in the node
              (conditioned on the node, sum aross a node is 1).
              % (One could argue that \code{2} should be the default value
              % for \code{class} models.)
              % For example, \code{.81 .19} means that of the observations in the node,
              % the first class has a probability of \code{.81}
              % and the second a probability of \code{.19}.

     \code{5} Class models:
              like \code{4} but do not display the fitted class.

     \code{6} Class models:
              the probability of the second class only.
              Useful for binary responses.

     \code{7} Class models:
              like \code{6} but do not display the fitted class.

     \code{8} Class models:
              the probability of the fitted class.

     \code{9} Class models:
              the probabilities times the fraction of observations in the node
              (the probability relative to all observations,
              sum across all leaves is 1).

     \code{+100} Add \code{100} to any of the above to also display
                 the percentage of observations in the node.
                 For example \code{use.n=101} displays the number
                 and percentage of observations in the node.
                 Actually, it's a weighted percentage
                 using the \code{weights} passed to \code{rpart}.\cr

    Note: Unlike \code{text.rpart},
    by default \code{prp} uses its own routine for
    generating node  labels (not the function attached to the object).
    See \code{node.fun}.\cr

  }
  \item{under}{
     Applies only if \code{extra > 0}.
     Default \code{FALSE}, meaning put the extra text in the box.
     Use \code{TRUE} to put the text under the box.
  }
  \item{clip.right.labs}{
     Deafult is \code{TRUE} meaing ``clip'' the right-hand split labels,
     i.e. do not print \code{variable=}.
     Applies only if \code{type=3} or \code{4}.
  }
  \item{fallen.leaves}{
     Default \code{FALSE}. If \code{TRUE}, display the leaves at the bottom of the graph.
  }
  \item{branch}{
    Controls the shape of the branch lines.
     Specify a value between \code{0} (V shaped branches) and
     \code{1} (square shouldered branches).
     Default is \code{if(fallen.leaves) 1 else .2}.
  }
  \item{uniform}{
    If \code{TRUE} (the default), the vertical spacing of the nodes is uniform.
    If \code{FALSE}, the nodes are spaced proportionally to the fit
    (more precisely, to the difference between a node's deviance and the sum of its children's deviances).
    Small spaces must be expanded to leave room for the labels.
    Note: \code{uniform=FALSE} with \code{cex=NULL} (the default)
    can sometimes cause very small text.
  }
  \item{digits}{
     The number of significant digits in displayed numbers (actually only a
     suggestion, see \code{\link{format}} for details).
     Default \code{2}.
     If \code{0}, use \code{\link{getOption}("digits")}.
     Note: Probabilities and percentages are treated specially.
     Probabilities are displayed with \code{digits} digits after the decimal point
     (by default 2 digits).
     Percentages are displayed with \code{digits-2} digits after the decimal point
     (by default no digits).
  }
  \item{varlen}{
     Length of variable names in text at the splits
     (and, for class responses, the class displayed at the node).
     Default \code{-8}, meaning truncate to eight characters.
     Three possibilities:\cr
     =0 use full names.\cr
     >0 call \code{\link{abbreviate}} with the given \code{varlen}.\cr
     <0 truncate variable names to the shortest length where they are still unique,
     but never truncate to shorter than \code{abs(varlen)}.
     % For example, \code{varlen=-1} will give unique names that are as short as possible.
  }
  \item{faclen}{
     Length of factor level names in splits.
     Default \code{3}, meaning abbreviate to three characters.
     Possible values are as \code{varlen} above, except that
     \code{1} is treated specially, meaning
     represent the factor levels with alphabetic characters
     (\code{a} for the first level, \code{b} for the second, etc.).
  }
  \item{cex}{
     Default \code{NULL}, meaning calculate the text size automatically.
     The default automatic calculation
     means that this seemingly innocuous argument has a far reaching effect.
     If necessary it will trigger the node shifting engine
     to get a decent type size (see \code{ycompress}).
  }
  \item{tweak}{
     Adjust the (possibly automatically calculated) \code{cex}.
     Default \code{1}, meaning no adjustment.
     Use say \code{tweak=1.1} to make the text 10\% larger.
     Note that font sizes are discrete, so the \code{cex} you ask for
     may not be the \code{cex} you get.
     And a small tweak may not actually change the type size
     or change it more than you want.
  }
  \item{compress}{
     Default \code{TRUE}, meaning compress the tree horizontally
     by shifting nodes horizontally where space is available.
     (This is the same as \code{plot.rpart}'s argument of the same name,
     except that here the default is \code{TRUE}.)
  }
  \item{ycompress}{
     If \code{TRUE} (the default unless \code{uniform=FALSE})
     and the initial automatically calculated \code{cex} is less than \code{0.7},
     shift crowded labels vertically where space is available.
     This often allows considerably larger text.
     Use \code{ycompress=FALSE} if you feel the resulting display is too messy.
     In the current implementation, the shifting algorithm
     works a little better (allowing larger text)
     with \code{type=1}, \code{2}, or \code{3}. % (i.e. not "fancy").
  }
  \item{snip}{
     Default \code{FALSE}.
     Set \code{TRUE} to interactively trim the tree with the mouse.
     See \url{../doc/prp.pdf} (or just try it).
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
  \url{../doc/prp.pdf}\cr
  \code{\link{prp}}\cr
  \code{\link{plot.rpart}}\cr
  \code{\link{text.rpart}}\cr
  \code{\link{rpart}}
}
\examples{
data(ptitanic)
tree <- rpart(survived ~ ., data=ptitanic, cp=.02)
                         # cp=.02 because want small tree for demo

old.par <- par(mfrow=c(2,2))
                         # put 4 figures on one page

rpart.plot(tree, main="default rpart.plot\n(type = 0, extra = 0)")

prp(tree, main="type = 4,  extra = 6", type=4, extra=6, faclen=0)
                         # faclen=0 to print full factor names

rpart.plot(tree, main="extra = 106,  under = TRUE", extra=106, under=TRUE, faclen=0)

# the old way for comparison
plot(tree, uniform=TRUE, compress=TRUE, branch=.2)
text(tree, use.n=TRUE, cex=.6, xpd=NA) # cex is a guess, depends on your window size
title("rpart.plot for comparison", cex=.6)

par(old.par)
}
\keyword{rpart}
\keyword{recursive}
\keyword{partitioning}
\keyword{tree}
\keyword{CART}
