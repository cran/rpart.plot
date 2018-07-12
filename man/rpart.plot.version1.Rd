\name{rpart.plot.version1}
\alias{rpart.plot.version1}
\title{Plot an \code{rpart} model (old version).}
\description{
This function is provided for backward compatibility
with old versions of \code{\link{rpart.plot}}
(prior to version 2.0.0, June 2016).

By default this function creates a minimal unadorned plot, whereas
\code{rpart.plot} creates a colored plot automatically tailored to the
model's type.

Actually, the only difference between the two functions is that some
arguments have different defaults.
This function has the same defaults as \code{\link{prp}}.
See the \code{\link{rpart.plot}} help page for a table showing the
different defaults.
}
\usage{
rpart.plot.version1(x = stop("no 'x' arg"),
    type = 0, extra = 0,
    under = FALSE, fallen.leaves = FALSE,
    digits = 2, varlen = -8, faclen = 3,
    cex = NULL, tweak = 1,
    snip = FALSE,
    box.palette = 0, shadow.col = 0,
    ...)
}
\arguments{
  \item{x}{ Same as \code{\link{rpart.plot}}.
  }
  \item{type}{ Default \code{0}. Same as \code{\link{rpart.plot}}.
  }
  \item{extra}{ Default \code{0}, otherwise the same as \code{\link{rpart.plot}}
                (which has a default of \code{"auto"}).
  }
  \item{under}{ Default \code{FALSE}. Same as \code{\link{rpart.plot}}.
  }
  \item{fallen.leaves}{ Default \code{FALSE}, otherwise the same as \code{\link{rpart.plot}}
                (which has a default of \code{TRUE}).
  }
  \item{digits}{ Default \code{2}. Same as \code{\link{rpart.plot}.}
  }
  \item{varlen}{ Default \code{-8}, otherwise the same as \code{\link{rpart.plot}}
                (which has a default of \code{0}).
  }
  \item{faclen}{ Default \code{3}, otherwise the same as \code{\link{rpart.plot}}
                (which has a default of \code{0}).
  }
  \item{cex}{ Default \code{NULL}. Same as \code{\link{rpart.plot}}.
  }
  \item{tweak}{ Default \code{1}. Same as \code{\link{rpart.plot}}.
  }
  \item{snip}{ Default \code{FALSE}. Same as \code{\link{rpart.plot}}.
  }
  \item{box.palette}{ Default \code{0}, otherwise the same as \code{\link{rpart.plot}}
                (which has a default of \code{"auto"}).
  }
  \item{shadow.col}{ Default \code{0}. Same as \code{\link{rpart.plot}}.
  }
  \item{\dots}{ Same as \code{\link{rpart.plot}}.
  }
}
\value{
The returned value is identical to that of \code{\link{prp}}.
}
\examples{
data(ptitanic)
tree <- rpart(survived ~ ., data = ptitanic, cp = .02)
                                # cp = .02 because want small tree for demo

old.par <- par(mfrow = c(2,2))
                                # put 4 figures on one page

rpart.plot.version1(tree, main = "default rpart.plot.version1\n(type = 0, extra = 0)")

rpart.plot.version1(tree, main = "type = 4, extra = 6",
                    type = 4, extra = 6,
                    faclen = 0) # faclen = 0 to print full factor names

rpart.plot.version1(tree, main = "extra = 106,  under = TRUE",
                    extra = 106, under = TRUE, faclen = 0)

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
