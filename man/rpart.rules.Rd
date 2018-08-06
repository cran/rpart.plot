\name{rpart.rules}
\alias{rpart.rules}
\alias{print.rpart.rules}
\title{Print an rpart model as a set of rules.}
\description{
Print an \code{rpart} model as a set of rules.
}
\usage{
rpart.rules(x = stop("no 'x' argument"),
            style = "wide", cover = FALSE, nn = FALSE,
            roundint = TRUE, clip.facs = FALSE,
            varorder = NULL, ...)

\method{print}{rpart.rules}(x = stop("no 'x' argument"), style = attr(x, "style"), ...)
}
\arguments{
\item{x}{
An \code{\link{rpart}} object.  The only required argument.
}
\item{style}{One of:\cr
\bold{"wide"} (default) One rule per line.  May require a lot of horizontal space.\cr
\bold{"tall"} One split per line.\cr
\bold{"tallw"} Like \code{"tall"} but with more horizontal white space for readability.\cr
}
\item{cover}{
Default \code{FALSE}.
If \code{TRUE}, also print the percentage of cases covered by each rule.
}
\item{nn}{
Default \code{FALSE}.
If \code{TRUE}, also print the leaf node number for each rule.
}
\item{roundint}{
If \code{roundint=TRUE} (default) and all values of a predictor in the
training data are integers, then splits for that predictor
are rounded to integer.
For example, display \code{nsiblings < 3} instead of \code{nsiblings < 2.5}.\cr
Identical to the argument of the same name in \code{\link{rpart.plot}}, see there for details.
}
\item{clip.facs}{
Default \code{FALSE}.
If \code{TRUE}, print splits on factors as \code{female} instead of
\code{sex = female}; the variable name and equals is dropped.
\cr
Identical to the argument of the same name in \code{\link{rpart.plot}}.
}
\item{varorder}{
By default, the variables in the rules are ordered left to right on
importance, where the ``importance'' of a variable here is the number of rules
it appears in.\cr
Use \code{varorder} to force variables to appear first in the rules.
For example \code{varorder="sex"} or \code{varorder=c("sex", "pclass"})
will put the specified variables first.\cr
Partial matching of variable names is supported.
}
\item{...}{
The following can be passed as dot arguments.
See \code{\link{prp}} for details on these arguments.

\tabular{llllll}{
\bold{argument}  \tab \bold{default} \tab \tab \tab \tab \cr
\code{extra} \tab \code{= "auto"} \tab \tab \tab \tab a subset of the legal values for \code{prp} are supported  \cr
\code{digits} \tab \code{= 2} \tab \tab \tab \tab default is two digits of accuracy, increase if necessary  \cr
\code{varlen} \tab \code{= 0} \tab \tab \tab \tab default displays full variable names  \cr
\code{faclen} \tab \code{= 0} \tab \tab \tab \tab default displays full factor names  \cr
\code{trace} \tab \code{= 0} \tab \tab \tab \tab \cr
\code{and} \tab \code{= " & "} \tab \tab \tab \tab the string that separates the splits when \code{style="wide"}  \cr
\code{because} \tab \code{= " because "} \tab \tab \tab \tab used by \code{\link{rpart.predict}} \cr
\code{facsep} \tab \code{= " or "} \tab \tab \tab \tab \cr
\code{eq} \tab \code{= " is "} \tab \tab \tab \tab \cr
\code{lt} \tab \code{= " <  "} \tab \tab \tab \tab \cr
\code{ge} \tab \code{= " >= "} \tab \tab \tab \tab \cr
\code{response.name} \tab \code{= NULL} \tab \tab \tab \tab the response name printed before the rules (\code{NULL} means automatic)  \cr
}
}
}
% \note{
%
% \bold{Order of printed rules}
%
% For \code{anova} models the rules are sorted on increasing fit.
%
% For \code{binomial class} models the rules are sorted on increasing fitted probability.
%
% For \code{multiclass} models, the rules are sorted first on
% the fitted class, then on the probability of that class.
%
% Finally, if the fit for two rules is the same, we sort the
% order of leaves in the plotted tree (from right to left)
% }
\value{
A \code{data.frame} of class \code{c("rpart.rules", "data.frame")}
with some attached attributes which are passed on to \code{print.rpart.rules}.
}
\seealso{
\code{\link{rpart.plot}}
}
\examples{
data(ptitanic)
model <- rpart(survived ~ ., data = ptitanic, cp = .02)
rpart.plot(model)
rpart.rules(model)
}
