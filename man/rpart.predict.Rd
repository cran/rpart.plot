\name{rpart.predict}
\alias{rpart.predict}
\title{Extended version of predict.rpart}
\description{
Identical to \code{\link{predict.rpart}} but
optionally show the node numbers and rules for the predicted values.
}
\usage{
rpart.predict(object, newdata,
         type = c("vector", "prob", "class", "matrix"),
         na.action = na.pass,
         nn=FALSE, rules=FALSE, ...)
}
\arguments{
\item{object, newdata, type, na.action}{
Identical to the same arguments for \code{\link{predict.rpart}}.\cr
If both \code{nn} and \code{rules} are \code{FALSE},
the returned value is identical to \code{\link{predict.rpart}}.
}
\item{nn}{
If \code{TRUE}, return a \code{data.frame}
with the predictions as usual
but with an extra column showing the leaf node number for each prediction.
}
\item{rules}{
If \code{TRUE}, return a \code{data.frame}
with the predictions as usual
but with an extra column showing the rpart rule (as a string) for each prediction.
}
\item{...}{
Passed on to \code{\link{rpart.rules}},
for example \code{clip.facs=TRUE}.
}
}
\value{
Same as \code{\link{predict.rpart}},
but with additional information if \code{nn=TRUE} and/or \code{rules=TRUE}.
}
\seealso{
\code{\link{predict.rpart}}\cr
\code{\link{rpart.rules}}
}
\examples{
data(ptitanic)
model <- rpart(survived ~ ., data = ptitanic, cp = .02)
head(rpart.predict(model, rules=TRUE))
}
