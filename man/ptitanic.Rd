\name{ptitanic}
\alias{ptitanic}
\title{Titanic data with passenger names and other details removed.}
\description{Titanic data with passenger names and other details removed.}
\format{
  A data frame with 1046 observations on 6 variables.

  \tabular{ll}{
      \code{pclass}   \tab passenger class, unordered factor: 1st 2nd 3rd\cr
      \code{survived} \tab factor: died or survived\cr
      \code{sex}      \tab unordered factor: male female\cr
      \code{age}      \tab age in years, min 0.167 max 80.0\cr
      \code{sibsp}    \tab number of siblings or spouses aboard, integer: 0...8\cr
      \code{parch}    \tab number of parents or children aboard, integer: 0...6\cr
  }
}
\source{
The dataset was compiled by Frank Harrell and Robert Dawson:\cr
\url{http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic.html}.

See also:\cr
\url{http://biostat.mc.vanderbilt.edu/twiki/pub/Main/DataSets/titanic3info.txt}.

For this version of the Titanic data, passenger details were deleted,
\code{survived} was cast as a \code{factor},
and the name changed to \code{ptitanic} to minimize confusion with
other versions.

In this data the crew are conspicuous by their absence.

Contents of \code{ptitanic}:
    \preformatted{
         pclass survived    sex    age sibsp parch
    1       1st survived female 29.000     0     0
    2       1st survived   male  0.917     1     2
    3       1st     died female  2.000     1     2
    4       1st     died   male 30.000     1     2
    5       1st     died female 25.000     1     2
    ...
    1309    3rd     died   male 29.000     0     0
    }
How \code{ptitanic} was built:
    \preformatted{
    load("titanic3.sav") # from Dr. Harrell's web site
    # discard name, ticket, fare, cabin, embarked, body, home.dest
    ptitanic <- titanic3[,c(1,2,4,5,6,7)]
    # change survived from integer to factor
    ptitanic$survived <- factor(ptitanic$survived, labels=c("died", "survived"))
    save(ptitanic, file="ptitanic.rda")}
This version of the data differs from
\code{\link[earth]{etitanic}} in the \code{\link[earth]{earth}} package
in that here \code{survived} is a factor (not an integer)
and \code{age} has some \code{NA}s.
}
\examples{
data(ptitanic)
summary(ptitanic)
# main indicator of missing data is 3rd class esp. with many children
obs.with.nas <- rowSums(is.na(ptitanic)) > 0
prp(rpart(obs.with.nas~., data=ptitanic, method="class"),
    main="observations with missing data", extra=7)
}
\keyword{datasets}
