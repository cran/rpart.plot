# test.prolog.R

printf <- function(fmt, ...) cat(sprint(fmt, ...), sep="")
cat0 <- function(...) cat(..., sep="")
# test that we got an error as expected from a try() call
expect.err <- function(object, expected.msg="")
{
    if(class(object)[1] == "try-error") {
        msg <- attr(object, "condition")$message[1]
        if(length(grep(expected.msg, msg, fixed=TRUE)))
            cat0("Got error as expected from ",
                 deparse(substitute(object)), "\n")
        else
            stop(sprintf("Expected: %s\n  Got:      %s",
                         expected.msg, substr(msg[1], 1, 1000)))
    } else
        stop("Did not get expected error: ", expected.msg)
}
options(warn=1) # print warnings as they occur
if(!interactive())
    postscript(paper="letter", fonts=c("Helvetica", "NewCenturySchoolbook"))
old.par <- par(no.readonly=TRUE)
library(rpart.plot)
set.seed(2018)
