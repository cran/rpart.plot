# test.na.R: test that NA predicted responses are shown with a hatched box

library(rpart.plot)
data(ptitanic)
library(earth)
data(ozone1)
sessionInfo()
ititanic <- ptitanic
ititanic$survived <- as.integer(ititanic$survived == "survived")
options(warn=1) # print warnings as they occur

# test that we got an error as expected from a try() call
expect.err <- function(object, expected.msg="")
{
    if(class(object)[1] == "try-error") {
        msg <- attr(object, "condition")$message[1]
        if(length(grep(expected.msg, msg)))
            cat("Got error as expected from ",
                deparse(substitute(object)), "\n", sep="")
        else
            stop(sprintf("Expected \"%s\"\n  but got \"%s...\"",
                         expected.msg, substr(msg, 1, 120)))
    } else
        stop("did not get expected try error")
}
par(mfrow=c(2,2))

# TODO I'm not sure how to generate models with NAs in the fitted values
#      so I hack it here by forcing values in the models frame

#--- continuous response ---

a.age <- rpart(age~., data=ptitanic, cp=.02)
a.age$frame$yval[6] <- NA
rpart.plot(a.age, nn=1, main="age with na")

# --- binary response ---

a <- rpart(survived~., data=ptitanic, control=list(cp=.02))
a$frame$yval[3] <- a$frame$yval2[3,1] <- a$frame$yval2[3,5] <- NA
a$frame$yval[4] <- a$frame$yval2[4,1] <- a$frame$yval2[4,5] <- NA

expect.err(try(rpart.plot(a, nn=1, type=1, fallen.leaves=FALSE,
               main="survived with na")),
 "Diverging palettes like box.palette=\"BuGn\" cannot be used for this model")

rpart.plot(a, nn=1, type=1, fallen.leaves=FALSE, box.palette="Blues",
           main="survived with na")

#--- multiclass response ---

a.pclass <- rpart(pclass ~ ., data=ptitanic, control=rpart.control(cp=.01))
a.pclass$frame$yval[3] <- a.pclass$frame$yval2[3,1] <- NA
rpart.plot(a.pclass, nn=1, main="pclass with na")
