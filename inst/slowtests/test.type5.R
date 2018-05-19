# test.type5.R

library(rpart.plot)
library(rattle, quietly=TRUE)
data(ptitanic)
library(earth)
data(ozone1)
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
cat("== start test.type5.R ==\n")
anova.model <- rpart(Mileage~., data=cu.summary)
data(iris)
multi.class.model <- rpart(Species~., data=iris, cp=.001, minsplit=10)
data(ptitanic)
binary.model <- rpart(survived~., data=ptitanic, cp=.02)

old.par <- par(mfrow=c(2,3), mar=c(0, 0, 6, 0))

expect.err(try(rpart.plot(anova.model, type=-1)), "type must be 0...5, you have type=-1")
expect.err(try(rpart.plot(anova.model, type=6)), "type must be 0...5, you have type=6")
expect.err(try(rpart.plot(anova.model, type=1.23)), "floor")
expect.err(try(rpart.plot(anova.model, type="x")), "is.numeric")

rpart.plot(anova.model, type=5, main="*** rpart.plot type=5 ***\nanova.model\nmileage")

rpart.plot(multi.class.model, type=5, main="multi.class.model\nspecies")

rpart.plot(binary.model, type=5, main="binary model\nptitanic")

rpart.plot(anova.model, type=5, under=TRUE, varlen=-2, faclen=4,
           box.palette="BnBu",
           main='varlen=3, faclen=3\nbox.palette="BnBu"')

par(old.par)

old.par <- par(mfrow=c(2,3), mar=c(0, 0, 6, 0))
par(old.par)

old.par <- par(mfrow=c(2,3), mar=c(0, 0, 6, 0))

expect.err(try(prp(anova.model, type=-1)), "type must be 0...5, you have type=-1")
expect.err(try(prp(anova.model, type=6)), "type must be 0...5, you have type=6")
expect.err(try(prp(anova.model, type=1.23)), "floor")
expect.err(try(prp(anova.model, type="x")), "is.numeric")

prp(anova.model, type=5, main="*** prp type=5 ***\nanova.model\nmileage")

prp(multi.class.model, type=5, main="multi.class.model\nspecies")

prp(binary.model, type=5, main="binary model\nptitanic")

prp(multi.class.model, type=5, varlen=-5, fallen.leaves=TRUE,
    clip.left.labs=FALSE, clip.right.labs=FALSE,
    box.palette=list("Blues", "Greens", "Reds"),
    main='varlen=-5 fallen.leaves\nbox.palette=list\nclip.left.labs=clip.right.labs=FALSE')

prp(multi.class.model, main="assorted arguments",
    type=5,
    extra=104,
    under=TRUE,
    nn=TRUE,
    clip.left.labs=FALSE,
    clip.right.labs=FALSE,
    faclen=-4,
    varlen=-5,
    branch.lty=3,
    branch=.5,
    split.prefix="is ",
    split.suffix="?",
    box.palette=list("Blues", "Greens", "Grays"),
    split.box.col="lightgray",
    split.border.col="darkgray",
    split.round=.5)

prp(anova.model, type=5, under=TRUE, varlen=-2, faclen=4,
           box.palette="RdYlGn",
           main='varlen=-2, faclen=4\nbox.palette="RdYlGn"')

par(old.par)
cat("== done test.type5.R ==\n")
