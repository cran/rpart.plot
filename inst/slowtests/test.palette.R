# test.palette.R

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

# binomial model

cat("== start test.palette.R ==\n")
data(ptitanic)
mod.survived <- rpart(survived~., data=ptitanic, control=list(cp=.02))
# test built-in palettes
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot     (mod.survived, type=2, fallen.leaves=TRUE, main="test.palette.R\ntwo class, box.palette=\"auto\"")
library(rattle, quietly=TRUE)
fancyRpartPlot(mod.survived, main="fancyRpartPlot", sub="")
rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="Blues",    box.palette="Blues", trace=1)
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Blues",   box.palette="-Blues", trace=2)
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Greens",   box.palette="Greens")
prp                (mod.survived, type=2, extra="AUTO", fallen.leaves=TRUE, main="-Greens",  box.palette="-Greens")
rpart.plot         (mod.survived, type=2, extra="AUTO", fallen.leaves=TRUE, main="Oranges",  box.palette="Oranges")
rpart.plot.version1(mod.survived, type=2, extra="Auto", fallen.leaves=TRUE, main="-Oranges", box.palette="-Oranges")
rpart.plot         (mod.survived, type=2, extra="Auto", fallen.leaves=TRUE, main="Purples",  box.palette="Purples")
prp                (mod.survived, type=2, extra="autO", fallen.leaves=TRUE, main="-Purples", box.palette="-Purples")
rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="Reds",     box.palette="Reds")
rpart.plot.version1(mod.survived, type=2, extra="aUTO", fallen.leaves=TRUE, main="-Reds",    box.palette="-Reds")
rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="Grays",    box.palette="Grays")
rpart.plot.version1(mod.survived, type=2, extra="au",   fallen.leaves=TRUE, main="-Grays",   box.pal="-Grays")
rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="Greys",    box.pa="Greys")
rpart.plot.version1(mod.survived, type=2, extra="AU",   fallen.leaves=TRUE, main="-Greys",   box.p="-Greys")
rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="RdYlGn",   box.palette="RdYlGn")
rpart.plot.version1(mod.survived, type=2, extra="aU",   fallen.leaves=TRUE, main="-RdYlGn",  box.palette="-RdYlGn")
rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="BuGn",     box.palette="BuGn")
rpart.plot.version1(mod.survived, type=2, extra="Au",   fallen.leaves=TRUE, main="-BuGn",    box.palette="-BuGn")
rpart.plot         (mod.survived, type=2, extra="aut",  fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot.version1(mod.survived, type=2, extra="AUT",  fallen.leaves=TRUE, main="-BuRd",    box.palette="-BuRd")
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdGn",    box.palette="-RdGn")
# rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="pal1",   box.palette="pal1")

rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="BuGn",     box.palette="BuGn")
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GnBu",     box.palette="GnBu")
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdBu",     box.palette="RdBu")
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GnRd",     box.palette="GnRd")
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Browns",   box.palette="Browns")
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuBu",     box.palette="BuBu")

# test list palette for binomial response
par(mfrow = c(2, 2))
rpart.plot         (mod.survived, type=2,               fallen.leaves=TRUE, main="test list palette for binomial response\nlist(Blues, Greens)", box.palette=list("Blues", "Greens"))
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="list(Blues, Purples)", box.palette=list("Blues", "Purples"))
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuGn", box.palette="BuGn")

# test custom palettes
par(mfrow = c(2, 2))
box.palette=gray(seq(.95, .6, length.out=100))
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main=paste(length(box.palette), " grays\n(custom palette)"), box.palette=box.palette)

box.palette=gray(seq(.9, .6, length.out=3))
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main=paste(length(box.palette), " grays"), box.palette=box.palette)

box.palette=gray(seq(.9, .6, length.out=2))
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main=paste(length(box.palette), " grays"), box.palette=box.palette)

rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="single pink", box.palette="pink")

rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="wheat", box.palette="wheat")

box.palette=c("pink", "palegreen")
rpart.plot         (mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="pink palegreen", box.palette=box.palette)

box.palette=c("pink", "wheat", "palegreen")
rpart.plot.version1(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="pink wheat palegreen", box.palette=box.palette)

# continuous response (anova tree)
itit <- ptitanic
itit$survived <- as.integer(itit$survived == "survived")
mod.continuous.survived <- rpart(survived~., data=itit, control=list(cp=.02))
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot         (mod.continuous.survived, type=2, extra="auto", main="continuous (survived)")
fancyRpartPlot(mod.continuous.survived, main="fancyRpartPlot", sub="")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Greens",  box.palette="-Greens")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Greens",   box.palette="Greens")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Blues",    box.palette="Blues")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Blues",   box.palette="-Blues")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Oranges",  box.palette="Oranges")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Oranges", box.palette="-Oranges")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Purples",  box.palette="Purples")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Purples", box.palette="-Purples")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Reds",     box.palette="Reds")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Reds",    box.palette="-Reds")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Grays",    box.palette="Grays")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Grays",   box.palette="-Grays")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Greys",    box.palette="Greys")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Greys",   box.palette="-Greys")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdYlGn",   box.palette="RdYlGn")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdYlGn",  box.palette="-RdYlGn")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuGn",     box.palette="BuGn")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-BuGn",    box.palette="-BuGn")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-BuRd",    box.palette="-BuRd")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdGn",    box.palette="-RdGn")

rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GnBu",     box.palette="GnBu")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-BuGn",    box.palette="-BuGn")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdBu",    box.palette="-RdBu")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-GnRd",    box.palette="-GnRd")
rpart.plot.version1(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GnYlRd",   box.palette="GnYlRd")
rpart.plot         (mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdYlGn",  box.palette="-RdYlGn")

par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
mod.age <- rpart(age ~ ., data=ptitanic)
rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto", main="age\nbox.palette=\"auto\"")
fancyRpartPlot(mod.age, main="fancyRpartPlot", sub="")
rpart.plot         (mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="Greens", yesno=2, main="Greens yesno=2")
rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="RdYlGn", main="RdYlGn")
rpart.plot         (mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="Reds",   main="Reds")
rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="BuGn",   main="BuGn")
rpart.plot         (mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto")

expect.err(try(rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="notacolor")),
           "box.palette: \"notacolor\" is neither a color nor a palette")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="#1234XX")),
           "box.palette: \"#1234XX\" is neither a color nor a palette")

expect.err(try(rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="#1234XX")),
           "box.palette: \"#1234XX\" is neither a color nor a palette")

expect.err(try(rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="B")),
           "pal=\"B\" is ambiguous")

expect.err(try(rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="NA")),
           "pal=\"NA\" is not allowed")

expect.err(try(rpart.plot.version1(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette=rpart.plot)),
           "object of type 'closure' is not subsettable")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette=c(0, 1, 2), main="box.palette=c(0,1,2)")),
           "box.palette: 0 is neither a color nor a palette")

rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette=c(2,3,4), main="box.palette=c(2,3,4)")

# multiclass responses
mod.class <- rpart(pclass~., data=ptitanic, control=list(cp=.02))
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot.version1(mod.class, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto",
                    main="multiclass\n(2 of 3 classes predicted)", trace=1)
fancyRpartPlot(mod.class, main="fancyRpartPlot", sub="")
rpart.plot         (mod.class, type=2, extra="auto", fallen.leaves=TRUE, box.palette=list("Blues", "Reds", "Greens"),
           main="list(Blues, Reds, Greens)", trace=2)
# expect warning: The box palettes have been reassigned
# Actually, I removed the warning (in the rpart.plot source code) because it causes confusion
rpart.plot(mod.class, type=2, extra="auto", fallen.leaves=TRUE, box.palette=list("Blues", "Reds"), main="list(Blues, Reds)")

data(iris)
mod.species <- rpart(Species~., data=iris, cp=1e-10)
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass\n(3 of 4 classes predicted)")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto", main="multiclass, tweak=1.1", tweak=1.1)
# expect warning: all boxes will be white
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette=list("Reds", "Grays"), main="list(Reds, Grays)\nall boxes white")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette=0, main="box.palette=0")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.col=2:4, main="mod.species\nbox.col=2:4")
prp(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="mod.continuous.survived\nc(\"Red\", \"Yellow\", \"Green\")",  box.palette=c("Red", "Yellow", "Green"))
prp(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="mod.continuous.survived\nc(\"Green\", \"Yellow\", \"Red\")",  box.palette=c("Green", "Yellow", "Red"))

par(mfrow = c(1, 2))
mod.country <- rpart(Country ~ ., cu.summary)
# expect warning: The box palettes have been reassigned
# Actually, I removed the warning (in the rpart.plot source code) because it causes confusion
rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass\n(4 of 10 classes predicted)")
fancyRpartPlot(mod.country, main="fancyRpartPlot", sub="") # fancyRpartPlot gets colors wrong
rv <- rpart.plot         (mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass\nbox.palette=\"auto\"", box.palette="auto")
rpart.plot(mod.country,
    type=2, extra="auto", fallen.leaves=TRUE,
    main="box.palette=\nlist(\"Greens\", \"Reds\", \"Grays\", \"Purples\")",
    box.palette=list("Greens", "Reds", "Grays", "Purples"))

rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass, trace=-1", box.palette="auto", trace=-1)
rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass, box.palette=0", box.palette=0)
rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=FALSE, box.palette="auto", main="multiclass, fallen.leaves=FALSE", trace=-1)
# expect error: box.palette must be 0 or "auto" or a list of palettes like ...
expect.err(try(rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, box.palette="Blues")),
           "The rpart model has a multiclass response")

# poisson model
library(earth)
data(ozone1)
ozone2 <- ozone1
set.seed(8)
ozone2$O3a <- round(runif(330, 1, 10))
y <- cbind(ozone2$O3, ozone2$O3a)
mod.poisson <- rpart(y~.-O3-O3a, data=ozone2, control=list(cp=.04))
par(mfrow=c(2, 2))
# plot(mod.poisson, unif=TRUE, branch=.3, main="poisson\n"); text(mod.poisson, use.n=TRUE, all=T, digits=3, xpd=NA, cex=1.1)
rpart.plot    (mod.poisson, type=2, extra="auto", fallen.leaves=TRUE, main="poisson\ndefault args", box.palette="auto")
fancyRpartPlot(mod.poisson, main="fancyRpartPlot", sub="")
rpart.plot    (mod.poisson, type=2, extra="auto", fallen.leaves=TRUE, main="poisson\nbox.palette=RdYlGn", box.palette="RdYlGn")
rpart.plot    (mod.poisson, type=2, extra="auto", fallen.leaves=TRUE, main="poisson\nbox.palette=BuGn", box.palette="BuGn")

# compare "auto" to "-auto"

par(mfrow=c(2, 2))
rpart.plot(mod.continuous.survived, type=2, extra="auto",  main="auto\nmod.continuous.survived",  box.palette="auto")
rpart.plot(mod.continuous.survived, type=2, extra="auto", main="-auto\nmod.continuous.survived", box.palette="-auto")

rpart.plot(mod.age,     type=2, extra="a",  main="auto\nmod.age",  box.palette="auto")
rpart.plot(mod.age,     type=2, extra="a", main="-auto\nmod.age", box.palette="-auto")

par(mfrow=c(2, 2))
rpart.plot(mod.class,   type=2, extra="au",  main="auto\nmod.class",  box.palette="auto")
rpart.plot(mod.class,   type=2, extra="au", main="-auto\nmod.class", box.palette="-auto")

rpart.plot(mod.poisson, type=2, extra="AUTO",  main="auto\nmod.poisson",  box.palette="auto")
rpart.plot(mod.poisson, type=2, extra="AUTO", main="-auto\nmod.poisson", box.palette="-auto")

par(mfrow=c(2, 2))
rpart.plot(mod.species, type=2, extra="A",  main="auto\nmod.species",  box.palette="auto")
rpart.plot(mod.species, type=2, extra="A", main="-auto\nmod.species", box.palette="-auto")

# TODO we shouldn't really accept extra="axxx" as "auto" (not serious)
rpart.plot(mod.country, type=2, extra="axxx",  main="auto\nmod.country",  box.palette="auto")
rpart.plot(mod.country, type=2, extra="axxx", main="-auto\nmod.country", box.palette="-auto")

# test legend.x and legend.y and legend.cex
par(mfrow=c(2,2))
rpart.plot(multi.class.model, legend.x=NULL, main="legend.x=NULL (default)")
rpart.plot(multi.class.model, legend.x=NA, main="legend.x=NA (no legend)")
rpart.plot(multi.class.model, fallen.leaves=TRUE, legend.x=.8, main="legend.x=.8")
rpart.plot(multi.class.model, legend.x=-.1, legend.cex=1.4, main="legend.x=-.1, legend.cex=1.4")

par(mfrow=c(2,2))
rpart.plot(multi.class.model, legend.y=NULL, main="legend.y=NULL (default)")
rpart.plot(multi.class.model, fallen.leaves=FALSE, legend.y=NA, main="legend.y=NA (no legend)")
rpart.plot(multi.class.model, fallen.leaves=FALSE, legend.y=.16, main="legend.y=.16")
rpart.plot(multi.class.model, legend.x=0, legend.y=1.1, legend.cex=.8,
           main="legend.x=0 legend.y=1.1 legend.cex=.8")

cat("== done test.palette.R ==\n")
