# test.imports.R: test packages that import or suggest rpart.plot

library(rpart.plot)
data(ptitanic)
library(earth)
data(ozone1)
sessionInfo()
ititanic <- ptitanic
ititanic$survived <- as.integer(ititanic$survived == "survived")
options(warn=1) # print warnings as they occur

library(DStree)
par(mfrow=c(3,3))
example(plot.DStree)
plot(fit, prob="surv", box.palette="auto", main="DStree\nbox.palette=\"auto\"")
plot(fit, prob="surv", type=1, nn=TRUE, yesno=2, box.palette="Oranges",
     main="DStree\ntype=1, nn=TRUE, yesno=2\nbox.palette=\"Oranges\"", cex.main=.9)
