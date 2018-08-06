# rpart.predict.R:

rpart.predict <- function(object, newdata,
         type = c("vector", "prob", "class", "matrix"),
         na.action = na.pass,
         nn=FALSE, rules=FALSE, ...)
{
    # predict.rpart uses missing(), so deal with that
    missing.newdata <- missing(newdata)
    missing.type <- missing(type)
    pred <-
        if(missing.newdata && missing.type)
            predict(object=object, na.action=na.action) # invokes predict.rpart
        else if(missing.newdata)
            predict(object=object, type=type, na.action=na.action)
        else if(missing.type)
            predict(object=object, newdata=newdata, na.action=na.action)
        else
            predict(object=object, newdata=newdata, type=type, na.action=na.action)

    want.nn <- check.boolean(nn)
    want.rules <- check.boolean(rules)
    if(want.nn || want.rules) {
        type <- match.arg(type)
        pred <- append.to.pred(pred, object, newdata, type, na.action,
                               want.nn, want.rules, missing.newdata, ...)
    }
    pred
}
append.to.pred <- function(pred, object, newdata, type, na.action,
                           want.nn, want.rules, missing.newdata, ...)
{
    # following based on code in predict.rpart (july 2018, rpart version 4.1-13)
    where <-
        if(missing.newdata)
            object$where
         else {
             if(is.null(attr(newdata, "terms"))) {
                 Terms <- delete.response(object$terms)
                 newdata <- model.frame(Terms, newdata, na.action = na.action,
                                        xlev = attr(object, "xlevels"))
                 if(!is.null(cl <- attr(Terms, "dataClasses")))
                    .checkMFClasses(cl, newdata, TRUE)
             }
             newdata <- getFromNamespace("rpart.matrix", ns="rpart")(newdata)
             getFromNamespace("pred.rpart", ns="rpart")(object, newdata)
         }
    names <- names(pred)
    pred <- as.data.frame(pred) # pred may be a vec or mat, convert to data.frame
    if(ncol(pred) == 1) {
        # column name is "pred", change it to something more useful
        colnames(pred) <-
            if(type == "vector")
                get.response.name(object, trace=0)
            else
                type
    }
    if(!is.null(names)) # true if original pred was a vector
        rownames(pred) <- names
    if(want.nn)
        pred$nn <- as.numeric(rownames(object$frame)[where])
    if(want.rules) {
        pred$rule <- rpart.rules(object, RPART.PREDICT=TRUE, WHERE=where, ...)
        # delete column name for rules because it's on the far right, looks odd
        colnames(pred) <- c(colnames(pred)[1:(length(colnames(pred))-1)], "")
    }
    pred
}
