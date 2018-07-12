# rpart.terms.R: The code that accesses the rpart model's terms and data.frame.
#
# Includes processing the roundint argument and get.is.logical.
# We have to be robust against badly formed rpart models, because some
# existing packages call prp internally with badly formed models.

# return a single NA, or a logical vec, element names are varnames
get.is.roundint <- function(obj)
{
    # "model" was initialized by rpart.model.frame() earlier
    # will be NA if model.frame not available
    model.frame <- obj[["model"]]
    if(identical(model.frame, NA) || is.null(model.frame))
        return(NA)
    # sanity check in case someone passes an illegal model to prp
    if(length(dim(model.frame)) != 2 || NROW(model.frame) < 1 || NCOL(model.frame) < 2) {
        warning0(
"Illegal model.frame (to make this warning go away, use roundint=FALSE)")
        return(NA)
    }
    varnames <- colnames(model.frame)
    if(length(varnames) < 2) {
        warning0(
"Illegal model.frame (to make this warning go away, use roundint=FALSE)")
        return(NA)
    }
    varnames <- varnames[varnames != get.response.name(obj, trace=0)]
    is.roundint <- repl(FALSE, length(varnames))
    names(is.roundint) <- varnames
    for(varname in varnames) {
        values <- model.frame[[varname]]
        if((is.numeric(values) || is.logical(values)) && is.integral(values))
            is.roundint[varname] <- TRUE
    }
    is.roundint # logical vector, element names are varnames
}
# return a single NA, or a logical vec with an entry for each split
get.is.logical <- function(x, splits, trace)
{
    terms <- try(terms(x), silent=TRUE)
    if(is.try.err(terms)) {
        # only issue a message if tracing because you can't turn this off (unlike roundint)
        trace1(trace, "%s (therefore cannot determine is.logical for the variables)\n",
               attr(terms,"condition")[[1]])
        return(NA)
    }
    dataClasses <- attr(terms, "dataClasses")
    if(!is.character(dataClasses)) {
        trace1(trace,
"Cannot get dataClasses (therefore cannot determine is.logical for the variables)\n")
        return(NA)
    }
    rownames <- rownames(splits)
    is.logical <- repl(FALSE, NROW(splits))
    first.time <- TRUE
    for(irow in 1:NROW(splits)) {
        classname <- dataClasses[rownames[irow]][1]
        if(!is.na(classname)) {
            if(classname == "logical")
                is.logical[irow] <- TRUE
        } else if(trace > 0 && first.time) {
            first.time <- FALSE
            # get here in "dfit" in Section 4 in slowtests/rpart.report.R
            # (because classname is "nmatrix.24")
            printf(
"Variable '%s' in splits is not in dataClasses (therefore cannot determine is.logical for the variable)\nterms$dataClasses:\n",
                   rownames[irow])
            print(dataClasses)
            cat0("\n")
        }
    }
    is.logical
}
# TODO This ignores the subset arg (which isn't an issue for the way we use this function)
rpart.model.frame <- function(obj, env, caller) # return NA if can't get the model frame
{
    stopifnot(inherits(obj, "rpart"))
    # if the rpart model was built with model=TRUE, then model frame is already saved
    if(!is.null(obj[["model"]]))
        return(obj[["model"]])
    goaway <-
"\n         To make this warning go away:\n             Call %s with roundint=FALSE,\n             or rebuild the rpart model with model=TRUE."
    goaway <- sprint(goaway, caller)
    form <- try(formula(obj), silent=TRUE)
    if(is.try.err(form)) {
        warning0("Invalid formula in rpart model (", attr(form,"condition")[[1]], ").", goaway)
        return(NA)
    }
    if(!inherits(form, "formula")) {
        warning0("The 'formula' field in the rpart model is not actually a formula).", goaway)
        return(NA)
    }
    if(!inherits(env, "environment")) {
        warning0("Cannot retrieve the environment used to build the model.", goaway)
        return(NA)
    }
    call <- try(getCall(obj), silent=TRUE)
    if(is.try.err(call)) {
        warning0("getCall failed for rpart model (", attr(call,"condition")[[1]], ")", goaway)
        return(NA)
    }
    if(!inherits(call, "call")) {
        warning0("Bad 'call' field in rpart model.", goaway)
        return(NA)
    }
    data <- call[["data"]]
    if(!is.null(data)) { # null if data arg not used in original call to rpart
        if(!inherits(data, "name")) {
            warning0("Bad 'data' field in model 'call' field.", goaway)
            return(NA)
        }
        data <- try(eval(data, envir=env), silent=TRUE)
        # will be a try error here if legitimate model but data no longer available
        if(is.try.err(data) ||
                length(dim(data)) != 2 || NROW(data) < 1 || NCOL(data) < 2) {
            warning0("Cannot retrieve the model data (will ignore roundint=TRUE).",
                     goaway)
            return(NA)
        }
    }
    # call model.frame.default
    mf <- try(stats::model.frame(formula=form, data=data, na.action="na.pass"),
                                 silent=TRUE)
    if(is.try.err(mf)) {
        warning0("Cannot retrieve the model.frame (", attr(mf,"condition")[[1]], ")",
                 goaway)
            return(NA)
    }
    if(length(dim(mf)) != 2 || NROW(mf) < 1 || NCOL(mf) < 2) {
        warning0(
"Cannot retrieve the model.frame used to build the model (will ignore roundint=TRUE).",
                 goaway)
        return(NA)
    }
    mf
}
get.response.name <- function(obj, trace) # return a string ("response" if something fails)
{
    terms <- try(terms(obj), silent=TRUE)
    if(is.try.err(terms)) {
        # only issue a message if tracing because you can't turn this off (unlike roundint)

        trace1(trace, "%s (therefore cannot get response name)\n\n",
               attr(terms,"condition")[[1]])
        return("response")
    }
    iresponse <- attr(terms, "response")
    if(is.null(iresponse)) {
        trace1(trace, "attr(terms, \"response\") failed (therefore cannot get response name)\n")
        return("response")
    }
    dataClasses <- attr(terms, "dataClasses")
    if(!is.character(dataClasses)) {
        trace1(trace, "Cannot get dataClasses (therefore cannot get response name)\n")
        return("response")
    }
    varnames <- names(dataClasses)
    if(is.null(varnames)) {
        trace1(trace, "Cannot get names(dataClasses), therefore cannot get response name\n")
        return("response")
    }
    check.index(iresponse, "attr(terms(obj), \"response\")", varnames,
                allow.negatives=FALSE)
    varnames[iresponse]
}
