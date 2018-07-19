# rpart.model.frame.R: The code that accesses the rpart model's model.frame
#
# Includes processing the roundint argument and is.logical determination
# for the variables.
# We have to semi-gracefully handle the situation where the data used
# to build the model is not longer available.
# We also have to be robust against badly formed rpart models, because some
# existing packages internally call prp with badly formed or partial rpart models.

get.modelframe.info <- function(obj, roundint, trace, env, caller)
{
    roundint <- check.boolean(roundint)
    trace <- as.numeric(check.numeric.scalar(trace, logical.ok=TRUE))
    response.name <- get.response.name(obj, trace)
    model.frame <- rpart.model.frame(obj, roundint, trace, env, caller)
    is.logical <- NA
    if(!identical(model.frame, NA))
        is.logical <- get.is.logical(obj, model.frame, trace)
    is.roundint <- NA
    if(roundint && !identical(model.frame, NA))
        is.roundint <- get.is.roundint(model.frame, trace, caller)
    list(response.name = response.name,
         is.logical    = is.logical,  # NA or a logical vec, elem for each split
         is.roundint   = is.roundint) # NA or a logical vec, elem names are varnames
}
get.response.name <- function(obj, trace) # return a string
{
    terms <- try(terms(obj), silent=TRUE)
    if(is.try.err(terms)) {
        if(trace > 0)
            cat0(attr(terms,"condition")[[1]],
                 " (therefore cannot get response name)\n\n")
        return("response")
    }
    iresponse <- attr(terms, "response")
    if(is.null(iresponse)) {
        if(trace > 0)
            cat0("attr(terms, \"response\") failed ",
                 "(therefore cannot get response name)\n")
        return("response")
    }
    dataClasses <- attr(terms, "dataClasses")
    if(!is.character(dataClasses)) {
        if(trace > 0)
            printf("Cannot get dataClasses (therefore cannot get response name)\n")
        return("response")
    }
    varnames <- names(dataClasses)
    if(is.null(varnames)) {
        if(trace > 0)
            printf("Cannot get names(dataClasses), therefore cannot get response name\n")
        return("response")
    }
    check.index(iresponse, "attr(terms(obj), \"response\")", varnames,
                allow.negatives=FALSE)
    varnames[iresponse]
}
# return a single NA, or a logical vec with an element for each split
get.is.logical <- function(obj, model.frame, trace)
{
    rownames <- rownames(obj$splits)
    if(length(rownames) == 0) # null model, no splits
        return(NA)
    is.logical <- repl(FALSE, length(rownames))
    for(irow in 1:length(rownames)) {
        x <- model.frame[[rownames[irow]]]
        if(!is.null(x))      # rowname is in model.frame?
            is.logical[irow] <- is.logical(x)
        else if(trace > 0) { # rowname not in model.frame
            # get here in e.g. model dfit in slowtests/rpart.report.R
            trace <- 0 # prevent further messages
            printf(
"Variable '%s' in splits is not in the model.frame (so cannot determine is.logical for the variable)\n",
                   rownames[irow])
            printf("colnames(model.frame): %s\n", paste.collapse(colnames(model.frame)))
        }
    }
    is.logical
}
# return a single NA, or a logical vec, element names are varnames.
get.is.roundint <- function(model.frame, trace, caller)
{
    varnames <- colnames(model.frame)
    is.roundint <- repl(FALSE, length(varnames))
    names(is.roundint) <- varnames
    for(varname in varnames) {
        x <- model.frame[[varname]] # will be NULL if varname not in model.frame
        is.roundint[varname] <- (is.numeric(x) || is.logical(x)) && is.integral(x)
    }
    if(trace >= 2) {
        varnames <- names(is.roundint)[is.roundint]
        if(length(varnames))
            cat("will apply roundint to the following variables in splits:",
                varnames, "\n")
        else
            cat("will apply roundint to no variables\n")
    }
    is.roundint # logical vector, element names are varnames
}
# Return NA if can't get the model frame.
# We issue warnings only if roundint is TRUE (this allows the user to silence the warnings).
# TODO This ignores the subset arg (but that isn't an issue for the way we use this function).
rpart.model.frame <- function(obj, roundint, trace, env, caller)
{
    # init msg function to either issue a warning or a trace message
    if(roundint) {
        to.silence <- paste0(".\nTo silence this warning:\n",
                             "    Call ", caller, " with roundint=FALSE,\n",
                             "    or rebuild the rpart model with model=TRUE.")
        msg <- function(...) { warning0(..., to.silence) }
    } else
        msg <- function(...) { if(trace > 0) cat0(..., "\n") }
    stopifnot(inherits(obj, "rpart"))
    # if the rpart model was built with model=TRUE, then model frame is already saved
    if(!is.null(obj[["model"]])) {
        model.frame <- obj[["model"]]
        if(length(dim(model.frame)) != 2 ||
                NROW(model.frame) < 1 || NCOL(model.frame) < 2) {
            msg("Bad 'model' field saved with the rpart model (bad dimensions)")
            return(NA)
        }
        if(length(colnames(model.frame)) < 2) { # response and at least one predictor
            msg("Bad 'model' field saved with the rpart model (bad colnames)")
            return(NA)
        }
        return(model.frame)
    }
    form <- try(formula(obj), silent=TRUE)
    if(is.try.err(form)) {
        msg("Invalid formula in rpart model (", attr(form,"condition")[[1]], ")")
        return(NA)
    }
    if(!inherits(form, "formula")) {
        msg("The 'formula' field in the rpart model is not actually a formula)")
        return(NA)
    }
    if(!inherits(env, "environment")) {
        msg("Cannot retrieve the environment used to build the model")
        return(NA)
    }
    call <- try(getCall(obj), silent=TRUE)
    if(is.try.err(call)) {
        msg("getCall failed for rpart model (", attr(call,"condition")[[1]], ")")
        return(NA)
    }
    if(!inherits(call, "call")) {
        msg("Bad 'call' field in rpart model")
        return(NA)
    }
    data <- call[["data"]]
    if(!is.null(data)) { # null if data arg not used in original call to rpart
        if(!inherits(data, "name")) {
            msg("Bad 'data' field in model 'call'")
            return(NA)
        }
        data <- try(eval(data, envir=env), silent=TRUE)
        # will be a try error here if legitimate model but data no longer available
        if(is.try.err(data) ||
                length(dim(data)) != 2 ||
                NROW(data) < 1 || NCOL(data) < 2) {
            msg("Cannot retrieve the data used to build the model ",
                "(so cannot determine roundint and is.logical for the variables)")
            return(NA)
        }
    }
    # call model.frame.default
    model.frame <- try(stats::model.frame(formula=form, data=data, na.action="na.pass"),
                                 silent=TRUE)
    if(is.try.err(model.frame)) {
        msg("Cannot retrieve the data used to build the model (model.frame: ",
            attr(model.frame,"condition")[[1]], ")")
       return(NA)
    }
    if(length(dim(model.frame)) != 2 ||
            NROW(model.frame) < 1 || NCOL(model.frame) < 2) {
        msg("Cannot retrieve the data used to build the model ",
            "(bad model.frame dimensions)")
        return(NA)
    }
    if(length(colnames(model.frame)) < 2) { # response and at least one predictor
        msg("Cannot retrieve the data used to build the model (bad colnames)")
        return(NA)
    }
    model.frame
}
# This was supplanted by get.is.logical because I wanted to treat roundint
# and is.logical more uniformly (using the model.frame not dataClasses)
#
# get.is.logical.from.dataClasses <- function(obj, trace)
# {
#     terms <- try(terms(obj), silent=TRUE)
#     if(is.try.err(terms)) {
#         trace1(trace, "%s (therefore cannot determine is.logical for the variables)\n",
#                attr(terms,"condition")[[1]])
#         return(NA)
#     }
#     dataClasses <- attr(terms, "dataClasses")
#     if(!is.character(dataClasses)) {
#         trace1(trace,
# "Cannot get dataClasses (therefore cannot determine is.logical for the variables)\n")
#         return(NA)
#     }
#     rownames <- rownames(obj$splits)
#     if(length(rownames) == 0) # null model, no splits
#         return(NA)
#     is.logical <- repl(FALSE, length(rownames))
#     first.time <- TRUE
#     for(irow in 1:length(rownames)) {
#         classname <- dataClasses[rownames[irow]][1]
#         if(!is.na(classname)) {
#             if(classname == "logical")
#                 is.logical[irow] <- TRUE
#         } else if(trace > 0 && first.time) {
#             first.time <- FALSE
#             # get here in "dfit" in Section 4 in slowtests/rpart.report.R
#             printf(
# "Variable '%s' in splits is not in dataClasses (therefore cannot determine is.logical for the variable)\nterms$dataClasses:\n",
#                    rownames[irow])
#             print(dataClasses)
#             cat0("\n")
#         }
#     }
#     is.logical
# }
