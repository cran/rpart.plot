# rpart.model.frame.R: The code that accesses the rpart model's model.frame
#
# Includes processing the roundint argument and islogical determination
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
    isbinary <- list(islogical=NA, is01=NA)
    if(!identical(model.frame, NA))
        isbinary <- isbinary(obj, model.frame, trace)
    isroundint <- NA
    if(roundint && !identical(model.frame, NA))
        isroundint <- isroundint(model.frame, trace, caller)
    list(response.name = response.name,
         islogical     = isbinary$islogical, # NA or a logical vec, elem for each split
         is01          = isbinary$is01,      # NA or a logical vec, elem for each split
         isroundint    = isroundint)         # NA or a logical vec, elem names are varnames
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
    response.name <- varnames[iresponse]
    if(substr(response.name, 1, 5) == "Surv(") # response is a survival object?
        response.name <- "Surv" # so rules not pushed far to the right with long response.name
    response.name
}
# islogical is true if is.logical is TRUE for the variable
# is01      is true if the variable only has values 0 and 1
# return a single NA, or a logical vec with an element for each split
isbinary <- function(obj, model.frame, trace)
{
    rownames <- rownames(obj$splits)
    if(length(rownames) == 0)   # null model, no splits
        return(list(islogical=NA, is01=NA))
    islogical <- repl(FALSE, length(rownames))
    is01      <- repl(FALSE, length(rownames))
    for(irow in 1:length(rownames)) {
        x <- model.frame[[rownames[irow]]]
        if(!is.null(x)) {       # rowname is in model.frame?
            islogical[irow] <- is.logical(x)
            u <- sort(unique(x))
            is01[irow] <-
                (length(u) == 1 && (u == 0 || u == 1)) ||
                (length(u) == 2 && (u[1] == 0 && u[2] == 1))
        } else if(trace > 0) {  # rowname not in model.frame
            # get here in e.g. model dfit in slowtests/rpart.report.R
            trace <- 0 # prevent further messages
            printf(
"Variable '%s' in splits is not in the model.frame (so cannot determine roundint and is.binary for the variable)\n",
                   rownames[irow])
            printf("colnames(model.frame): %s\n", paste.collapse(colnames(model.frame)))
        }
    }
    list(islogical=islogical, is01=is01)
}
# return a single NA, or a logical vec, element names are varnames.
isroundint <- function(model.frame, trace, caller)
{
    varnames <- colnames(model.frame)
    isroundint <- repl(FALSE, length(varnames))
    names(isroundint) <- varnames
    for(varname in varnames) {
        x <- model.frame[[varname]] # will be NULL if varname not in model.frame
        isroundint[varname] <- (is.numeric(x) || is.logical(x)) && is.integral(x)
    }
    if(trace >= 2) {
        varnames <- names(isroundint)[isroundint]
        if(length(varnames))
            cat("will apply roundint to the following variables in splits:",
                varnames, "\n")
        else
            cat("will apply roundint to no variables\n")
    }
    isroundint # logical vector, element names are varnames
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
        if(inherits(data, "name")) {
            data <- try(eval(data, envir=env), silent=TRUE)
            # will be a try error here if legitimate model but data no longer available
            if(is.try.err(data) ||
                    length(dim(data)) != 2 ||
                    NROW(data) < 1 || NCOL(data) < 2) {
                msg("Cannot retrieve the data used to build the model ",
                    "(so cannot determine roundint and is.binary for the variables)")
                return(NA)
            }
        } else if(!is.data.frame(data) && !is.matrix(data)) {
            msg("Bad 'data' field in model 'call' (expected a data.frame or a matrix)")
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
    if(trace >= 3) {
        old.width <- options(width=1e4)$width
        on.exit(options(width=old.width))
        printf("head(model.frame):\n")
        print(head(model.frame))
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
# This was supplanted by isbinary because I wanted to treat roundint
# and islogical more uniformly (using the model.frame not dataClasses)
#
# isbinary.from.dataClasses <- function(obj, trace)
# {
#     terms <- try(terms(obj), silent=TRUE)
#     if(is.try.err(terms)) {
#         trace1(trace, "%s (therefore cannot determine islogical for the variables)\n",
#                attr(terms,"condition")[[1]])
#         return(NA)
#     }
#     dataClasses <- attr(terms, "dataClasses")
#     if(!is.character(dataClasses)) {
#         trace1(trace,
# "Cannot get dataClasses (therefore cannot determine islogical for the variables)\n")
#         return(NA)
#     }
#     rownames <- rownames(obj$splits)
#     if(length(rownames) == 0) # null model, no splits
#         return(NA)
#     islogical <- repl(FALSE, length(rownames))
#     first.time <- TRUE
#     for(irow in 1:length(rownames)) {
#         classname <- dataClasses[rownames[irow]][1]
#         if(!is.na(classname)) {
#             if(classname == "logical")
#                 islogical[irow] <- TRUE
#         } else if(trace > 0 && first.time) {
#             first.time <- FALSE
#             # get here in "dfit" in Section 4 in slowtests/rpart.report.R
#             printf(
# "Variable '%s' in splits is not in dataClasses (therefore cannot determine islogical for the variable)\nterms$dataClasses:\n",
#                    rownames[irow])
#             print(dataClasses)
#             cat0("\n")
#         }
#     }
#     islogical
# }
