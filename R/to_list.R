#' List comprehensions for R
#'
#' @param expr expression which starts with \code{for} or \code{while}
#' @param recursive	logical. Should unlisting be applied to list components of result? See \link[base]{unlist} for details.
#' @return list for \code{to_list} and vector for \code{to_vec}
#' @export
#'
#' @examples
to_list = function(expr){
    expr = substitute(expr)
    if(!is.call(expr)) {
        stop(paste("to_list: argument should be expression with 'for' or 'while' but we have: ", deparse(expr, width.cutoff = 500)[1]))
    }
    first_item = expr[[1]]
    if(!(
        identical(first_item, quote(`for`)) ||
        identical(first_item, quote(`while`))
    )
    ) {
        stop(paste("to_list: argument should be expression with 'for' or 'while' but we have: ", deparse(expr, width.cutoff = 500)[1]))
    }
    last_item = length(expr)
    expr[[last_item]] = bquote({

        .__curr = {.(expr[[last_item]])}
        if(!is.null(.__curr)){
            .___counter = .___counter + 1
            .___res[[.___counter]] = .__curr
        }

    })
    on.exit(suppressWarnings(rm(list = c(".___res", ".___counter", ".__curr"), envir = parent.frame())))
    eval.parent(quote(.___res <- list()))
    eval.parent(quote(.___counter <- 0)) # initial list length
    eval.parent(expr)
    res = get(".___res", envir = parent.frame())
    res

}

#' @export
#' @rdname to_list
to_vec = function(expr, recursive = TRUE, use.names = FALSE){
    res= eval.parent(substitute(to_list(expr)))
    unlist(res, recursive = recursive, use.names = use.names)
}









