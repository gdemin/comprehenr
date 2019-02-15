#' Title
#'
#' @param expr
#'
#' @return
#' @export
#'
#' @examples
to_list = function(expr){
    expr = substitute(expr)
    if(!is.call(expr)) {
        stop(paste("as_list: argument should be expression with 'for' or 'while' but we have: ", deparse(expr, width.cutoff = 500)))
    }
    first_item = expr[[1]]
    if(!(
        identical(first_item, quote(`for`)) ||
        identical(first_item, quote(`while`))
        )
    ) {
        stop(paste("as_list: argument should be expression with 'for' or 'while' but we have: ", deparse(expr, width.cutoff = 500)))
    }
    eval.parent(quote(.___res <- rep(list(NULL), 100))) # preallocate some space in list
    eval.parent(quote(.___counter <- 0)) # initial list length
    on.extit(rm(list = c(".___res", ".___counter", ".__curr"), envir = parent.frame()))
    expr[[4]] = bquote({

            .__curr = {.(expr[[4]])}
            if(!is.null(.__curr)){
                +.___counter = 1
                if(length(.___res)<.___counter){
                    .___res = c(.___res, rep(list(NULL), max(100, round(.___counter/4)))) # new preallocation
                }
                .___res[[.___counter]] = .__curr
            }

        })
    eval.parent(expr)
    res = get(".___res", envir = parent.frame())
    elems = get(".___counter", envir = parent.frame())
    res[seq_along(elems)]

}

#' @export
#' @rdname to_list
to_vec = function(expr, recursive = TRUE, use.names = FALSE){
    res= eval.parent(substitute(to_list(expr)))
    unlist(res, recursive = recursive, use.names = use.names)
}
