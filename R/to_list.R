#' List comprehensions for R
#'
#' \code{to_list} converts usual R loops expressions to list producers.
#' Expression should be started with \code{for} ,  \code{while} or
#' \code{repeat}. You can iterate over multiple lists if you provide several
#' loop variables in backticks. See examples.
#' @param expr expression which starts with \code{for} ,  \code{while} or \code{repeat}.
#' @param recursive	logical. Should unlisting be applied to list components of result? See \link[base]{unlist} for details.
#' @param use.names logical. Should names be preserved? See \link[base]{unlist} for details.
#' @return list for \code{to_list} and vector for \code{to_vec}
#' @export
#'
#' @examples
#' # rather useless expression - squares of even numbers
#' to_list(for(i in 1:10) if(i %% 2==0) i*i)
#'
#' # Pythagorean triples
#' to_list(for (x in 1:30) for (y in x:30) for (z in y:30) if (x^2 + y^2 == z^2) c(x, y, z))
#'
#' colours = c("red", "green", "yellow", "blue")
#' things = c("house", "car", "tree")
#' to_vec(for(x in colours) for(y in things) paste(x, y))
#'
#' # prime numbers
#' noprimes = to_vec(for (i in 2:7) for (j in seq(i*2, 99, i)) j)
#' primes = to_vec(for (x in 2:99) if(!x %in% noprimes) x)
#' primes
#'
#' # iteration over multiple lists
#' to_vec(for(`i, j` in numerate(letters)) if(i %% 2==0) paste(i, j))
#'
#' set.seed(123)
#' rand_sequence = runif(20)
#' # gives only locally increasing values
#' to_vec(for(`i, j` in lag_list(rand_sequence)) if(j>i) j)
to_list = function(expr){
    expr = substitute(expr)
    if(!is_loop(expr)) {
        stop(paste("argument should be expression with 'for', 'while' or 'repeat' but we have: ", deparse(expr, width.cutoff = 500)[1]))
    }

    expr = expand_loop_variables(expr)
    expr = add_assignment_to_final_loops(expr)
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


add_assignment_to_final_loops = function(expr){
    if(is_loop(expr)){
        if(has_loop_inside(expr[-1])){
            expr[-1] = as.call(lapply(as.list(expr)[-1], add_assignment_to_final_loops))
        } else {
            expr = add_assignment_to_loop(expr)
        }
    } else {
        if(has_loop_inside(expr)){
            expr = as.call(lapply(as.list(expr), add_assignment_to_final_loops))
        }
    }
    expr
}

expand_loop_variables = function(expr){
    if(!is.call(expr)) return(expr)
    if(is_for_loop(expr)){
            expr = add_expansion_to_loop(expr)
            expr[-1] = as.call(lapply(as.list(expr)[-1], expand_loop_variables))
    } else {
        expr = as.call(lapply(as.list(expr), expand_loop_variables))
    }
    expr
}

add_expansion_to_loop = function(expr){

    loop_variable = expr[[2]]
    if(is.symbol(loop_variable) && grepl(",", as.character(loop_variable))){
        all_loop_variables = trimws(unlist(strsplit(as.character(loop_variable), split = ",")))
        expansion_expr = lapply(seq_along(all_loop_variables), function(item_num){
            curr_item = as.name(all_loop_variables[item_num])
            bquote(.(curr_item) <- .(loop_variable)[[.(item_num)]])
        })
        expansion_expr = as.call(c(list(quote(`{`)), expansion_expr))
        last_item = length(expr)
        expr[[last_item]] = bquote({
            .(expansion_expr)
            .(expr[[last_item]])
        })
    }
    expr
}

is_loop = function(expr){
    if(!is.call(expr)) return(FALSE)
    first_item = expr[[1]]
    identical(first_item, quote(`for`)) ||identical(first_item, quote(`while`)) ||identical(first_item, quote(`repeat`))
}

is_for_loop = function(expr){
    if(!is.call(expr)) return(FALSE)
    first_item = expr[[1]]
    identical(first_item, quote(`for`))
}

has_loop_inside = function(expr){
    if(!is.call(expr)) return(FALSE)
    if(is_loop(expr)) return(TRUE)
    any(
        unlist(
            lapply(as.list(expr), has_loop_inside),
            recursive = TRUE,
            use.names = FALSE
        ))
}


add_assignment_to_loop = function(expr){
    last_item = length(expr)
    expr[[last_item]] = bquote({

        .__curr = {.(expr[[last_item]])}
        if(!is.null(.__curr)){
            .___counter = .___counter + 1
            .___res[[.___counter]] = .__curr
        }

    })
    expr
}
