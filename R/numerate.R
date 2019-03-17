#' Auxiliary functions for working with lists
#'
#' \itemize{
#' \item{numerate}{ returns list of lists. Each list consists of two elements:
#' sequential number of element and element. Reverse operation -
#' \code{unumerate}.}
#' \item{mark}{ returns list of lists. Each list consists of two elements: name of element and element. Reverse operation -
#' \code{unmark}.}
#' \item{zip_lists}{ combine lists side-by-sidy. Reverse operation - \code{unzip_list}.}
#' \item{unzip_list}{ It's similair to matrix transposition but for list of lists.}
#' \item{lag_list}{ convert argument to list of arguments with previous values. x -> list(x[i-1], x[i]).}
#' }
#'
#' @param x list, vector or list of lists
#' @param item numeric number of list in which stored values
#' @param ... lists which will be zipped
#'
#' @return list or list of lists
#' @export
#'
#' @examples
#' cities = c('Chicago', 'Detroit', 'Atlanta')
#' airports = c('ORD', 'DTW', 'ATL')
#' pairs = zip_lists(cities, airports)
#'
#' str(pairs)
#' str(unzip_list(pairs))
#'
#' str(numerate(cities))
#'
#' named_list = c('Chicago' = 'ORD', 'Detroit' = 'DTW', 'Atlanta' = 'ATL')
#' str(mark(named_list))
#'
#' set.seed(123)
#' rand_sequence = runif(20)
#' # gives only locally increasing values
#' to_vec(for(`i, j` in lag_list(rand_sequence)) if(j>i) j)
numerate = function(x){
    unzip_list(list(seq_along(x), x))
}

#' @export
#' @rdname numerate
enumerate = numerate

#' @export
#' @rdname numerate
unnumerate = function(x, item = 2){
    unzip_list(x)[[item]]
}

#' @export
#' @rdname numerate
mark = function(x){
    all_names = names(x)
    if(is.null(all_names)) all_names = rep("", length(x))
    unzip_list(list(all_names, x))

}


#' @export
#' @rdname numerate
unmark = function(x, item = 2){
    res = unzip_list(x)
    set_names(
        res[[item]],
        unlist(res[[1]], use.names = FALSE)
        )
}


#' @export
#' @rdname numerate
unzip_list = function(x){
    all_lengths = unique(lengths(x))
    if(length(x)==0) return(x)
    if(length(all_lengths)!=1){
        stop("each element of the argument 'x' should have the same length.")
    }
    global_names = names(x)
    local_names = lapply(x, names)
    get_names = function(i){
        res = lapply(local_names, "[[", i)
        all_lengths = lengths(res)
        if(all(all_lengths==0)) {
            return(global_names)
        }
        res[all_lengths==0] = ""
        unlist(res, use.names = FALSE)

    }
    lapply(seq_along(x[[1]]), function(i) set_names(lapply(x, `[[`, i), get_names(i)))
}

#' @export
#' @rdname numerate
zip_lists = function(...){
    unzip_list(list(...))
}

#' @export
#' @rdname numerate
lag_list = function(x){
    lapply(seq_along(x)[-1], function(i) list(x[i-1], x[i]))
}


set_names = function (object = nm, nm) {
    names(object) = nm
    object
}
