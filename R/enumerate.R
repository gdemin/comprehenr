

numerate = function(x){
    lapply(seq_along(x), function(i) list(i, x[[i]]))
}

enumerate = numerate

mark = function(x){
    all_names = names(x)
    if(is.null(all_names)){
        lapply(seq_along(x), function(i) list("", x[[i]]))
    } else {
        lapply(seq_along(x), function(i) list(all_names[i], x[[i]]))
    }

}

unmark = function(x, item = 2){
    all_names = unlist(unnumerate(x, item = 1), use.names = FALSE)
    all_values = unnumerate(x, item = item)
    setNames(all_values, all_names)
}


unnumerate = function(x, item = 2){
    all_lengths = unique(lengths(x))
    if(length(all_lengths)!=1 || any(all_lengths<item)){
        stop(paste0("'denumerate': each element of the argument should have the same length greater or equal to ", item))
    }
    lapply(x, `[[`, item)
}


transpose = function(x){
    all_lengths = unique(lengths(x))
    if(length(x)==0) return(x)
    if(length(all_lengths)!=1){
        stop("'transpose': each element of the argument should have the same length.")
    }
    all_names = lapply(x, names)
    get_names = function(i){
        res = lapply(all_names, "[[", i)
        all_lengths = lengths(res)
        if(all(all_lengths==0)) return(NULL)
        res[all_lengths==0] = ""
        unlist(res, use.names = FALSE)

    }
    lapply(seq_along(x[[1]]), function(i) setNames(lapply(x, `[[`, i), get_names(i)))
}


zip_lists = function(...){
    transpose(list(...))
}
