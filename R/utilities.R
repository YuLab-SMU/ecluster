##' @importFrom SummarizedExperiment assay
.extract_assay <- function(x, nm){
    if (is_numeric(nm)){
        nm <- as.numeric(nm)
    }
    x <- assay(x, nm)
}

#' @importFrom stats dist
.run_dist <- function(x, method){
    dist(x, method = method)
}

#' @importFrom stats hclust
.run_hclust <- function(x, method){
    hclust(x, method)
}

.run_dist_hclust <- function(x, dist.method, hclust.method){
    x <- .run_dist(x, method = dist.method)
    x <- .run_hclust(x, method = hclust.method)
    return(x)
}

is_numeric <- function(x){
    !anyNA(suppressWarnings(as.numeric(x)))
}
