#' Hierarchical cluster analysis for the sample or features with SummarizedExperiment class
#' @rdname eclust-methods
#' @param .data R object, here is SummarizedExperiment class
#' @param .assay the name or index of the assays in SummarizedExperiment class,
#' default is 1.
#' @param dist.method the method to calculate the distance, default is 'euclidean'.
#' @param hclust.method the agglomeration method for the hierarchical cluster, 
#' default is 'average'.
#' @param direction sample ('col') or features ('row') to preform the hierarchical 
#' cluster analysis, default is 'col'.
#' @param ... additional parameters
#' @return eclust class
#' @export
setGeneric('eclust', function(
  .data, 
  .assay, 
  dist.method = 'euclidean', 
  hclust.method = 'average', 
  direction = 'col', 
  ...) standardGeneric("eclust")
)

##' @rdname eclust-methods
##' @aliases eclust,SummarizedExperiment
##' @exportMethod eclust
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
##' @importFrom rlang enquo quo_is_missing get_expr
##' @importFrom SummarizedExperiment assay
setMethod('eclust', signature(.data = 'SummarizedExperiment'),
   function(.data, 
            .assay, 
            dist.method = 'euclidean', 
            hclust.method = 'average', 
            direction = 'col',
            ...){
       .assay <- rlang::enquo(.assay)
       direction <- match.arg(direction, c("row", "col", "both"))
       if (quo_is_missing(.assay)){
           da <- assay(.data, 1) 
       }else{ 
           da <- .extract_assay(.data, nm=get_expr(.assay))
       }
       .data <- new('eclust', .data) 
       if (direction == 'col'){
           da <- t(da)
           x <- .run_dist_hclust(da, dist.method, hclust.method)
           colhclust(.data) <- x
       }else if (direction == 'row'){
           x <- .run_dist_hclust(da, dist.method, hclust.method)
           rowhclust(.data) <- x
       }else if (direction == 'both'){
           row.x <- .run_dist_hclust(da, dist.method, hclust.method)
           col.x <- .run_dist_hclust(t(da), dist.method, hclust.method)
           rowhclust(.data) <- row.x
           colhclust(.data) <- col.x
       }
       return (.data)
   }
)




