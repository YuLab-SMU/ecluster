##' @importFrom methods setOldClass
setOldClass("hclust")

##' @importFrom methods setClassUnion
setClassUnion('hclust_or_NULL', c('hclust', 'NULL'))

#' @title eclust class
#' @docType class
#' @slot rowhclust hclust object to store the result of hierarchical cluster based on 
#' the row index of SingleCellExperiment.
#' @slot colhclust hclust object to store the result of hierarchical cluster based on
#' the column index of SingleCellExperiment.
#' @importClassesFrom SummarizedExperiment SummarizedExperiment 
#' @exportClass eclust
setClass('eclust',
  #contains = "SingleCellExperiment",
  contains = 'SummarizedExperiment',
  slots    = c(
    rowhclust = "hclust_or_NULL",
    colhclust = "hclust_or_NULL"
  ),
  prototype = c(
    rowhclust = NULL,
    colhclust = NULL
  )
)
