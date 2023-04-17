#' transform the eclust class to treedata class
#' @rdname as_treedata-methods
#' @param .data R object, here is eclust class
#' @param bycol cluster by column
#' @param assay assay of the eclust that is to be saved in treedata, default is 1
#' @param longer transform the assay to longer type
#' @param metadata add the metadata of the eclust
#'
#' @return treedata class
#' @export
#'
#' @examples
#' data(ec)
#' tree <- as_treedata(ec,longer = TRUE)
setGeneric('as_treedata', function(.data,
                                   bycol = TRUE,
                                   assay = 1,
                                   longer = FALSE,
                                   metadata = FALSE)
  standardGeneric("as_treedata"))


#' @rdname as_treedata-methods
#' @exportMethod as_treedata
#' @importFrom SummarizedExperiment assayNames
#' @importFrom SummarizedExperiment colData
#' @importFrom tibble rownames_to_column
#' @importFrom treeio as.phylo
#' @importFrom tidytree left_join
#' @importFrom tidytree treedata
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest
setMethod('as_treedata', signature(.data = 'eclust'),
          function(.data,
                   bycol = TRUE,
                   assay = 1,
                   longer = FALSE,
                   metadata = FALSE) {
            da <- assay(.data, assay) |> data.frame() |> rownames_to_column(var = "label")
            
            if (is.null(assayNames(.data)[assay])) {
              name <- 'counts'
            } else {
              name <- assayNames(.data)[assay]
            }
            
            if (bycol) {
              ph <- colhclust(.data) |> as.phylo()
              md <- colData(.data)
              if (longer) {
                da <-
                  pivot_longer(
                    da,
                    cols = !'label',
                    names_to = 'sample',
                    values_to = name
                  ) |> nest(counts = c(sample, name))
              }
              
            } else {
              ph <- rowhclust(.data) |> as.phylo()
              md <- rowData(.data)
              if (longer) {
                da <-
                  pivot_longer(
                    da,
                    cols = !'label',
                    names_to = 'feature',
                    values_to = name
                  ) |> nest(counts = c(feature, name))
              }
            }
            
            treedata <- treedata(phylo = ph) |> left_join(da)
            
            if (metadata) {
              md <- data.frame(md) |> rownames_to_column(var = "label")
              treedata <- left_join(treedata, md)
            }
            
            return(treedata)
          })