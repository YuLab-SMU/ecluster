##' @importFrom tidytree as.treedata
##' @export
tidytree::as.treedata


##' @method as.treedata eclust
##' @export
as.treedata.eclust <- function(tree, ...) {
    as.treedata.eclust2(tree, ...)
}


##' @importFrom SummarizedExperiment assayNames
##' @importFrom SummarizedExperiment colData
##' @importFrom SummarizedExperiment rowData
##' @importFrom tibble rownames_to_column
##' @importFrom treeio as.phylo
##' @importFrom tidyselect all_of
##' @importFrom tidytree left_join
##' @importFrom tidytree treedata
##' @importFrom tidyr pivot_longer
##' @importFrom tidyr nest
##' @importFrom rlang .data
as.treedata.eclust2 <- function(
    .data,
    bycol = TRUE,
    assay = 1,
    longer = FALSE,
    metadata = TRUE) {

    da <- assay(.data, assay) |> data.frame() |> rownames_to_column(var = "label")

    if (is.null(assayNames(.data)[assay])) {
        name <- 'counts'
    } else {
        name <- assayNames(.data)[assay]
    }

    if (bycol) {
        ph <- colhclust(.data) |> as.phylo()
        md <- colData(.data)
        names_to <- "sample"
    } else {
        ph <- rowhclust(.data) |> as.phylo()
        md <- rowData(.data)
        names_to <- 'feature'
    }

    if (longer) {
        da <- pivot_longer(
                da,
                cols = !'label',
                names_to = names_to,
                values_to = name
                ) |> nest(counts = c(tidyselect::all_of(names_to), 
                                    tidyselect::all_of(name)))
    }

    treedata <- treedata(phylo = ph) |> left_join(da)

    if (metadata) {
        md <- data.frame(md) |> rownames_to_column(var = "label")
        treedata <- left_join(treedata, md)
    }

    return(treedata)
}
