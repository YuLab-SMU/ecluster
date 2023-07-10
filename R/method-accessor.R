#' eclust accessors
#' @param x R object, eclust class in here
#' @param ... additional parameters.
#' @name eclust-accessors
NULL

#' @rdname eclust-accessors
#' @export
setGeneric("colhclust", function(x, ...) standardGeneric('colhclust'))

#' @rdname eclust-accessors
#' @aliases colhclust,eclust
#' @export
setMethod('colhclust', signature(x = 'eclust'),
          function(x, ...){
    x@colhclust
})

#' @rdname eclust-accessors
#' @export
setGeneric("rowhclust", function(x, ...) standardGeneric('rowhclust'))

#' @rdname eclust-accessors
#' @aliases rowhclust,eclust
#' @export
setMethod('rowhclust', signature(x = 'eclust'), 
          function(x, ...){
    x@rowhclust          
})

#' @rdname eclust-accessors
#' @param x R object, here is eclust class
#' @param value hclust or NULL
#' @export
setGeneric("colhclust<-", function(x, ..., value) standardGeneric('colhclust<-'))

#' @rdname eclust-accessors
#' @aliases colhclust<-,eclust,hclust_or_NULL
#' @export
setReplaceMethod('colhclust', signature(x = 'eclust', value = 'hclust_or_NULL'), 
  function(x, ..., value){
      x@colhclust <- value
      return(x)
})

#' @rdname eclust-accessors
#' @param x R object, here is eclust class
#' @param value hclust or NULL
#' @export
setGeneric("rowhclust<-", function(x, ..., value) standardGeneric('rowhclust<-'))

#' @rdname eclust-accessors
#' @aliases rowhclust<-,eclust,hclust_or_NULL
#' @export
setReplaceMethod('rowhclust', signature(x = 'eclust', value = 'hclust_or_NULL'),
  function(x, ..., value){
      x@rowhclust <- value
      return(x)
   }
)
