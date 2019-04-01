#' Read a single neuron in skeleton format from DVID server
#'
#' @param x body id
#' @param conn optional DVID connection object (see \code{\link{dv_conn}})
#' @param ... Additional arguments passed to nat::read.neuron
#'
#' @export
read.neuron.dvid <- function(x, conn=NULL, ...) {
  conn <- dv_conn(conn=conn)
  url=sprintf('%s/api/node/%s/segmentation_skeletons/key/%s_swc', conn$server, conn$node, x)
  # res <- httr::GET(url)
  tf <- tempfile(pattern = as.character(x), fileext = '.swc')
  on.exit(unlink(tf))
  curl::curl_download(url, destfile = tf)
  nat::read.neuron(tf, ...)
}

#' @param df Optional data frame of metadata with one row per neuron
#'
#' @return a \code{\link{neuronlist}} object or a \code{\link{neuron}} for
#'   \code{read.neuron.dvid}
#' @inheritParams nat::nlapply
#' @export
#'
#' @rdname read.neuron.dvid
#' @examples
#' \dontrun{
#' nl=read.neuron.dvid(c(12345, 45678))
#' }
read.neurons.dvid <- function(x, conn=NULL, OmitFailures=NA, df=NULL, ... ) {
  if (is.null(df)) {
    names(x) = as.character(x)
    df = data.frame(bodyid = x)
    rownames(df) = names(x)
  } else {
    names(x) = rownames(df)
  }
  fakenl = nat::as.neuronlist(as.list(x), df = df)
  nat::nlapply(fakenl,
               read.neuron.dvid,
               conn = conn,
               OmitFailures = OmitFailures,
               ...)
}
