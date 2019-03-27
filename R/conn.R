#' Make a connection to DVID server
#'
#' @param server DVID server (with optional port specification)
#' @param node DVID node
#' @param conn A pre-existing connection object
#' @param ... Named options / headers to be set in the curl handle (see
#'   \code{\link[curl]{handle}} and \code{\link[curl]{curl_options}} for
#'   details)
#'
#' @return An list of class \code{dv_conn}
#' @export
#' @seealso \code{\link[curl]{handle}}, \code{\link[curl]{curl_options}}
#' @examples
#' \dontrun{
#'
#' conn1 <- dv_conn('http://dvid.connectomesrus.com:8900', node='a32b')
#' conn2 <- dv_conn('http://dvid.connectomesrus.com:8900', node='a42c')
#'
#' }
dv_conn <- function(server=Sys.getenv('drvid.server'), node=Sys.getenv('drvid.node'), conn=NULL, ...) {
  if (!is.null(conn))
    return(conn)
  conn=list(server = server, node = node, options=pairlist(...))
  class(conn)='dv_conn'
  conn
}
