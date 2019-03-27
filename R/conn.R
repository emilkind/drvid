#' Make a connection to DVID server
#'
#' @param server DVID server (with optional port specification)
#' @param node DVID node
#' @param conn A pre-existing connection object
#' @param ... Named options / headers to be set in the curl handle (see section
#'   Curl options for details)
#'
#' @return An list of class \code{dv_conn}
#' @export
#' @seealso \code{\link[curl]{handle}}, \code{\link[curl]{curl_options}}
#'
#' @section Curl options: \bold{drvid} uses the curl library provided by the
#'   \code{curl} package to carry out remote requests. You can set curl options
#'   by passing additional named arguments captured by \code{...}. For example
#'   you can set authentication or a proxy. See \code{\link[curl]{handle}} and
#'   \code{\link[curl]{curl_options}} for a full list of possible options.
#'
#'   You can also set default curl options using environment variables with
#'   names of the form \code{drvid.curl.<curloption>}. For example the following
#'   entry in you \code{\link{Renviron}} file will set the curl \code{proxy}
#'   option.
#'
#'   \verb{drvid.curl.proxy="socks5://localhost:6666"}
#'
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
  conn=list(server = server, node = node, options=dv_curl_options(...))
  class(conn)='dv_conn'
  conn
}

dv_curl_options <- function(...) {
  envs=Sys.getenv()
  curlopts=envs[grepl("^drvid\\.curl\\.", names(envs))]
  if (length(curlopts)) {
    names(curlopts) = sub("drvid.curl.", "", names(curlopts))
  } else {
    curlopts = list()
  }
  extra_opts=pairlist(...)
  keep=setdiff(names(curlopts), names(extra_opts))
  c(curlopts[keep], extra_opts)
}
