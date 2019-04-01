#' @keywords internal
#'
#' @section Package environment variables:
#'
#'   The following environment variables may be set e.g. in your
#'   \code{\link{Renviron}} file in order to specify a default DVID server. See
#'   \code{\link{dv_conn}} for more information about specifying DVID login
#'   details.
#'
#'   \itemize{
#'
#'   \item drvid.server
#'
#'   \item drvid.node
#'
#'   }
#'
#'   In addition if you need to set authentication or other curl request options
#'   you can do so directly by passing arguments to \code{\link{dv_conn}} or by
#'   setting environment variables of the form:
#'
#'   \itemize{
#'
#'   \item drvid.curl.<curloption>
#'
#'   }
#'
#'   where \code{<curloption>} represents the name of a curl option.
"_PACKAGE"
