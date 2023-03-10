#' Retrieve user bookmarks for a neuTu user from a DVID server
#'
#' @description Retrieve a user's bookmarks specifying an XYZ position and
#'   annotations, for a given  neuTu user on a DVID server
#' @param user a user is, typically surname followed by the first letter of
#'   their first name, sometimes followed by a number
#' @param conn optional DVID connection object (see \code{\link{dv_conn}})
#' @param ... Additional arguments passed to dv_fetch
#'
#' @export
#' @rdname dv_get_user_bookmarks
dv_get_user_bookmarks <- function(user, conn = NULL, ...){
  conn <- dv_conn(conn=conn)
  url=sprintf('api/node/%s/bookmark_annotations/tag/user:%s', conn$node, user)
  dvf = dv_fetch(path = url, conn = conn,  ...)
  d = lapply(dvf, function(d) data.frame(cbind(do.call(cbind,d[[1]]),do.call(cbind,d[[4]]))))
  for(i in 1:length(d)){
    if(is.null(d[[i]]$checked)){
      d[[i]]$checked = 0
    }
  }
  d = do.call(rbind,d)
  colnames(d) = c("X","Y","Z","bodyid","checked","comment","custom","status","type","user")
  d
}

#' Retrieve annotations for bodyids
#'
#' @description Retrieve annotations for a bodyid or vector of bodyids
#' @inheritParams dv_get_user_bookmarks
#' @param bodyids a vector of body IDs for neurons and/or segmentations hosted on a DVID server
#' @return a dataframe with information on names, annotations and the users that  gave them for the given bodyids
#'
#' @export
#' @rdname dv_get_annotations
dv_get_annotations <- function(bodyids, conn = NULL, ...){
  conn <- dv_conn(conn=conn)
  d = data.frame()
  values = c("body ID", "body.ID", "user", "name", "naming","status", "comment", "naming user", "naming.user")
  for(bodyid in bodyids){
    url=sprintf('api/node/%s/segmentation_annotations/key/%s', conn$node, bodyid)
    dvf = tryCatch(dv_fetch(path = url, conn = conn,  ...),
             error = function(e) NULL)
    if(!is.null(dvf)){
      m = data.frame(do.call(cbind,dvf))
      for(v in values){
        if(is.null(m[[v]])){
          m[[v]] = NA
        }
      }
      m$bodyid = bodyid
      d = rbind(d, m)
    }else{
      warning("Annotations for ", bodyid," could not be retreived")
    }
  }
  d[,c("bodyid", "name", "status", "comment", "user", "naming", "naming user", "naming.user")]
}

#' Get information about a segmentation
#'
#' @description  Get information about the segmentation hosted on a DVID server
#' @inheritParams dv_get_user_bookmarks
#' @return a list of information, possibly including: BlockSize, VoxelSize, VoxelUnits, MinPoint, MaxPoint, MinIndex, MaxIndex,
#' Background, MaxLabel, MaxRepoLabel, IndexedLabels, MaxDownresLevel, DataUUID, RepoUUID, Compression, etc.
#'
#' @export
#' @rdname dv_get_segmentation_info
dv_get_segmentation_info <- function(conn = NULL, ...){
  conn <- dv_conn(conn=conn)
  url=sprintf('/api/node/%s/labels/info', conn$node)
  dvf = dv_fetch(path = url, conn = conn, ...)
  dvf
}
