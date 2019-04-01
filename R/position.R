#' Get the bodyid for a volumetric segmentation that covers a given point/points in space
#'
#' @description  Get the bodyid for a volumetric segmentation that covers a given point/points in space
#' @param xyz a vector specifying X,Y,Z coorindates, or a n x 3 matrix of multiple 3D points
#' @inheritParams dv_get_user_bookmarks
#' @return a vector of bodyids corresponding with the given points
#'
#' @export
dv_bodyid_at_xyz <- function(xyz, conn = NULL, ...){
  conn <- dv_conn(conn=conn)
  get_bodyid <- function(xyz, conn=NULL, ...){
    url=sprintf('api/node/%s/segmentation/label/%s_%s_%s', conn$node, xyz[1], xyz[2], xyz[3])
    dvf = dv_fetch(path = url, conn = conn,  ...)
    dvf$Label
  }
  if(is.vector(xyz)){
    get_bodyid(xyz=xyz,conn=conn, ...)
  }else if (is.matrix(xyz)){
    apply(xyz, 1, get_bodyid, conn=conn, ...)
  } else if (is.data.frame(xyz)){
    xyz = nat::xyzmatrix(xyz)
    apply(xyz, 1, get_bodyid, conn=conn, ...)
  }
}
