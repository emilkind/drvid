#' Get the bodyid for a volumetric segmentation that covers a given point/points
#' in space
#'
#' @description  Get the bodyid for a volumetric segmentation that covers a
#'   given point/points in space
#' @param xyz a vector specifying X,Y,Z coordinates, or a n x 3 matrix of
#'   multiple 3D points
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

# unable to get httr to encode the data in GET body
# I think it really ought to be possible use the curl package to do this
dv_bodyids_at_xyz <- function(xyz, conn = NULL, viafile=NA, ...){
  xyzmat=nat::xyzmatrix(xyz)
  if(is.na(viafile))
    viafile=nrow(xyzmat)>4000

  if(viafile) {
    tf <- tempfile()
    on.exit(unlink(tf))
  }
  bodyj <- jsonlite::toJSON(xyzmat)
  path=sprintf('api/node/%s/segmentation/labels', conn$node)
  url=file.path(conn$server, path, fsep="/")
  # res=dv_fetch(path=url, body = bodyj, conn=conn, simplifyVector = T,...)
  if(viafile) {
    writeLines(bodyj, con=tf)
    cmd=sprintf('curl -X GET --data-binary "@%s" %s', tf, url)
  } else {
    cmd=sprintf('curl -X GET --data "%s" %s', bodyj, url)
  }
  res=system(cmd, intern=T)
  res2=jsonlite::fromJSON(res, simplifyVector = T)
  res2
}



