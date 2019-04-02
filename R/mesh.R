#' @export
#' @rdname dv_get_volume
dv_get_voxels <- function(bodyid, scale = 4, conn = NULL, ...){
  conn <- dv_conn(conn=conn)
  info = dv_get_segmentation_info(conn=conn)
  res = info$Extended$MaxDownresLevel
  if(scale=="coarse"){
    voxelsize = do.call(cbind,info$Extended$BlockSize)
    url=sprintf('/api/node/%s/segmentation/sparsevol-coarse/%s', conn$node, bodyid)
  }else if (scale%in%0:res){
    voxelsize = do.call(cbind,info$Extended$VoxelSize)*(2^scale)
    url=sprintf('/api/node/%s/segmentation/sparsevol/%s?scale=%s', conn$node, bodyid, scale)
  }else{
    stop("Scale must be 'coarse', or a number between 0 and ", res)
  }
  dvf = dv_fetch(path = url, conn = conn, parse.json = FALSE, ...)
  b = dvf$content
  header = c(readBin(b[1:4], what = integer(), n=4, size = 1, signed = TRUE, endian = "little"),
             readBin(b[5:12], what = integer(), n=2, size = 4, endian = "little"))
  names(header) = c('start_byte', 'n_dims', 'run_dims', 'reserved', 'n_blocks', 'n_spans')
  coords = data.frame()
  for(i in 0:header['n_spans']){
    offset = 12 + (i * 16) + 1
    pos = readBin(b[offset:(offset+15)], what = integer(), n = 4, size = 4, endian = "little")
    m = matrix(pos[1:3], nrow = pos[4], ncol = 3, byrow = TRUE)
    m[,1] = m[,1] + 0:(pos[4]-1)
    coords = rbind(coords,m)
  }
  coords = unique(coords)
  colnames(coords) = c("X","Y","Z")
  voxels = coords * voxelsize
  attr(voxels,"df") = dv_get_annotations(bodyids = bodyid, conn = conn, ...)
  voxels
}

#' Get a mesh or voxel data for a given bodyid
#'
#' @description  Get a mesh or voxel data for a given bodyid, from a DVID server.
#' Meshes are made via alphashapes, using the function \code{alphashape3d::ashape3d}, which will work poorly is scale is too low, and slowly is scale is too high.
#' @param bodyid a body ID for a neuron or segmentation hosted on a DVID server
#' @param scale Resolution of sparse volume starting with 0 where each level
#' beyond 0 has 1/2 resolution of previous level. "coarse" will
#' return the volume in block coordinates.
#' @param use.surface.voxels if TRUE, surface voxels as estimated as points that do not have at least k other points within 2*voxelsize of them
#' @param k see use.surface.voxels
#' @param conn optional DVID connection object (see \code{\link{dv_conn}})
#' @param ... Additional arguments passed to dv_fetch
#' @return a mesh3
#' @export
#' @rdname dv_get_volume
dv_get_mesh <- function(bodyid, scale = 0, conn = NULL, use.surface.voxels = TRUE, k = 10, ...){
  voxels = dv_get_voxels(bodyid = bodyid, scale = scale, conn = conn, ...)
  info = dv_get_segmentation_info(conn=conn)
  res = info$Extended$MaxDownresLevel
  if(scale=="coarse"){
    vsize = mean(unlist(info$Extended$BlockSize))[1]
  }else if (scale%in%0:res){
    vsize = mean(unlist(info$Extended$VoxelSize))*(2^scale)[1]
  }
  if(use.surface.voxels){
    edge.points = nabor::knn(query=voxels,data=voxels,k=k,radius=vsize*2)
    e = unique(voxels[apply(edge.points$nn.dists,1,function(e) sum(is.na(e))>1|max(e)>vsize*2),])
  }else{
    e = voxels
  }
  a = alphashape3d::ashape3d(as.matrix(e), pert = TRUE, alpha = vsize*4)
  ashape2mesh3d(a, remove.interior.points = FALSE)
}



