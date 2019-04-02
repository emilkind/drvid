# hidden
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

# hidden
ashape2mesh3d <- function (ashape, remove.interior.points = TRUE) {
  triangles = ashape$triang[apply(ashape$triang, 1, function(x) {
    (any(as.numeric(x[9]) > 1))
  }), ][, 1:3]
  if (remove.interior.points) {
    if (!requireNamespace("pbapply", quietly = TRUE))
      stop("Please install suggested pbapply package")
    vertices = unique(as.vector(unique(triangles)))
    kept = 1:length(vertices)
    names(kept) = vertices
    vert = t(ashape$x)[, vertices]
    tri <- pbapply::pbapply(triangles, 1, function(x) kept[as.character(x)])
    mesh3d = rgl::tmesh3d(vertices = vert, indices = tri,
                          homogeneous = F)
  }
  else {
    mesh3d = rgl::tmesh3d(t(ashape$x), t(triangles), homogeneous = F)
  }
  mesh3d
}
