#' @title Principal stresses
#' @description Calculates the the principal stresses for a set of angular measurements.
#' @param dir A vector of dip directions measurements in degrees
#' @param dip A vector of dip angles measurements in degrees
#' @details This always considers a plane expressed as dip direction and dip angle, it could be a bedding plane or a fault plane
#' @export
#' @return A data frame with the dip direction and dip angle for the principal stresses
#' @examples
#' dir = c(12,18,22,15,10,20)
#' dip = c(42,40,48,30,42,30)
#' dir_stats_ps(dir, dip)
#'
dir_stats_ps = function(dir, dip) {
  # Vector of spherical measurements
  d = dir # dip directions
  i = dip # dip angles
  drad = d*pi/180
  irad = i*pi/180

  # Components of mean vector
  x = cos(irad)*cos(drad)
  y = cos(irad)*sin(drad)
  z = sin(irad)

  # Stress tensor components
  a11 = sum(x^2)
  a12 = sum(x*y)
  a13 = sum(x*z)
  a22 = sum(y^2)
  a33 = sum(z^2)
  a23 = sum(y*z)

  # Stress tensor matrix
  D = matrix(c(a11,a12,a13,a12,a22,a23,a13,a23,a33),nrow = 3)

  # Eigenvalues and eigenvectors
  stress = eigen(D)
  eigval = as.data.frame(stress[1])
  names(eigval) = "eigenvalues"
  eigval$S = sapply(1:3,function(x) eigval[x,1]/sum(eigval))
  eigvec = as.data.frame(stress[2])

  correctedvec = ifelse(eigvec[3,]<0,-eigvec[1:3,],eigvec[1:3,])
  eigvec = as.data.frame(correctedvec)
  names(eigvec) = c("V1","V2","V3")
  row.names(eigvec) = c("X","Y","Z")

  # Principal stresses
  S = matrix(0,nrow = 2,ncol = 3)
  colnames(S) = c("S1","S2","S3")
  row.names(S) = c("Dip Dir","Dip")

  for (w in 1:nrow(eigvec)) {
    S[1,w] = degs(atan(eigvec[2,w]/eigvec[1,w]))
    if (eigvec[2,w]>0 & eigvec[1,w]>0) {
      S[1,w] = S[1,w]
    } else if (eigvec[2,w]>0 & eigvec[1,w]<0) {
      S[1,w] = S[1,w]+180
    } else if (eigvec[2,w]<0 & eigvec[1,w]<0) {
      S[1,w] = S[1,w]+180
    } else {
      S[1,w] = S[1,w]+360
    }
    S[2,w] = degs(asin(eigvec[3,w]/(sqrt(sum(eigvec[,w]^2)))))
  }
  S = round(S,2)
  return(PS = S)
}
