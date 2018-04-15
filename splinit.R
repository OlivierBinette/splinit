source("prettyplot.R")
library(glmnet)
library(MASS)
library(tspmeta)
library(vegan)

phi <- function(d, ell) {
  # Base spline function.
  
  fun <- function(u) {
    x = (d*u/(2*pi) + (ell+1)/2)
    if(x < 0) return(0)
    k = 0:floor(x)
    sum((-1)^k * choose(ell+1, k) * (x-k)^ell / factorial(ell))
  }
  return(Vectorize(fun))
}

spline.design <- function(X, d, ell) {
  # Design matrix.
  
  Phi <- function(u, d, ell) {
    u = u - (0:(d-1))*2*pi/d
    u = u %% (2*pi)
    u[u > pi] = u[u > pi] - 2*pi
    phi(d, ell)(u)
  }
  t(sapply(X, function(u) Phi(u, d, ell)))
}

circulant <- function(x, d = length(x)) {
  # Circulant matrix.
  
  matrix(x[(1:d - rep(1:d, each=d)) %% d + 1L], ncol=d, byrow=TRUE)
}

spline.reg <- function(X, Y, dim=15, degree=3,
                       fused = TRUE, type = "ridge",
                       eval.pts=seq(0, 2*pi, length.out=200)) 
{
  # Regularisation matrix
  rho = if (fused) 1 else 0;
  G =  circulant(c(1, -rho, rep(0, dim-2)))
  G.inv = ginv(G)
  
  # Spline basis
  M = spline.design(X, dim, degree)
  Mu = spline.design(eval.pts, dim, degree)
  
  # Cross validated estimation
  alpha = if (type == "ridge") 0 else 1;
  beta = coef(cv.glmnet(M %*% G.inv, Y, alpha=alpha))
  
  Mu %*% G.inv %*% beta[-1] + beta[1]
}

splinit <- function(Y, param=NULL, 
                    dim=5*ceiling(nrow(Y)^0.33)+5, degree=3, 
                    fused = TRUE, type = "ridge",
                    knn = ceiling(nrow(Y)^0.33) + 1,
                    rep = 20,
                    eval.pts=seq(0, 2*pi, length.out=200))
{
  #
  # Periodic spline regression and closed curve reconstruction
  #
  # Args:
  #   Y :   Matrix of data points. Each row must represent an 
  #         observation.
  #   param:  Cicular parameterization of the data points
  #           by angles in the interval [0, 2 pi).
  #   dim:    Number of spline basis elements.
  #   degree: Degree of the splines (e.g. 3 for cubic splines).
  #   fused:  TRUE for fused regression (regularizes the circular 
  #           difference sequence of the parameters rather than the 
  #           parameters themselves.)
  #   type:   One of "ridge" or "lasso".
  #   knn:    Number or neighbors to consider in the isomap-type 
  #           intrinsic distance approximation. Used only for unknown
  #           parameterizations.
  #   rep:    Number of repetitions to use for the minimal hamiltonian
  #           cycle approximation. Increase for greater accuracy and 
  #           stability. Used only for unknown parameterizations.
  #   eval.pts: Points at which the spline function is evaluated.
  #
  # Returns:
  #   A matrix containing the evaluation of the splines at the points
  #   specified by eval.pts.
  #
  # Examples:
  # > angles = 2*pi*(1:20)/20
  # > points = cbind(cos(angles), sin(angles))
  # > plot(points)
  # > lines(splinit(points))
  #
  # Comments:
  #   - Use only with more than 30 observations (cross validation may 
  #     fail ortherwise).
  #   - Stability of the estimate can be improved by increasing the
  #     _rep_ parameter.
  
  if (missing(param)) {
    # Parameterization estimation
    d = isomapdist(dist(Y), k=knn)
    D = solve_TSP(TSP(d), method="two_opt", rep=rep)
    param = as.numeric(sapply(1:nrow(Y), function(i) D[[i]]))
    param = 2*pi*param/nrow(Y)
  }
  
  # Independent regressions on the components of Y
  fun <- function(y) spline.reg(param, y, dim=dim, degree=degree, fused=fused, type=type, eval.pts=eval.pts)
  apply(Y, 2, fun)
}
```