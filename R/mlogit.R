#' @title Multistate Life Table Method
#' @description  A Multistate Life Table Method Based on Bayesian Approach
#' @param y Dummy variables for multistates
#' @param X Covariates
#' @param samp Sampling times
#' @param burn 'burn-in' times
#' @param verbose progress report
#' @param m.0 1
#' @param P.0 1
#' @export
#' @import stats
#' @importFrom stats pnorm rexp rnorm runif
#' @return  An array for all parameter samples
#' @examples
#' mlogit(y, X, samp=3000, burn=0,verbose=10)

mlogit <- function(y, X,
                   m.0=array(0, dim=c(ncol(X), ncol(y))),
                   P.0=array(0, dim=c(ncol(X), ncol(X), ncol(y))),
                   samp=1000, burn=500, verbose=1000)

{

  TRUNC = 0.64

  mass.texpon <- function(Z)
  {
    x = TRUNC;
    fz = pi^2 / 8 + Z^2 / 2;
    b = sqrt(1.0 / x) * (x * Z - 1);
    a = -1.0 * sqrt(1.0 / x) * (x * Z + 1);

    x0 = log(fz) + fz * TRUNC;
    xb = x0 - Z + pnorm(b, log.p=TRUE);
    xa = x0 + Z + pnorm(a, log.p=TRUE);

    qdivp = 4 / pi * ( exp(xb) + exp(xa) );

    1.0 / (1.0 + qdivp);
  }

  rtigauss <- function(Z, R=TRUNC)
  {
    Z = abs(Z);
    mu = 1/Z;
    X = R + 1;
    if (mu > R) {
      alpha = 0.0;
      while (runif(1) > alpha) {
        ## X = R + 1
        ## while (X > R) {
        ##     X = 1.0 / rgamma(1, 0.5, rate=0.5);
        ## }
        E = rexp(2)
        while ( E[1]^2 > 2 * E[2] / R) {
          E = rexp(2)
        }
        X = R / (1 + R*E[1])^2
        alpha = exp(-0.5 * Z^2 * X);
      }
    }
    else {
      while (X > R) {
        lambda = 1.0;
        Y = rnorm(1)^2;
        X = mu + 0.5 * mu^2 / lambda * Y -
          0.5 * mu / lambda * sqrt(4 * mu * lambda * Y + (mu * Y)^2);
        if ( runif(1) > mu / (mu + X) ) {
          X = mu^2 / X;
        }
      }
    }
    X;
  }

  a.coef <- function(n,x)
  {
    if ( x>TRUNC )
      pi * (n+0.5) * exp( -(n+0.5)^2*pi^2*x/2 )
    else
      (2/pi/x)^1.5 * pi * (n+0.5) * exp( -2*(n+0.5)^2/x )
  }

  rpg.devroye.1 <- function(Z)
  {
    Z = abs(Z) * 0.5;

    ## PG(1,z) = 1/4 J*(1,Z/2)
    fz = pi^2 / 8 + Z^2 / 2;
    ## p = (0.5 * pi) * exp( -1.0 * fz * TRUNC) / fz;
    ## q = 2 * exp(-1.0 * Z) * pigauss(TRUNC, 1.0/Z, 1.0);

    num.trials = 0;
    total.iter = 0;

    while (TRUE)
    {
      num.trials = num.trials + 1;

      if ( runif(1) < mass.texpon(Z) ) {
        ## Truncated Exponential
        X = TRUNC + rexp(1) / fz
      }
      else {
        ## Truncated Inverse Normal
        X = rtigauss(Z)
      }

      ## C = cosh(Z) * exp( -0.5 * Z^2 * X )

      ## Don't need to multiply everything by C, since it cancels in inequality.
      S = a.coef(0,X)
      Y = runif(1)*S
      n = 0

      while (TRUE)
      {
        n = n + 1
        total.iter = total.iter + 1;
        if ( n %% 2 == 1 )
        {
          S = S - a.coef(n,X)
          if ( Y<=S ) break
        }
        else
        {
          S = S + a.coef(n,X)
          if ( Y>S ) break
        }
      }

      if ( Y<=S ) break
    }

    ## 0.25 * X
    list("x"=0.25 * X, "n"=num.trials, "total.iter"=total.iter)
  }

  ## N - number of trials
  ## J - number of categories
  ## P - number of covariates
  ## y - N x J-1 matrix.  y_{ij} = fraction of outcomes in jth category on trial i.
  ## X - N x P design matrix
  ## n - N x 1 matrix of rolls
  ## Assume beta_J = 0 for identification.

  N = nrow(X);
  P = ncol(X);
  J = ncol(y) + 1;

  out = list(
    beta = array(0, dim=c(samp, P, J-1))
  )

  beta = matrix(0, P, J);
  w    = matrix(0, N, J);

  ## Precompute. (Initialize)
  n=rep(1,nrow(as.matrix(y)))
  kappa = (y - 0.5)*n;

  b.0 = matrix(0, P, J-1);
  for (j in 1:(J-1)) b.0[,j] = P.0[,,j] %*% m.0[,j];

  ## A = rowSums( exp(X %*% beta[,-1]) );
  for (i in 1:(samp+burn)) {
    for (j in 1:(J-1)) {

      ## For now recompute at each iteration.  Try taking out later.
      A = rowSums( exp(X %*% beta[,-j]) );

      c.j   = log(A);
      eta.j = X %*% beta[,j] - c.j;

      ## omega.j
      for (q in 1:N)
        w[q,j] = rpg.devroye.1(eta.j[q])$x;

      ## beta.j
      PL.j = t(X) %*% (X * w[,j]);
      bL.j = t(X) %*% (kappa[,j] + c.j * w[,j]);

      P1.j = PL.j + P.0[,,j];
      ## Can speed up using Choleksy.
      V1.j = chol2inv(chol(P1.j));
      m1.j = V1.j %*% (bL.j + b.0[,j]);

      sqrtV1.j = t(chol(V1.j));
      beta[,j] = m1.j + sqrtV1.j %*% rnorm(P);

      ## A += exp(X %*% beta[,j]) - exp(X %*% beta[,(j+1) %% (J-1)])

      ## Store
      if (i > burn) {
        out$beta[i-burn,,j] = beta[,j];
      }
    }
    if (i %% verbose == 0) cat("Finished", i, "\n");
  }

  return(out)
}

