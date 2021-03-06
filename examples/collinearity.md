Sample Collinearity
================

This document introduces `collinearity()`, which estimates sample collinearity on a 0 to 1 scale based on generalized correlation of the X variables. The proposal is to adapt this function so that the highest degree polynomial is selected such that collinearity does not exceed say 0.95.

**Background**: `collinearity` is an issue that occurs in the least squares context when *X*′*X* cannot be inverted due to the fact that at least one of the columns of X is a linear combination of the others (or a near perfect linear combination). By definition,

$$ (X'X)^{-1} = \\frac{1}{det(X'X)} Adj(X'X) $$
 If one of the columns is linearly dependent, *d**e**t*(*X*′*X*) approaches 0 and the calculation becomes impossible. (Collinearity comes up of course with other methods of estimation but manifests differently, e.g. as lack of convergence.) Note that if *X* is *N* observations by *P* features, *X*′*X* is *P* x *P* and it contains similar information to the sample covariance matrix.

Generalized variance is defined (e.g., T.W. Anderson) as the determinant of the covariance matrix, |*Σ*| and generalized correlation is defined analogously in terms of the correlation matrix. The function introduced below uses correlation because it is much easier to interpret since it places everything on a 0 to 1 scale, where 0 represents dependence and 1 represents independence. (For a bit of detail, see <http://statweb.stanford.edu/~sabatti/Stat200/mvn.pdf>).

**Code**: Here is the source code for `collinearity()`.

``` r
collinearity <- function(X){
  
  if(sum(is.na(X)) > 0)
    stop("collinearity() does not accept missing data.")
  
  col_sd <- apply(X, 2, sd)
  if(min(col_sd) == 0)
    warning(paste("The following columns do not vary:", paste(which(col_sd == 0), collase=", "), "\nIf not removed, collinearity() returns NaN.\n"))
  # S <- cov(X)
  # return(1 - det(S)/prod(diag(S)))
  # equivalent to below
  return(1 - det(cor(X)))
}
```

**Example**: Here we use the `pe` data and then call `collinearity()` on the x variables on the raw data as well as at polynomial degree 2 (in the sense operationalized by `polyreg::plm()`).

``` r
library(partools)
library(polyreg)      # version from github.com/rdrr1990/polyreg
library(ggplot2)
library(dplyr)        # used only for select in demo code

set.seed(2018)

getPE <- function(){
  data(prgeng)
  pe <- prgeng[,c(1,3,7:9)]
  # dummies for MS, PhD
  pe$ms <- as.integer(pe$educ == 14)
  pe$phd <- as.integer(pe$educ == 16)
  pe$educ <- NULL
  pe <<- pe
}
getPE() # code from partools, possibly only on git version
sum(is.na(pe)) # no missing data
```

    [1] 0

``` r
pe2 <- plm(pe[,c(1,2,4:6,3)], 2)
colnames(pe2) <- c(paste('x',1:(ncol(pe2) - 1), sep=''),'wageinc')

pe3 <- plm(pe[,c(1,2,4:6,3)], 3)
colnames(pe3) <- c(paste('x',1:(ncol(pe3) - 1),sep=''),'wageinc')

pe4 <- plm(pe[,c(1,2,4:6,3)], 4)
colnames(pe4) <- c(paste('x',1:(ncol(pe4) - 1),sep=''),'wageinc')

collinearity(select(pe, -wageinc))
```

    [1] 0.02300254

``` r
collinearity(select(pe2, -wageinc))
```

    Warning in collinearity(select(pe2, -wageinc)): The following columns do not vary: 11 ,  
    If not removed, collinearity() returns NaN.

    Warning in cor(X): the standard deviation is zero

    [1] NA

Collinearity is quite low in the original data frame, however at polynomial degree 2 it cannot be calculated since one of the columns doesn't vary. Dropping that column (like `lm` would) reveals that there is still a problem.

``` r
collinearity(select(pe2, -wageinc, -x11))
```

    [1] 1

``` r
collinearity(pe2[,1:10])
```

    [1] 0.9999994

The original data are found in the first five columns.

``` r
collinearity(pe2[,c(1:5)])              # 0.023, same as above
```

    [1] 0.02300254

All columns don't introduce equal trouble.

``` r
collinearity(pe2[,c(1:5, 6)])
```

    [1] 0.9754033

``` r
collinearity(pe2[,c(1:5, 7)])
```

    [1] 0.9629374

``` r
collinearity(pe2[,c(1:5, 8)])
```

    [1] 0.9219983

``` r
collinearity(pe2[,c(1:5, 9)])
```

    [1] 0.8993689

``` r
collinearity(pe2[,c(1:5, 10)])
```

    [1] 0.9158467

One strategy for `polyreg` would be to pick a threshold for tolerable collinearity and pick the highest degree polynomial that does not go above that threshold (dropping additional columns as need be in a case like this one...).
