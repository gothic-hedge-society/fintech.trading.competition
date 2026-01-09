# gmrr

Calculate geometric means of an xts series of returns.

## Usage

``` r
gmrr(returns_xts)
```

## Arguments

- returns_xts:

  An xts object whose values are period-over-period returns observed for
  the assets specified by the identifiers in the column names. Returns
  are NOT assumed to be in 'percent form': i.e., make sure that in
  whatever xts is passed as `returns_xts`, a return of *12*% is
  represented as *0.12*.

## Value

A numeric vector whose values are the geometric means of the returns in
`returns_xts` and whose names are the identifiers for which each
geometric mean return was calculated.
