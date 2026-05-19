# Bearing capacity factors

Calculate the bearing capacity factors Nq, Ngamma, and Nc using either
the Terzaghi or Vesic methods.

## Usage

``` r
Nq(phi, case = "general", method = "Terzaghi")

Ngamma(phi, case = "general", method = "Terzaghi")

Nc(phi, case = "general", method = "Terzaghi")
```

## Arguments

- phi:

  Friction angle in degrees

- case:

  "general" or "local" to indicate general or local shear failure
  ("general" is default)

- method:

  "Terzaghi" or "Vesic" ("Terzaghi" is default)

## Value

Bearing capacity factor Nq, Ngamma, or Nc

## Details

These functions are taken from the "geotech" package from James
Kaklamanos

## References

Coduto, D.P., Kitch, W.A., and Yeung, M.R. (2016). Foundation Design:
Principles and Practices, Pearson, Boston.

Terzaghi, K. (1943). Theoretical Soil Mechanics, John Wiley, New York

Vesic, A.S. (1973). Analysis of Ultimate Loads of Shallow Foundations,
ASCE Journal of the Soil Mechanics and Foundations Division, Vol. 99,
No. SM1, pp. 45-73.

## Examples

``` r
if (FALSE) { # \dontrun{
Nq(phi = 20, case = "local", method = "Terzaghi")
} # }

if (FALSE) { # \dontrun{
Ngamma(phi = 20, case = "local", method = "Terzaghi")
} # }

if (FALSE) { # \dontrun{
Nc(phi = 20, case = "local", method = "Terzaghi")
} # }
```
