% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waywiser-package.R
\docType{package}
\name{waywiser-package}
\alias{waywiser}
\alias{waywiser-package}
\title{waywiser: Ergonomic Methods for Assessing Spatial Models}
\description{
\if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}

Assessing predictive models of spatial data can be challenging, both because these models are typically built for extrapolating outside the original region represented by training data and due to potential spatially structured errors, with "hot spots" of higher than expected error clustered geographically due to spatial structure in the underlying data. Methods are provided for assessing models fit to spatial data, including approaches for measuring the spatial structure of model errors, assessing model predictions at multiple spatial scales, and evaluating where predictions can be made safely. Methods are particularly useful for models fit using the 'tidymodels' framework. Methods include Moran's I ('Moran' (1950) \doi{10.2307/2332142}), Geary's C ('Geary' (1954) \doi{10.2307/2986645}), Getis-Ord's G ('Ord' and 'Getis' (1995) \doi{10.1111/j.1538-4632.1995.tb00912.x}), agreement coefficients from 'Ji' and Gallo (2006) (\doi{ 10.14358/PERS.72.7.823}), agreement metrics from 'Willmott' (1981) (\doi{ 10.1080/02723646.1981.10642213}) and 'Willmott' 'et' 'al'. (2012) (\doi{ 10.1002/joc.2419}), an implementation of the area of applicability methodology from 'Meyer' and 'Pebesma' (2021) (\doi{10.1111/2041-210X.13650}), and an implementation of multi-scale assessment as described in 'Riemann' 'et' 'al'. (2010) (\doi{10.1016/j.rse.2010.05.010}).
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/ropensci/waywiser}
  \item \url{https://docs.ropensci.org/waywiser/}
  \item Report bugs at \url{https://github.com/ropensci/waywiser/issues}
}

}
\author{
\strong{Maintainer}: Michael Mahoney \email{mike.mahoney.218@gmail.com} (\href{https://orcid.org/0000-0003-2402-304X}{ORCID})

Other contributors:
\itemize{
  \item Lucas Johnson \email{lucas.k.johnson03@gmail.com} (\href{https://orcid.org/0000-0002-7953-0260}{ORCID}) [contributor]
  \item Virgilio Gómez-Rubio (Virgilio reviewed the package (v. 0.2.0.9000) for rOpenSci, see <https://github.com/ropensci/software-review/issues/571>) [reviewer]
  \item Jakub Nowosad (Jakub reviewed the package (v. 0.2.0.9000) for rOpenSci, see <https://github.com/ropensci/software-review/issues/571>) [reviewer]
  \item Posit Software, PBC [copyright holder, funder]
}

}
\keyword{internal}
