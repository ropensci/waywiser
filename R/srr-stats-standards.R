#' srr_stats
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.1} The third is mentioned in the README.
#' @srrstats {G1.2} Included in CONTRIBUTING.md
#' @srrstats {G1.3} I have attempted to do so throughout the package.
#' @srrstats {G2.3a} There is no univariate character input in this package.
#' @srrstats {G2.3b} There is no univariate character input in this package.
#' @srrstats {G2.4a} There are no places necessitating automated coercion.
#' @srrstats {G2.4b} There are no places necessitating automated coercion.
#' @srrstats {G2.4c} There are no places necessitating automated coercion.
#' @srrstats {G2.4d} There are no places necessitating automated coercion.
#' @srrstats {G2.4e} There are no places necessitating automated coercion.
#' @srrstats {SP2.1} This package does not use sp, rather uses sf.
#' @srrstats {SP2.2} This package relies upon sf for classes throughout, for maximum compatibility.
#' @srrstats {SP2.2a} Documentation extensively uses sf and shows that outputs are sf objects.
#' @srrstats {SP2.2b} Tests rely extensively upon sf objects.
#'
#'
#' @noRd
NULL

#' NA_standards
#'
#' I am confident that the following are not applicable:
#' @srrstatsNA {G1.5} No performance claims are made in associated publications.
#' @srrstatsNA {G1.6} This software does not make explicit performance claims.
#' @srrstatsNA {G2.3} (Addressed 2.3a & b separately)
#' @srrstatsNA {G2.5} No inputs are expected to be factors.
#' @srrstatsNA {G3.1} I don't believe this software calls covariance functions, or any which would be useful to allow user control over.
#' @srrstatsNA {G3.1a} I don't believe this software calls covariance functions, or any which would be useful to allow user control over.
#' @srrstatsNA {G4.0} This software does not write to file.
#' @srrstatsNA {G5.4a} No new methods are implemented.
#' @srrstatsNA {G5.6} I don't believe parameter tests are applicable here, but may be misunderstanding the concept.
#' @srrstatsNA {G5.6a} I don't believe parameter tests are applicable here, but may be misunderstanding the concept.
#' @srrstatsNA {G5.6b} I don't believe parameter tests are applicable here, but may be misunderstanding the concept.
#' @srrstatsNA {G5.10} There are no extended tests in this package.
#' @srrstatsNA {G5.11} There are no extended tests in this package.
#' @srrstatsNA {G5.11a} There are no extended tests in this package.
#' @srrstatsNA {G5.12} There are no extended tests in this package.
#' @srrstatsNA {SP2.0a} No new spatial classes are implemented.
#' @srrstatsNA {SP6.5} This software does not include unsupervised learning algorithms.
#' @srrstatsNA {SP2.4} This is handled by sf.
#' @srrstatsNA {SP2.4a} This is handled by sf.
#' @srrstatsNA {SP2.5} This is handled by sf.
#' @srrstatsNA {SP2.5a} No spatial classes are implemented.
#' @srrstatsNA {SP3.2} This software does not itself implement or use any sampling.
#' @srrstatsNA {SP3.4} This is not spatial clustering software.
#' @srrstatsNA {SP5.0} No spatial class system is implemented.
#' @srrstatsNA {SP5.1} No spatial class system is implemented.
#' @srrstatsNA {SP5.2} No spatial class system is implemented.
#' @srrstatsNA {SP5.3} No spatial class system is implemented.
#' @srrstatsNA {SP6.0} No such routines are implemented.
#' @srrstatsNA {SP6.1a} All functions can be applied to either.
#'
#' I have reasons to not implement the following:
#' @srrstatsNA {G2.6} This would introduce errors into package calculations. Examining the storage mode of units objects and then treating them as bare vectors, for instance, would ignore that units objects are explicitly not bare numeric vectors; an agreement coefficient calculated between a vector in meters and one in feet would be wrong, or at the very least incredibly difficult to interpret. The appropriate thing to do here is to error, particularly if I don't intend to support every S3 class.
#' @srrstatsNA {G2.11} As above, I'm of the belief that developers who make the decision to not subclass standard vector types have done so intentionally, and their objects should not be processed as standard vector types.
#'
#' I am less confident that the following are not applicable:
#' @srrstatsNA {G2.9} This package only uses lossless casting.
#' @srrstatsNA {G5.3} Functions from this package rely on user-provided NA handling functions, and as such do not assert that there are no NA values.
#' @srrstatsNA {G5.8d} I don't entirely understand what this means, but don't believe it applies here.
#' @srrstatsNA {SP3.3} This is not spatial regression software.
#' @srrstatsNA {SP3.5} This package does not implement any new ML methods, and as such I don't believe this applies.
#' @srrstatsNA {SP3.6} This package does not implement any new ML methods, and as such I don't believe this applies.
#' @srrstatsNA {SP6.4} This package does not implement new functions which rely on neighbors.
#' @srrstatsNA {SP6.6} This package does not implement any new ML methods, and as such I don't believe this applies.
#'
#' @noRd
NULL
