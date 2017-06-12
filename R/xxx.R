#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

.onLoad <- function(libname = find.package("SanFranBeachWater"),
                    pkgname = "SanFranBeachWater"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".", "Date"))
  invisible()
}
