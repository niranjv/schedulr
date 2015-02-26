#' Runtimes on m3xlarge instance
#'
#' Dataset containing exponentially distributed runtimes generated based on mean runtimes for jobs run on m3xlarge instances in EC2
#'
#' @format A data frame with 21750 rows and 2 variables
#' \describe{
#'  \item{size_bp}{size in bp}
#'  \item{runtime_sec}{runtime in seconds on m3xlarge instance}
#' }
#' @source Generated via the R script /data-raw/generate_m3xlarge_runtimes_expdist.R in the source package
"m3xlarge.runtimes.expdist"
