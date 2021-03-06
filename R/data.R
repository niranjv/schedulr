#' Runtimes for tasks on m3xlarge instances
#' '
#' Dataset containing runtimes for tasks of various sizes. The runtimes for each
#' task size are generated from an exponential distribution. The mean of the 
#' exponential distribution is proportional to the task size.
#'
#' @format A data frame with 21750 rows and 2 variables
#' \describe{
#'  \item{size_bp}{size in bp}
#'  \item{runtime_sec}{runtime in seconds on m3xlarge instance}
#' }
#' @source Generated via the R script /data-raw/generate_m3xlarge_runtimes_expdist.R in the source package
"m3xlarge.runtimes.expdist"
