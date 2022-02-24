
#' @title start_cluster
#' @description Creates a compute cluster
#' @param workers \code{(numeric)} number of worker nodes. Defaults to 3/4 of the available.
#' @param outfile \code{(character/logical)} Path and name of the outfile or `FALSE` to disable an outfile.
#' @return \code{(RichSOCKcluster)}
#' @export

start_cluster <- function(workers = future::availableCores() %/% 2, timeout = 60 * 60 * 5, outfile = file.path(getwd(), "cl_out.log")) {
  .args <- list(
    workers = workers,
    timeout = timeout
  )
  if (!isFALSE(outfile)) {
    # Remove the previous outfile if it exists
    if (file.exists(outfile)) {
      file.remove(outfile)
    } else {
      file.create(outfile)
    }
    .args$outfile = outfile
  }


  # create the cluster
  do.call(parallelly::makeClusterPSOCK, purrr::compact(.args))
}

