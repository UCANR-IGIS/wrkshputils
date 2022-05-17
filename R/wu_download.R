#' Download a file with an option to unzip
#'
#' @param x A single filename or URL of a PNG or JPG image
#' @param destdir Destination directory
#' @param auto_unzip Automatically unzip the file, Logical
#' @param overwrite Overwrite existing files, Logical
#' @param quiet Suppress messages
#'
#' @details This will download a file (using \code{\link{download.file}} under the hood) with an
#' option to unzip it (if its a zip file that is).
#'
#' \code{x} must be a valid URL using http or ftp. This function requires that the URL ends
#' in a filename (unfortunately not all URLs do).
#'
#' @returns A character vector of the downloaded file. If downloading a zip file with auto_unzip = TRUE, it
#' return a vector of filename(s) where they were unzipped.

#' @importFrom utils unzip download.file
#' @importFrom tools file_ext
#' @export

wu_download <- function(x, destdir = ".", auto_unzip = TRUE, overwrite = FALSE, quiet = FALSE) {

  if (length(x) != 1) stop("x should be of length 1")
  if (!grepl("^http|^ftp", x, ignore.case = TRUE)) stop("x should be a URL that starts with `http` or `ftp`")
  if (!file.exists(destdir)) stop(paste0("Can't find ", destdir))
  if (!file.info(destdir[[1]])[["isdir"]]) stop("destdir must be a directory")

  ## Get the URL file extension
  x_ext <- tolower(file_ext(x))
  if (x_ext == "") stop("Sorry, this function requires the URL to end with a file name")

  if (x_ext == "zip" && auto_unzip) {
    ## Create a temp file name
    temp_fn <- tempfile(fileext = x_ext)
    cat(temp_fn, "\n")
    on.exit(unlink(temp_fn))

    ## Download to a temp file
    dl_success <- download.file(x, destfile=temp_fn, mode="wb")
    if (dl_success != 0) stop("Sorry, the download was not succesful. Please check the URL and your internet connection, and try again.")
    if (!quiet) message("Zip download successful")

    ## Unzip it to destdir
    unzip(temp_fn, exdir = destdir, overwrite = overwrite)

    ## Return the list of files in the zip archive
    invisible(file.path(destdir, unzip(temp_fn, list=TRUE)$Name))

  } else {
    dest_fn <- file.path(destdir, basename(x))
    if (file.exists(dest_fn) && !overwrite) stop("File already exists. To download it again, set `overwrite=TRUE`.")
    dl_success <- download.file(x, destfile=dest_fn, mode="wb")
    if (dl_success != 0) stop("Sorry, the download was not successful. Please check the URL and your internet connection, and try again.")
    if (!quiet) message("Success!")
    invisible(dest_fn)
  }

}
