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
#' @importFrom crayon yellow green
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
    temp_fn <- tempfile(fileext = paste0(".", x_ext))
    on.exit(unlink(temp_fn))

    ## Download to a temp file
    dl_success <- download.file(x, destfile=temp_fn, mode="wb", quiet = quiet)
    if (dl_success != 0) stop("Download failed. Please check the URL and your internet connection.")
    if (!quiet) message(crayon::green("Zip file downloaded\nUnzipping..."))

    ## Compute the file paths when they'll be unzipped
    files_in_zip <- file.path(destdir, unzip(temp_fn, list=TRUE)$Name)

    ## Compute the files that will actually be unzipped
    if (overwrite) {
      files2unzip <- files_in_zip
    } else {
      files2unzip <- files_in_zip[!file.exists(files_in_zip)]
    }

    ## Unzip to destdir
    if (length(files2unzip) > 0 ) {
      unzip(temp_fn, exdir = destdir, overwrite = overwrite)
      if (!quiet) message(crayon::green(paste("  ", files2unzip, collapse = " \n", sep = "")))
    } else {
      if (!quiet) message(crayon::yellow("  all file(s) already exist, nothing overwritten"))
    }

    ## Return the list of files in the zip archive
    invisible(files_in_zip)

  } else {
    ## Not a zip or not auto_unzip
    dest_fn <- file.path(destdir, basename(x))

    if (file.exists(dest_fn) && !overwrite) {
      if (!quiet) message(yellow("File already exists. To download it again, set `overwrite=TRUE`."))
      invisible(dest_fn)

    } else {
      dl_success <- download.file(x, destfile=dest_fn, mode="wb", quiet = quiet)
      if (dl_success != 0) stop("Download failed. Please check the URL and your internet connection.")
      if (!quiet) {
        message(green("File(s) downloaded:"))
        message(green(paste("  ", dest_fn, collapse = "\n")))
      }
      invisible(dest_fn)
    }

  }

}
