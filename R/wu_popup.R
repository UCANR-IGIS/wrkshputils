#' Plot a png or jpg image file
#'
#' @param x A filename or URL of a PNG or JPG image
#' @param height The (minimum) height of the viewer pane
#'
#' @details This will display a PNG or JPG image in the RStudio viewer pane.
#' \code{x} can be either a local file or a URL. This gives you the ability to
#' create 'popup' slides in a R script that you work through during a lesson.
#'
#' \code{height} determines the minimum height in screen pixels that the viewer pane will
#' resize itself to (if needed). Set to \code{NULL} to disable.
#'
#' @examples
#' \dontrun{
#' wu_popup("http://placekitten.com/800/600")
#' }
#'
#' @importFrom utils browseURL
#' @importFrom tools file_ext
#' @export

wu_popup <- function(x, height = 360) {

  ## Identify the extension of the URL
  img_ext <- file_ext(x)
  if (!tolower(img_ext) %in% c("png", "jpg", "jpeg", "svg")) stop("Supported image formats include png, jpg, and svg files.")

  if (grepl("^http", x, ignore.case = TRUE)) {
    ## This is a URL
    img_url <- x

  } else {
    ## This is a local image file
    if (!file.exists(x)) stop(paste0("Can't find ", x))

    ## Create a file name in the temp dir
    img_tmp_fn <- tempfile(fileext = paste0(".", img_ext))

    ## Copy the file over to the temp dir (will be deleted when the R session closes)
    file.copy(x, img_tmp_fn)

    ## Save the basename as the 'URL' that will get inserted into the HTML template
    img_url <- basename(img_tmp_fn)

  }

  ## Get the viewer function if we're in RStudio, else browseURL
  viewer <- getOption("viewer", default = NA)

  ## Get the popup.html template
  popup_templt_fn <-system.file("popup.html", package="wrkshputils")
  popup_orig_chr <- readLines(popup_templt_fn)

  ## Swap in the URL
  popup_swpd_chr <- gsub("<img-url-here>", img_url, popup_orig_chr, ignore.case = TRUE)

  ## Save the swapped HTML file to disk
  popup_tmp_fn <- tempfile(fileext = ".html")
  cat(popup_swpd_chr, file = popup_tmp_fn)

  ## Open in the viewer
  if (identical(viewer, NA)) {
    utils::browseURL(popup_tmp_fn)
    ## Alternately we could use plot(magick::image_read(img_path_fn))
  } else {
    viewer(popup_tmp_fn, height = height)
  }

}
