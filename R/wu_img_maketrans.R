#' Make PNG images transparent backgrounds
#'
#' @param img_fn PNG filename
#' @param img_dir Image directory
#' @param pattern Pattern to match for input images
#' @param ext_out Output extension
#'
#' @details This will take a directory of PNG files and create versions where the color white is transparent.
#' This can be useful if you want to use the images as a build.
#'
#' You must pass
#'
#' @importFrom magick image_read image_write image_transparent
#' @export

wu_img_maketrans <- function(img_fn = NULL, img_dir = NULL, pattern = ".png$", ext_out= "_bgtrns.png") {

  ## This will take a folder of PNG files (i.e., exported from PowerPoint) and save
  ## versions of them with transparent background

  if (!is.null(img_fn) && !is.null(img_dir)) stop("Pass either img_fn or img_dir, not both.")

  if (!is.null(img_fn)) {
    if (!file.exists(img_fn)) stop(paste0(img_fn, " not found"))
    img_in <- image_read(img_fn)
    img_out <- paste0(tools::file_path_sans_ext(img_fn), ext_out)
    image_write(img_in %>% image_transparent('white'),
                path = img_out, format = "png")
    invisible(img_out)

  } else if (!is.null(img_dir))  {
    img_in_fns <- list.files(img_dir,
                             pattern = pattern,
                             full.names = TRUE,
                             ignore.case = TRUE)

    if (length(img_in_fns) == 0) stop("No images found that match the pattern")

    img_out_fns <- gsub(".png$", ext_out, img_in_fns, ignore.case = TRUE)

    for (i in 1:length(img_in_fns)) {
      img_in <- image_read(img_in_fns[i])
      image_write(img_in %>% image_transparent('white'),
                  path = img_out_fns[i],
                  format = "png")
    }
    invisible(img_out_fns)

  } else {

    stop("Input directory not found")
  }

}
